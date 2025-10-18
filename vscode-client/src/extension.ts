import * as vscode from 'vscode';
import * as path from 'path';

import { LanguageClient, LanguageClientOptions, ServerOptions } from 'vscode-languageclient/node';
import { CompletionList } from 'vscode';

let client: LanguageClient;

// dpl files have sections that are indentation based.
// to determine if user is inside of a javascript function section that looks like
//```
//function javascript(func_name)
//  print("in javascriptn")
//```
// simply iterate backwards until a line is found without leading whitespace
// and check if the section header is "function" and if it contains "javascript"
// if in a javascript function section returns a number that is the line
// the javascript function starts at, -1 if not found
function isInJavascriptFunctionSection(text: string, line: number): number {
  const lines = text.split('\n');
  for (let i = line; i >= 0; i -= 1) {
    const lineText = lines.at(i);
    const firstChar = lineText?.charAt(0);
    if (firstChar != ' ' && firstChar != '\t') {
      // found a section
      // return here true if its a javascript function section
      // false otherwise
      const isJavascriptFunction = lineText?.startsWith("function") && lineText.includes("javascript(");
      if (isJavascriptFunction ?? false) {
        return i;
      }
      return -1;
    }
  }
  return -1;
}

// given the line that the javascript function section starts at, return a string
// containing the javascript body
function extractJavascriptFunctionSection(text: string, line: number): string {
  const lines = text.split('\n');
  const javascriptLines = [];
  let num_whitespace_chars = 0;
  for (let i = 0; i < lines.length; i += 1) {
    if (i <= line) {
      javascriptLines.push('');
      continue
    }
    const lineText = lines.at(i);
    if (lineText === undefined) {
      break
    }
    if (lineText.length === 0) {
      break
    }
    if (num_whitespace_chars === 0) {
      for (let i = 0; i < lineText.length; i += 1) {
          const c = lineText.charAt(i);
          if (c == ' ' || c == '\t') {
            num_whitespace_chars += 1;
            continue
          }
          break
        }
    }
    javascriptLines.push(lineText);
  }
  return javascriptLines.join("\n")
}

export function activate(context: vscode.ExtensionContext) {
  console.log(context.extension.extensionPath);
  const serverExe = process.platform === 'win32' ? 'language_server.exe' : 'language_server';	
  const serverPath = context.asAbsolutePath(path.join('out', serverExe));
  const serverOptions: ServerOptions = {
    command: serverPath,
    args: [],
  };

  const virtualDocumentContents = new Map<string, string>();

  vscode.workspace.registerTextDocumentContentProvider('embedded-content', {
		provideTextDocumentContent: uri => {
			const originalUri = uri.path.slice(1).slice(0, -3);
			const decodedUri = decodeURIComponent(originalUri);
			const out = virtualDocumentContents.get(decodedUri);
      return out;
		}
	});
  const clientOptions: LanguageClientOptions = {
    documentSelector: [{ scheme: 'file', language: 'dpl' }],
    middleware: {
      provideCompletionItem: async (document, position, context, token, next) => {
        const text = document.getText();
        const javascriptSectionStartsAt = isInJavascriptFunctionSection(text, position.line);
        if (javascriptSectionStartsAt < 0) {
          // route to the language server as normal
          return await next(document, position, context, token);
        }
        const javascriptText = extractJavascriptFunctionSection(text, javascriptSectionStartsAt);
        // if here: we're in a javascript function section, so make a forwarded request
        // to the typescript language server
        // (we use typescript instead of javascript because we will benefit from extra type information
        // such as the shape of input/output that javascript language server wouldnt be able to leverage)
        const originalUri = document.uri.toString(true);
				virtualDocumentContents.set(originalUri, javascriptText);
				const vdocUriString = `embedded-content://typescript/${encodeURIComponent(
					originalUri
				)}.ts`;
				const vdocUri = vscode.Uri.parse(vdocUriString);
				const stuff = position;
        const res = await vscode.commands.executeCommand<CompletionList>(
					'vscode.executeCompletionItemProvider',
					vdocUri,
					stuff,
					context.triggerCharacter,
				);
        return res;
      }
    }
  };

  client = new LanguageClient('dplServer', 'DPL Language Server', serverOptions, clientOptions);
  client.onNotification('custom/log', (logMessage) => {
    console.log(logMessage);
  });
  client.start();
}

export function deactivate(): Thenable<void> | undefined {
  return client?.stop();
}

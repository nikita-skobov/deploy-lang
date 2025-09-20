import * as vscode from 'vscode';
import * as path from 'path';

import { LanguageClient, LanguageClientOptions, ServerOptions } from 'vscode-languageclient/node';

let client: LanguageClient;

export function activate(context: vscode.ExtensionContext) {
  console.log(context.extension.extensionPath);
  const serverExe = process.platform === 'win32' ? 'language_server.exe' : 'language_server';	
  const serverPath = context.asAbsolutePath(path.join('out', serverExe));
  const serverOptions: ServerOptions = {
    command: serverPath,
    args: [],
  };

  const clientOptions: LanguageClientOptions = {
    documentSelector: [{ scheme: 'file', language: 'dcl' }],
  };

  client = new LanguageClient('dclServer', 'DCL Language Server', serverOptions, clientOptions);
  client.onNotification('custom/log', (logMessage) => {
    console.log(logMessage);
  });
  client.start();
}

export function deactivate(): Thenable<void> | undefined {
  return client?.stop();
}

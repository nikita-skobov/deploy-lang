use str_at_line::{StrAtLine, StringAtLine};

use crate::{parse::{Section, SpannedDiagnostic}, DclFile};

pub const SECTION_TYPE: &str = "template";

#[derive(Debug, PartialEq, Default, Clone)]
pub struct TemplateSection {
    /// a unique identifier for this template
    pub template_name: StringAtLine,
    /// a template must have a create subsection. it is the only required
    /// subsection
    pub create: Transition,
    /// a template can have multiple update subsections.
    /// if a template does not have any update subsections,
    /// this will be empty
    pub update: Vec<Transition>,
    /// a delete subsection is optional, not all templates will provide it
    pub delete: Option<Transition>,

    /// internal field to know if create was already set or not. defaults to false
    create_was_set: bool,
}

#[derive(Debug, PartialEq, Default, Clone)]
pub struct Transition {
    pub before_directives: Vec<Directive>,
    pub after_directives: Vec<Directive>,
    pub cli_commands: Vec<CliCommand>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct CliCommand {
    /// name of the command to be ran
    pub command: StringAtLine,
    /// the first args that are always to be added to the command
    /// these should be defined statically in the dcl file
    pub prefix_args: Vec<StringAtLine>,
    pub arg_transforms: Vec<ArgTransform>,
}

/// a cli command can have a list of arg transforms
/// these are processed in order, and manipulate an arg map prior to
/// passing the transformed args to the actual command
#[derive(Debug, PartialEq, Clone)]
pub enum ArgTransform {
    /// represented in .dcl file by "... $.query"
    /// this transform takes the provided json path query
    /// and expands the value found at that path query
    /// from the resource object into the arg set, merging the existing
    /// arg set with the key/value pairs found from the looked up object
    /// this is usually used by "... $.input" where the resource object's input
    /// is a json object. destructuring only works for objects, and is an error
    /// if it is applied to non-objects.
    Destructure(jsonpath_rust::parser::model::JpQuery),
    /// represented in .dcl file by "! field-name"
    /// this transformation removes the field from the arg set, if present
    /// if the arg set does not have the field, it is a no-op
    Remove(StringAtLine),
    /// represented in .dcl file by "field-name $.query"
    /// this transform takes the provided json path query
    /// evaluates it from the resource object, and sets the result value to the field
    /// of the given name
    Add(StringAtLine, jsonpath_rust::parser::model::JpQuery),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Directive {

}

pub fn parse_template_section<'a>(dcl: &mut DclFile, section: &Section<'a>) -> Result<(), SpannedDiagnostic> {
    let template_name = section.parameters
        .ok_or("must have a template name")
        .map_err(|e| {
            let diag = SpannedDiagnostic::new(e.to_string(), section.start_line, SECTION_TYPE.len());
            return diag
        })?;
    let template_name = template_name.trim();
    if template_name.s.is_empty() {
        return Err(SpannedDiagnostic::new("must have a template name".to_string(), section.start_line, SECTION_TYPE.len()));
    }
    let mut out = TemplateSection::default();
    out.template_name = template_name.to_owned();
    // parse the various sub-sections. each sub-section should have no indentation
    let mut body_iter: std::iter::Peekable<std::slice::Iter<'_, StrAtLine<'a>>> = section.body.iter().peekable();
    loop {
        let line = match body_iter.next() {
            Some(l) => l,
            None => break,
        };
        // TODO: allow directives on top of subsections
        if line.s.starts_with("create") {
            parse_lines_to_create_transition(&mut out, &line, &mut body_iter)?;
        } else {
            let line_index = line.line;
            let diag = SpannedDiagnostic::new(
                format!("expected transition section 'create' instead found '{}'", line), line_index, 999);
            return Err(diag);
        }
    }
    dcl.templates.push(out);

    Ok(())
}

pub fn parse_lines_to_create_transition<'a>(
    template_section: &mut TemplateSection,
    current_line: &StrAtLine<'a>,
    lines: &mut std::iter::Peekable<std::slice::Iter<'_, StrAtLine<'a>>>,
) -> Result<(), SpannedDiagnostic> {
    // can only have 1 create subsection:
    if template_section.create_was_set {
        let line_index = current_line.line;
        let diag = SpannedDiagnostic::new(format!("templates cannot have multiple create subsections"), line_index, 999);
        return Err(diag);
    }
    // TODO: support options next to "create" keyword
    let first_command = parse_command(lines)?;
    // must have at least 1 command
    let first_command = match first_command {
        Some(c) => c,
        None => {
            return Err(SpannedDiagnostic::new(format!("unexpected end of transition. must have at least 1 command"), current_line.line, 999));
        }
    };
    let mut commands = vec![first_command];
    // keep parsing commands that are part of this transition:
    while let Some(command) = parse_command(lines)? {
        commands.push(command);
    }
    template_section.create = Transition {
        before_directives: vec![],
        after_directives: vec![],
        cli_commands: commands,
    };
    template_section.create_was_set = true;
    Ok(())
}

/// returns a string representing the whitespace sequence of the line prior to the first non-whitespace character
fn get_prefix(s: &str) -> String {
    let mut out = String::with_capacity(8);
    for char in s.chars() {
        if char.is_ascii_whitespace() {
            out.push(char);
        } else {
            break
        }
    }
    out
}

pub fn parse_command<'a>(
    lines: &mut std::iter::Peekable<std::slice::Iter<'_, StrAtLine<'a>>>,
) -> Result<Option<CliCommand>, SpannedDiagnostic> {
    let (command_line, indent_prefix) = match lines.peek() {
        Some(l) => {
            // check if its an empty line with whitespace:
            if l.s.chars().all(|c| c.is_ascii_whitespace()) {
                return Ok(None);
            }
            // test that its a command line: it should have an indent:
            let indent_prefix = get_prefix(l.s);
            if indent_prefix.is_empty() {
                return Ok(None)
            }
            // its the command line: take it off the iterator:
            let out = l.to_owned();
            let _ = lines.next();
            (out, indent_prefix)
        },
        None => {
            return Ok(None);
        }
    };
    let command_line = command_line.trim();
    let (command, prefix_args) = match command_line.split_once(' ') {
        Some((l, r)) => (l, r),
        None => (command_line, Default::default()),
    };
    let command = command.trim().to_owned();
    let prefix_args: Vec<_> = prefix_args.split_ascii_whitespace().map(|x| x.to_owned()).collect();
    let mut arg_transforms = vec![];
    loop {
        // keep peeking the next line, if its
        // indented more than indent_prefix, its args for this command:
        let arg_transform_line = match lines.peek() {
            Some(l) => {
                let current_prefix = get_prefix(l.s);
                if current_prefix.len() > indent_prefix.len() {
                    // take it off the iterator:
                    let out = l.trim();
                    let _ = lines.next();
                    out
                } else {
                    break;
                }
            },
            None => break,
        };
        
        match arg_transform_line.split_once(' ') {
            Some((left, right)) => {
                match left.s {
                    "..." => {
                        let path_query = jsonpath_rust::parser::parse_json_path(right.s)
                            .map_err(|e| {
                                SpannedDiagnostic::from_str_at_line(right, format!("failed to parse json path query ('{}') of destructure arg transform: {:?}", right, e))
                            })?;
                        arg_transforms.push(ArgTransform::Destructure(path_query));
                    }
                    "!" => {
                        arg_transforms.push(ArgTransform::Remove(right.trim().to_owned()));
                    }
                    _field_name => {
                        let path_query = jsonpath_rust::parser::parse_json_path(right.s)
                            .map_err(|e| {
                                SpannedDiagnostic::from_str_at_line(right, format!("failed to parse json path query ('{}') of add arg transform: {:?}", right, e))
                            })?;
                        arg_transforms.push(ArgTransform::Add(left.trim().to_owned(), path_query));
                    }
                }
            }
            None => {
                let line_index = arg_transform_line.line;
                return Err(SpannedDiagnostic::new(format!("invalid arg transform. must start with '...' '!' or a field name, instead found '{}'", arg_transform_line), line_index, 999));
            }
        }
    }

    Ok(Some(CliCommand { command, prefix_args, arg_transforms }))
}

#[cfg(test)]
mod test {
    use crate::parse::{parse_document_to_sections, sections_to_dcl_file};

    #[test]
    fn should_error_on_invalid_arg_transform() {
        let document = r#"
template something
  create
    echo
      ... mypath
        "#;
        let mut sections = parse_document_to_sections(document);
        let valid_sections: Vec<_> = sections.drain(..).map(|x| x.unwrap()).collect();
        let err = sections_to_dcl_file(&valid_sections).expect_err("it should err");
        assert!(err.message.starts_with("failed to parse json path query ('mypath')"), "it was {}", err.message);
        assert_eq!(err.span.start.line, 4);
        assert_eq!(err.span.start.column, 10);
        assert_eq!(err.span.end.column, 16);
        let document = r#"
template something
  create
    echo
      my-field not-a-path
        "#;
        let mut sections = parse_document_to_sections(document);
        let valid_sections: Vec<_> = sections.drain(..).map(|x| x.unwrap()).collect();
        let err = sections_to_dcl_file(&valid_sections).expect_err("it should err");
        assert!(err.message.starts_with("failed to parse json path query ('not-a-path')"), "it was {}", err.message);
        assert_eq!(err.span.start.line, 4);
        assert_eq!(err.span.start.column, 15);
        assert_eq!(err.span.end.column, 25);
    }

    #[test]
    fn can_parse_multiple_arg_transforms() {
        let document = r#"
template something
  create
    echo
      ... $.input.mypath
      ! some-field
      other-field $.input.thing
"#;
        let mut sections = parse_document_to_sections(document);
        let valid_sections: Vec<_> = sections.drain(..).map(|x| x.unwrap()).collect();
        let dcl = sections_to_dcl_file(&valid_sections).expect("it should not err");
        assert_eq!(dcl.templates[0].create.cli_commands[0].arg_transforms.len(), 3);
    }

    #[test]
    fn can_parse_multiple_commands_in_a_transition() {
        let document = r#"
template something
  create
    echo
    cat
    ls
"#;
        let mut sections = parse_document_to_sections(document);
        let valid_sections: Vec<_> = sections.drain(..).map(|x| x.unwrap()).collect();
        let dcl = sections_to_dcl_file(&valid_sections).expect("it should not err");
        assert_eq!(dcl.templates[0].create.cli_commands.len(), 3);
    }

    #[test]
    fn can_parse_multiple_commands_with_multiple_arg_transforms_each() {
        let document = r#"
template something
  create
    echo
       ... $.input
       ! some-field
    cat
       ! this
       ! that
    ls
       this $.input.this
       that $.input.that
"#;
        let mut sections = parse_document_to_sections(document);
        let valid_sections: Vec<_> = sections.drain(..).map(|x| x.unwrap()).collect();
        let dcl = sections_to_dcl_file(&valid_sections).expect("it should not err");
        assert_eq!(dcl.templates[0].create.cli_commands.len(), 3);
        assert_eq!(dcl.templates[0].create.cli_commands[0].arg_transforms.len(), 2);
        assert_eq!(dcl.templates[0].create.cli_commands[1].arg_transforms.len(), 2);
        assert_eq!(dcl.templates[0].create.cli_commands[2].arg_transforms.len(), 2);
    }
}

/*

template aws_iam_policy
  create
    aws iam create-policy
      ... $.input
  update
    # adds the value of PolicyVersion.VersionId to the accumulator
    @after add-output $.Policy.DefaultVersionId $.accum.PolicyVersion.VersionId
    aws iam create-policy-version --set-as-default
      policy-arn $.output.Policy.Arn
      policy-document $.input.policy-document
    @after drop-output
    aws iam delete-policy-version
      policy-arn $.output.Policy.Arn
      version-id $.output.DefaultVersionId


*/

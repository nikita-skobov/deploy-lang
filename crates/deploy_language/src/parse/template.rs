use enumdoc::enumdoc_derive::Enumdoc;
use json_with_positions::{CharIterator, Value};
use str_at_line::{StrAtLine, StringAtLine};

use crate::{parse::{Section, SpannedDiagnostic}, DplFile};

pub const SECTION_TYPE: &str = "template";

#[derive(Debug, PartialEq, Default, Clone)]
pub struct TemplateSection {
    /// a unique identifier for this template
    pub template_name: StringAtLine,
    /// a template must have a create subsection. it is the only required
    /// subsection
    pub create: Transition,
    /// a template can have optionally have an update section.
    /// if a template does not have an update section
    /// this will be empty
    pub update: Option<Transition>,
    /// a delete subsection is optional, not all templates will provide it
    pub delete: Option<Transition>,

    /// internal field to know if create was already set or not. defaults to false
    create_was_set: bool,
}

#[derive(Debug, PartialEq, Default, Clone)]
pub struct Transition {
    /// a transition can have directives itself, in addition
    /// to directives per each command
    pub directives: Vec<Directive>,
    pub cli_commands: Vec<CliCommandWithDirectives>,
}

#[derive(Debug, PartialEq, Clone, Default)]
pub struct CliCommandWithDirectives {
    pub directives: Vec<Directive>,
    pub cmd: CmdOrBuiltin,
}

#[derive(Debug, PartialEq, Clone)]
pub enum CmdOrBuiltin {
    Command(CliCommand),
    Builtin(Builtin),
}

impl CmdOrBuiltin {
    #[cfg(debug_assertions)]
    pub fn as_command(&self) -> &CliCommand {
        match self {
            CmdOrBuiltin::Command(cli_command) => cli_command,
            CmdOrBuiltin::Builtin(_) => panic!("it was a builtin but caller assumed it was a cli command")
        }
    }
    #[cfg(debug_assertions)]
    pub fn as_builtin(&self) -> &Builtin {
        match self {
            CmdOrBuiltin::Command(_) => panic!("it was a command but caller assumed it was a builtin"),
            CmdOrBuiltin::Builtin(builtin) => builtin,
        }
    }
}

impl Default for CmdOrBuiltin {
    fn default() -> Self {
        Self::Command(Default::default())
    }
}

impl From<CliCommand> for CmdOrBuiltin {
    fn from(value: CliCommand) -> Self {
        Self::Command(value)
    }
}

impl From<Builtin> for CmdOrBuiltin {
    fn from(value: Builtin) -> Self {
        Self::Builtin(value)
    }
}

/// builtins are commands that deploy-lang can run without a shell, these are built into the deploy-lang interpreter
/// unlike CliCommands, builtin types are known ahead of time, and are parsed more strictly. CliCommands allow for arbitrary execution,
/// whereas builtins operate only on json values in memory
#[derive(Debug, PartialEq, Clone)]
pub enum Builtin {
    Strcat { kw: StringAtLine, values: Vec<Value> }
}

#[derive(Debug, PartialEq, Clone, Default)]
pub struct CliCommand {
    /// name of the command to be ran
    pub command: StringAtLine,
    /// the first args that are always to be added to the command
    /// these should be defined statically in the dpl file
    pub prefix_args: Vec<StringAtLine>,
    pub arg_transforms: Vec<ArgTransform>,
}

/// a cli command can have a list of arg transforms
/// these are processed in order, and manipulate an arg map prior to
/// passing the transformed args to the actual command
#[derive(Debug, PartialEq, Clone)]
pub enum ArgTransform {
    /// represented in .dpl file by "... $.query"
    /// this transform takes the provided json path query
    /// and expands the value found at that path query
    /// from the resource object into the arg set, merging the existing
    /// arg set with the key/value pairs found from the looked up object
    /// this is usually used by "... $.input" where the resource object's input
    /// is a json object. destructuring only works for objects, and is an error
    /// if it is applied to non-objects.
    Destructure(jsonpath_rust::parser::model::JpQuery),
    /// represented in .dpl file by "! field-name"
    /// this transformation removes the field from the arg set, if present
    /// if the arg set does not have the field, it is a no-op
    Remove(StringAtLine),
    /// represented in .dpl file by "field-name $.query"
    /// this transform takes the provided json path query
    /// evaluates it from the resource object, and sets the result value to the field
    /// of the given name
    Add(StringAtLine, jsonpath_rust::parser::model::JpQuery),
}

#[derive(Enumdoc, Debug, PartialEq, Clone)]
/// a directive consists of an @ symbol
/// a keyword "kw" in all variants that is used to differentiate the type of directive.
/// and is followed by args which are specific to each directive.
/// the "kw" and the @ symbol is the only mandatory part of a directive
/// and the kw is used to get the string span of the directive.
/// a directive must be on a single line, and therefore if we wish to show
/// diagnostics on an invalid directive we can infer the line from the "kw"
#[non_exhaustive]
pub enum Directive {
    /// only relevant to update commands: a diff directive
    /// requires that the value the query resolves to must be different
    /// than the same value from the last time this resource was transitioned.
    /// authors can specify multiple queries in one directives optionally by placing them in a json array
    /// such as `[$.a, $.b]`, which are ANDed together.
    /// multiple diff and same directives are ORed together
    /// eg if an author specifies 
    /// 
    /// ```text
    /// @diff [$.a, $.b]
    /// @diff $.c
    /// ```
    /// then the command below this directive will be ran if the value of $.c is different from prev and current
    /// OR if the value of $.a AND the value of $.b is different from prev and current
    Diff { kw: StringAtLine, query: Vec<jsonpath_rust::parser::model::JpQuery> },
    /// only relevant to update commands: a same directitve
    /// requires that the value the query resolves to must be the same
    /// as the value from the last time this resource was transitioned.
    /// authors can specify multiple quries, which are ANDed together.
    /// multiple diff and same directives are ORed together
    /// eg if an author specifies
    /// 
    /// ```text
    /// @same [$.a, $.b]
    /// @same $.c
    /// ```
    /// 
    /// then the command below this directive will be ran if the value of $.c is the same in prev and current
    /// OR if the value of $.a AND the value of $.b is the same in prev and current
    Same { kw: StringAtLine, query: Vec<jsonpath_rust::parser::model::JpQuery> },

    /// causes the command's output to be ignored entirely and not merged into the accumulator.
    /// this directive doesnt take any args. it should just be the directive keyword eg:
    /// ```text
    /// @dropoutput
    /// ```
    DropOutput { kw: StringAtLine },


    /// insert a value into the accumulator after this command runs. the accum_path can leave out the $.accum since it's implied
    /// the value is going into the accumulator. the src_path can be a json path starting with $.input $.name $.accum or $.output.
    /// the syntax is a json array with 2 values. the first is the src_path of where to read from
    /// and the 2nd is the accum_path which is where the value will be inserted to
    /// eg
    /// ```text
    /// @accum [$.accum.somefield, $.nested.fieldvalue]
    /// ```
    /// the above example would run the command, merge into the accumulator,
    /// then after it merges, it reads the value of the accumulator "somefield"
    /// and it inserts that value again into the accumulator but at a different path "nested.fieldvalue"
    Accum { kw: StringAtLine, src_path: jsonpath_rust::parser::model::JpQuery, accum_path: jsonpath_rust::parser::model::JpQuery },

    // TODO: support a diff same directive that ands together same and diff conditions
    // as otherwise there's no way to support running a command with multiple same/diff conditions
    // anded together. at the root level they are ORed together
    // /// only relevant to update commands: a combined same and diff directive
    // /// where all conditions are ANDed together. the syntax is an object with two keys
    // /// "same" and "diff". the value of each key must be a json path query, or an array of json path queries
    // /// eg if an author specifies
    // /// ```text
    // /// @samediff {"same": [$.a, $.b], "diff": $.c}
    // /// ```
    // /// 
    // /// then the command below this directive will be ran if the value of $.a AND $.b are the same in prev and current
    // /// AND the value of $.c is different in prev than current
    // SameDiff { kw: StringAtLine, same: Vec<jsonpath_rust::parser::model::JpQuery>, diff: Vec<jsonpath_rust::parser::model::JpQuery> },
}

pub fn parse_template_section<'a>(dpl: &mut DplFile, section: &Section<'a>) -> Result<(), SpannedDiagnostic> {
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
            parse_lines_as_transition(&mut out, &line, &mut body_iter, "create")?;
        } else if line.s.starts_with("update") {
            parse_lines_as_transition(&mut out, &line, &mut body_iter, "update")?;
        } else if line.s.starts_with("delete") {
            parse_lines_as_transition(&mut out, &line, &mut body_iter, "delete")?;
        } else {
            let line_index = line.line;
            let diag = SpannedDiagnostic::new(
                format!("expected transition section 'create' instead found '{}'", line), line_index, 999);
            return Err(diag);
        }
    }
    dpl.templates.push(out);

    Ok(())
}

pub fn parse_lines_as_transition<'a>(
    template_section: &mut TemplateSection,
    current_line: &StrAtLine<'a>,
    lines: &mut std::iter::Peekable<std::slice::Iter<'_, StrAtLine<'a>>>,
    transition_type: &str,
) -> Result<(), SpannedDiagnostic> {
    if transition_type == "create" && template_section.create_was_set {
        // can only have 1 create subsection:
        let line_index = current_line.line;
        let diag = SpannedDiagnostic::new(format!("templates cannot have multiple create subsections"), line_index, 999);
        return Err(diag);
    }
    if transition_type == "update" && template_section.update.is_some() {
        // can only have 1 update subsection:
        let line_index = current_line.line;
        let diag = SpannedDiagnostic::new(format!("templates cannot have multiple update subsections"), line_index, 999);
        return Err(diag);
    }
    if transition_type == "delete" && template_section.delete.is_some() {
        // can only have 1 update subsection:
        let line_index = current_line.line;
        let diag = SpannedDiagnostic::new(format!("templates cannot have multiple update subsections"), line_index, 999);
        return Err(diag);
    }
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
    match transition_type {
        "create" => {
            template_section.create = Transition {
                cli_commands: commands,
                ..Default::default()
            };
            template_section.create_was_set = true;
        }
        "update" => {
            template_section.update = Some(Transition {
                cli_commands: commands,
                ..Default::default()
            });
        }
        "delete" | _ => {
            template_section.delete = Some(Transition {
                cli_commands: commands,
                ..Default::default()
            });
        }
    }
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

pub fn parse_directive<'a>(
    line: StrAtLine<'a>
) -> Result<Directive, SpannedDiagnostic> {
    let (at_char, rest) = line.split_at_checked(1)
        .ok_or_else(|| SpannedDiagnostic::new("directive must start with '@' character", line.line, 999))?;
    if at_char.s != "@" {
        return Err(SpannedDiagnostic::new("directive must start with the '@' character", at_char.line, at_char.col + 1));
    }
    let (kw, rest) = match rest.split_once(" ") {
        Some((l, r)) => (l, r),
        None => (rest, StrAtLine { s: "", col: rest.col, line: rest.line }),
    };
    match kw.s {
        "diff" => {
            let query = parse_json_directive_query(rest.s)
                .map_err(|e| SpannedDiagnostic::from_str_at_line(rest, format!("failed to parse diff directive json path query: {:?}", e)))?;
            Ok(Directive::Diff { kw: kw.to_owned(), query })
        }
        "same" => {
            let query = parse_json_directive_query(rest.s)
                .map_err(|e| SpannedDiagnostic::from_str_at_line(rest, format!("failed to parse same directive json path query: {:?}", e)))?;
            Ok(Directive::Same { kw: kw.to_owned(), query })
        }
        "dropoutput" => {
            Ok(Directive::DropOutput { kw: kw.to_owned() })
        }
        "accum" => {
            let val = json_with_positions::parse_json_value(rest.s)
                .map_err(|e| SpannedDiagnostic::from_str_at_line(rest, format!("failed to parse accum directive as array of two json paths: {:?}", e)))?;
            let mut array = match val {
                json_with_positions::Value::Array { val, .. } => val,
                x => return Err(SpannedDiagnostic::from_str_at_line(rest, format!("accum directive must be followed by json array of two json paths. instead found {:?}", x)))
            };
            let second = array.pop();
            let first = array.pop();
            let (first, second) = match (first, second) {
                (Some(f), Some(s)) => (f, s),
                _ => return Err(SpannedDiagnostic::from_str_at_line(rest, format!("accum directive must be followed by a json array of two json paths")))
            };
            let first_path_str = match first {
                json_with_positions::Value::JsonPath { val, .. } => val,
                x => return Err(SpannedDiagnostic::from_str_at_line(rest, format!("first accum directive value must be a json path, instead found '{:?}'", x))),
            };
            let second_path_str = match second {
                json_with_positions::Value::JsonPath { val, .. } => val,
                x => return Err(SpannedDiagnostic::from_str_at_line(rest, format!("second accum directive value must be a json path, instead found '{:?}'", x))),
            };
            let src_path = jsonpath_rust::parser::parse_json_path(first_path_str.as_str())
                .map_err(|e| SpannedDiagnostic::from_str_at_line(rest, format!("failed to parse '{}' as json path query: {:?}", first_path_str.s, e)))?;
            let accum_path = jsonpath_rust::parser::parse_json_path(second_path_str.as_str())
                .map_err(|e| SpannedDiagnostic::from_str_at_line(rest, format!("failed to parse '{}' as json path query: {:?}", second_path_str.s, e)))?;
            Ok(Directive::Accum { kw: kw.to_owned(), src_path, accum_path })
        }
        unknown_kw => {
            Err(SpannedDiagnostic::from_str_at_line(kw, format!("unknown directive '{}'", unknown_kw)))
        }
    }
}

pub fn parse_json_directive_query(s: &str) -> Result<Vec<jsonpath_rust::parser::model::JpQuery>, String> {
    let val = json_with_positions::parse_json_value(s)?;
    let mut array_of_json_path_strs: Vec<json_with_positions::Value>;
    match val {
        json_with_positions::Value::JsonPath { pos, val, .. } => {
            array_of_json_path_strs = vec![json_with_positions::Value::JsonPath { pos, val }];
        }
        json_with_positions::Value::Array { val, .. } => {
            array_of_json_path_strs = val;
        }
        x => return Err(format!("expected directive to contain a json path or array of json paths, instead found '{:?}'", x)),
    }
    let mut out = Vec::with_capacity(array_of_json_path_strs.len());
    for (i, json_path_str) in array_of_json_path_strs.drain(..).enumerate() {
        if let json_with_positions::Value::JsonPath { val, .. } = &json_path_str {
            let jpq = jsonpath_rust::parser::parse_json_path(&val.s)
                .map_err(|e| format!("failed to parse '{}' as json path query: {:?}", val.s, e))?;
            out.push(jpq);
        } else {
            return Err(format!(
                "expected directive to contain a json path or array of json paths. value of array[{}] was '{:?}' which is not a json path",
                i,
                json_path_str
            ))
        }
    }

    Ok(out)
}

/// keeps peeking lines until a non-directive line is found
/// a directive line must:
/// 1. have whitespace indentation
/// 2. start with an @ symbol
/// as soon as a line is found that doesnt meet one of those conditions
/// then this returns what it's parsed so far
pub fn parse_all_directives<'a>(
    lines: &mut std::iter::Peekable<std::slice::Iter<'_, StrAtLine<'a>>>
) -> Result<Vec<Directive>, SpannedDiagnostic> {
    let mut out = vec![];
    loop {
        match lines.peek() {
            Some(l) => {
                let indent_prefix = get_prefix(l.s);
                if indent_prefix.is_empty() {
                    break;
                }
                let trimmed = l.trim();
                if !trimmed.s.starts_with('@') {
                    break;
                }
                let _ = lines.next();
                out.push(parse_directive(trimmed)?);
            }
            None => break
        }
    }
    Ok(out)
}

pub fn parse_builtin_command<'a>(
    command: StringAtLine,
    directives: Vec<Directive>,
    indent_prefix: String,
    current_line: StringAtLine,
    lines: &mut std::iter::Peekable<std::slice::Iter<'_, StrAtLine<'a>>>,
) -> Result<Option<CliCommandWithDirectives>, SpannedDiagnostic> {
    // first, collect all lines whose whitespace is longer than the command's leading whitespace.
    let mut builtin_arg_lines = vec![];
    if !current_line.as_str().is_empty() {
        builtin_arg_lines.push(current_line);
    }
    loop {
        match lines.peek() {
            Some(l) => {
                let current_prefix = get_prefix(l.s);
                if current_prefix.len() > indent_prefix.len() {
                    // take it off the iterator:
                    let out = l.trim();
                    let _ = lines.next();
                    builtin_arg_lines.push(out.to_owned());
                } else {
                    break;
                }   
            }
            None => break
        };
    }
    // now interpret the parsed lines depending on builtin type:
    let cmd = match command.as_str() {
        "/strcat" => {
            // parse as a json value, where it must be an array:
            let mut char_iter = CharIterator::new(
                builtin_arg_lines.iter().map(|x| x.as_borrowed())
            ).peekable();
            let json_arr = json_with_positions::parse_json_value_from_iter_no_recursion(&mut char_iter)
                .map_err(|e| SpannedDiagnostic::from_str_at_line(&command, format!("/strcat builtin failed to parse: {}", e)))?;
            let values = match json_arr {
                Value::Array { val, .. } => val,
                x => return Err(SpannedDiagnostic::from_str_at_line(&command, format!("/strcat builtin must be followed be a json array, instead found '{:?}'", x)))
            };
            CmdOrBuiltin::Builtin(Builtin::Strcat { kw: command, values })
        }
        x => return Err(SpannedDiagnostic::from_str_at_line(&command, format!("unknown builtin '{}'", x)))
    };
    Ok(Some(CliCommandWithDirectives {
        directives,
        cmd,
    }))
}

pub fn parse_command<'a>(
    lines: &mut std::iter::Peekable<std::slice::Iter<'_, StrAtLine<'a>>>,
) -> Result<Option<CliCommandWithDirectives>, SpannedDiagnostic> {
    let directives = parse_all_directives(lines)?;
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
    if command.s.starts_with("/") {
        return parse_builtin_command(command, directives, indent_prefix, prefix_args.to_owned(), lines);
    }
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

    let cmd = CliCommand { command, prefix_args, arg_transforms };
    Ok(Some(CliCommandWithDirectives { directives, cmd: cmd.into() }))
}

#[cfg(test)]
mod test {
    use crate::parse::{parse_document_to_sections, sections_to_dpl_file, template::{ArgTransform, Builtin, Directive}};
    use assert_matches::assert_matches;
    use json_with_positions::Value;

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
        let err = sections_to_dpl_file(&valid_sections).expect_err("it should err");
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
        let err = sections_to_dpl_file(&valid_sections).expect_err("it should err");
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
        let dpl = sections_to_dpl_file(&valid_sections).expect("it should not err");
        assert_eq!(dpl.templates[0].create.cli_commands[0].cmd.as_command().arg_transforms.len(), 3);
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
        let dpl = sections_to_dpl_file(&valid_sections).expect("it should not err");
        assert_eq!(dpl.templates[0].create.cli_commands.len(), 3);
    }

    #[test]
    fn can_parse_builtin_commands() {
        let document = r#"
template something
  create
    /strcat []
"#;
        let mut sections = parse_document_to_sections(document);
        let valid_sections: Vec<_> = sections.drain(..).map(|x| x.unwrap()).collect();
        let dpl = sections_to_dpl_file(&valid_sections).expect("it should not err");
        assert_eq!(dpl.templates[0].create.cli_commands.len(), 1);
        assert_matches!(dpl.templates[0].create.cli_commands[0].cmd.as_builtin(), Builtin::Strcat { values, .. } => {
            assert_eq!(values.len(), 0);
        });
    }

    #[test]
    fn can_parse_builtin_commands_multi_line() {
        let document = r#"
template something
  create
    /strcat [
     "a", "b", # comment here
     # comment
     $.input.something
     ]
"#;
        let mut sections = parse_document_to_sections(document);
        let valid_sections: Vec<_> = sections.drain(..).map(|x| x.unwrap()).collect();
        let dpl = sections_to_dpl_file(&valid_sections).expect("it should not err");
        assert_eq!(dpl.templates[0].create.cli_commands.len(), 1);
        assert_matches!(dpl.templates[0].create.cli_commands[0].cmd.as_builtin(), Builtin::Strcat { values, .. } => {
            assert_eq!(values.len(), 3);
            assert_eq!(&values[0].clone().to_serde_json_value(), &serde_json::json!("a"));
            assert_eq!(&values[1].clone().to_serde_json_value(), &serde_json::json!("b"));
            assert_matches!(&values[2], Value::JsonPath { val, .. } => {
                assert_eq!(val.as_str(), "$.input.something");
            });
        });
    }

    #[test]
    fn can_parse_multiple_builtin_commands_multi_line() {
        let document = r#"
template something
  create
    /strcat [
     "a", "b", # comment here
     # comment
     $.input.something
     ]
    # comment here
    /strcat []
    # comment here
    echo hello
      ... $.input
    /strcat ["a"]
"#;
        let mut sections = parse_document_to_sections(document);
        let valid_sections: Vec<_> = sections.drain(..).map(|x| x.unwrap()).collect();
        let dpl = sections_to_dpl_file(&valid_sections).expect("it should not err");
        assert_eq!(dpl.templates[0].create.cli_commands.len(), 4);
        assert_matches!(dpl.templates[0].create.cli_commands[0].cmd.as_builtin(), Builtin::Strcat { values, .. } => {
            assert_eq!(values.len(), 3);
            assert_eq!(&values[0].clone().to_serde_json_value(), &serde_json::json!("a"));
            assert_eq!(&values[1].clone().to_serde_json_value(), &serde_json::json!("b"));
            assert_matches!(&values[2], Value::JsonPath { val, .. } => {
                assert_eq!(val.as_str(), "$.input.something");
            });
        });
        assert_matches!(dpl.templates[0].create.cli_commands[1].cmd.as_builtin(), Builtin::Strcat { values, .. } => {
            assert_eq!(values.len(), 0);
        });
        assert_eq!(dpl.templates[0].create.cli_commands[2].cmd.as_command().command, "echo");
        assert_matches!(dpl.templates[0].create.cli_commands[3].cmd.as_builtin(), Builtin::Strcat { values, .. } => {
            assert_eq!(values.len(), 1);
        });
    }

    #[test]
    fn strcat_builtin_must_be_json_arr() {
        let document = r#"
template something
  create
    /strcat
      {}
"#;
        let mut sections = parse_document_to_sections(document);
        let valid_sections: Vec<_> = sections.drain(..).map(|x| x.unwrap()).collect();
        let diag = sections_to_dpl_file(&valid_sections).expect_err("it should err");
        assert!(diag.message.starts_with("/strcat builtin must be followed be a json array"));
    }

    #[test]
    fn can_parse_dropoutput_directive() {
        let document = r#"
template something
  create
    @dropoutput
    echo
"#;
        let mut sections = parse_document_to_sections(document);
        let valid_sections: Vec<_> = sections.drain(..).map(|x| x.unwrap()).collect();
        let dpl = sections_to_dpl_file(&valid_sections).expect("it should not err");
        assert_eq!(dpl.templates.len(), 1);
        assert_eq!(dpl.templates[0].create.cli_commands.len(), 1);
        assert_eq!(dpl.templates[0].create.cli_commands[0].directives.len(), 1);
        assert_matches!(&dpl.templates[0].create.cli_commands[0].directives[0], Directive::DropOutput { kw } => {
            assert_eq!(kw.s, "dropoutput");
            assert_eq!(kw.line, 3);
            assert_eq!(kw.col, 5);
        });
    }

    #[test]
    fn can_parse_accum_directive() {
        let document = r#"
template something
  create
    @accum [$.input.xyz, $.some.accum.value]
    echo
"#;
        let mut sections = parse_document_to_sections(document);
        let valid_sections: Vec<_> = sections.drain(..).map(|x| x.unwrap()).collect();
        let dpl = sections_to_dpl_file(&valid_sections).expect("it should not err");
        assert_eq!(dpl.templates.len(), 1);
        assert_eq!(dpl.templates[0].create.cli_commands.len(), 1);
        assert_eq!(dpl.templates[0].create.cli_commands[0].directives.len(), 1);
        assert_matches!(&dpl.templates[0].create.cli_commands[0].directives[0], Directive::Accum { kw, src_path, accum_path } => {
            assert_eq!(kw.s, "accum");
            assert_eq!(kw.line, 3);
            assert_eq!(kw.col, 5);
            assert_eq!(src_path.to_string(), "$inputxyz");
            assert_eq!(accum_path.to_string(), "$someaccumvalue");
        });
    }

    #[test]
    fn can_parse_with_comments() {
        let document = r#"
template xyz
  create
    # comment xyz
    echo hello
  # comment eeee
  update
    # comment abc
    echo
      # comment here
      abc $.output.someval
    echo
      # comment there
      xyz $.accum

resource xyz(resourceA)
    {}
"#;
        let mut dpl = crate::parse_and_validate(document).expect("it should not error");
        assert_eq!(dpl.resources.len(), 1);
        assert_eq!(dpl.templates.len(), 1);
        let mut template = dpl.templates.remove(0);
        assert_eq!(template.create.directives.len(), 0);
        assert_eq!(template.create.cli_commands.len(), 1);
        assert_eq!(template.create.cli_commands[0].cmd.as_command().command, "echo");
        let mut update = template.update.take().expect("it should have an update section");
        assert_eq!(update.directives.len(), 0);
        assert_eq!(update.cli_commands.len(), 2);
        let first = update.cli_commands.remove(0);
        assert_eq!(first.cmd.as_command().arg_transforms.len(), 1);
        assert_matches!(&first.cmd.as_command().arg_transforms[0], ArgTransform::Add(x, y) => {
            assert_eq!(x.s, "abc");
            assert_eq!(y.to_string(), "$outputsomeval");
        });
        let next = update.cli_commands.remove(0);
        assert_eq!(next.cmd.as_command().arg_transforms.len(), 1);
        assert_matches!(&next.cmd.as_command().arg_transforms[0], ArgTransform::Add(x, y) => {
            assert_eq!(x.s, "xyz");
            assert_eq!(y.to_string(), "$accum");
        });
    }

    #[test]
    fn can_parse_multiple_commands_each_with_directives() {
        let document = r#"
template something
  create
    @diff $
    echo
    @diff $.input
    cat
    @diff $.input.something[1]
    @same $.input.otherThing
    ls
"#;
        let mut sections = parse_document_to_sections(document);
        let valid_sections: Vec<_> = sections.drain(..).map(|x| x.unwrap()).collect();
        let mut dpl = sections_to_dpl_file(&valid_sections).expect("it should not err");
        assert_eq!(dpl.templates[0].create.cli_commands.len(), 3);
        let mut next = dpl.templates[0].create.cli_commands.remove(0);
        assert_eq!(next.directives.len(), 1);
        assert_matches!(next.directives.remove(0), Directive::Diff { kw, query } => {
            assert_eq!(kw.s, "diff");
            assert_eq!(query.len(), 1);
            assert_eq!(&query[0].to_string(), "$");
        });
        let mut next = dpl.templates[0].create.cli_commands.remove(0);
        assert_eq!(next.directives.len(), 1);
        assert_matches!(next.directives.remove(0), Directive::Diff { kw, query } => {
            assert_eq!(kw.s, "diff");
            assert_eq!(query.len(), 1);
            assert_eq!(&query[0].to_string(), "$input");
        });
        let mut next = dpl.templates[0].create.cli_commands.remove(0);
        assert_eq!(next.directives.len(), 2);
        assert_matches!(next.directives.remove(0), Directive::Diff { kw, query } => {
            assert_eq!(kw.s, "diff");
            // TODO: wrapper library for jsonpath_rust: its to_string impl omits segment delimiters...
            assert_eq!(query.len(), 1);
            assert_eq!(&query[0].to_string(), "$inputsomething1");
        });
        assert_matches!(next.directives.remove(0), Directive::Same { kw, query } => {
            assert_eq!(kw.s, "same");
            assert_eq!(query.len(), 1);
            assert_eq!(&query[0].to_string(), "$inputotherThing");
        });
        assert_eq!(next.cmd.as_command().command.s, "ls");
    }

    #[test]
    fn can_parse_multiple_jsonpath_queries_in_directive() {
        let document = r#"
template something
  create
    @diff [$.a, $]
    @same [$, $.a]
    echo
"#;
        let mut sections = parse_document_to_sections(document);
        let valid_sections: Vec<_> = sections.drain(..).map(|x| x.unwrap()).collect();
        let mut dpl = sections_to_dpl_file(&valid_sections).expect("it should not err");
        assert_eq!(dpl.templates[0].create.cli_commands.len(), 1);
        let mut next = dpl.templates[0].create.cli_commands.remove(0);
        assert_eq!(next.directives.len(), 2);
        assert_matches!(next.directives.remove(0), Directive::Diff { kw, query } => {
            assert_eq!(kw.s, "diff");
            assert_eq!(query.len(), 2);
            assert_eq!(&query[0].to_string(), "$a");
            assert_eq!(&query[1].to_string(), "$");
        });
        assert_matches!(next.directives.remove(0), Directive::Same { kw, query } => {
            assert_eq!(kw.s, "same");
            assert_eq!(query.len(), 2);
            assert_eq!(&query[0].to_string(), "$");
            assert_eq!(&query[1].to_string(), "$a");
        });
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
        let dpl = sections_to_dpl_file(&valid_sections).expect("it should not err");
        assert_eq!(dpl.templates[0].create.cli_commands.len(), 3);
        assert_eq!(dpl.templates[0].create.cli_commands[0].cmd.as_command().arg_transforms.len(), 2);
        assert_eq!(dpl.templates[0].create.cli_commands[1].cmd.as_command().arg_transforms.len(), 2);
        assert_eq!(dpl.templates[0].create.cli_commands[2].cmd.as_command().arg_transforms.len(), 2);
    }

    #[test]
    fn can_parse_multiple_subsection_types() {
        let document = r#"
template aws_lambda_function
    create
        aws lambda create-function
            ... $.input
    update
        @diff $.input.zipfile
        @same $.input.functionname
        aws lambda update-function-code
        @same $.input.zipfile
        @same $.input.functionname
        aws lambda update-function-configuration
            ... $.input
            ! zip-file
    delete
        blah blah
"#;
        let mut sections = parse_document_to_sections(document);
        let valid_sections: Vec<_> = sections.drain(..).map(|x| x.unwrap()).collect();
        let mut dpl = sections_to_dpl_file(&valid_sections).expect("it should not err");
        assert_eq!(1, dpl.templates.len());
        let template = dpl.templates.remove(0);
        assert_eq!(template.template_name.s, "aws_lambda_function");
        assert!(template.update.is_some());
        assert!(template.delete.is_some());
        assert_eq!(template.create.cli_commands.len(), 1);
        let mut update_section = template.update.unwrap();
        assert_eq!(update_section.cli_commands.len(), 2);
        let first_command = update_section.cli_commands.remove(0);
        assert_eq!(first_command.cmd.as_command().command.s, "aws");
        assert_eq!(first_command.cmd.as_command().prefix_args.join(" "), "lambda update-function-code");
        assert_matches!(&first_command.directives[0], Directive::Diff { query, .. } => {
            assert_eq!(query.len(), 1);
            assert_eq!(&query[0].to_string(), "$inputzipfile");
        });
        assert_matches!(&first_command.directives[1], Directive::Same { query, .. } => {
            assert_eq!(query.len(), 1);
            assert_eq!(&query[0].to_string(), "$inputfunctionname");
        });
        assert!(first_command.cmd.as_command().arg_transforms.is_empty());
        let second_command = update_section.cli_commands.remove(0);
        assert_eq!(second_command.cmd.as_command().command.s, "aws");
        assert_eq!(second_command.cmd.as_command().prefix_args.join(" "), "lambda update-function-configuration");
        assert_matches!(&second_command.directives[0], Directive::Same { query, .. } => {
            assert_eq!(query.len(), 1);
            assert_eq!(&query[0].to_string(), "$inputzipfile");
        });
        assert_matches!(&second_command.directives[1], Directive::Same { query, .. } => {
            assert_eq!(query.len(), 1);
            assert_eq!(&query[0].to_string(), "$inputfunctionname");
        });
        assert_eq!(second_command.cmd.as_command().arg_transforms.len(), 2);
        assert_matches!(&second_command.cmd.as_command().arg_transforms[0], ArgTransform::Destructure(d) => {
            assert_eq!(d.to_string(), "$input");
        });
        assert_matches!(&second_command.cmd.as_command().arg_transforms[1], ArgTransform::Remove(r) => {
            assert_eq!(r, "zip-file");
        });
    }
}

/*
template aws_iam_policy
  create
    aws iam create-policy
      ... $.input
  update
    # adds the value of PolicyVersion.VersionId to the accumulator
    @accum [$.Policy.DefaultVersionId, $.accum.PolicyVersion.VersionId]
    aws iam create-policy-version --set-as-default
      policy-arn $.output.Policy.Arn
      policy-document $.input.policy-document
    @after drop-output
    aws iam delete-policy-version
      policy-arn $.output.Policy.Arn
      version-id $.output.DefaultVersionId
*/

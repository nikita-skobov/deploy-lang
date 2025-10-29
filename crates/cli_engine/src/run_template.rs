use std::fmt::Display;

use deploy_language::parse::template::{ArgTransform, CliCommand, Directive, Transition};
use jsonpath_rust::{parser::model::{JpQuery, Segment}};
use log::Log;
use serde_json::Map;
use tokio::process::Command;

use crate::run_builtin;

pub type ArgSet = Vec<ArgInstruction>;

struct Gerund<'a> {
    pub verb: &'a str,
}

#[derive(Debug)]
pub enum ArgOptType {
    Long,
    Short,
    Verbatim,
}

#[derive(Debug)]
pub struct ArgInstruction {
    pub opt_type: ArgOptType,
    pub name: String,
    pub value: serde_json::Value,
}

impl<'a> Display for Gerund<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.verb)?;
        f.write_str("ing")
    }
}

fn transition_gerund<'a>(transition_type: &'a str) -> Gerund<'a> {
    let len = transition_type.len();
    Gerund {
        verb: transition_type.get(0..len-1).unwrap_or(transition_type)
    }
}

/// an ephemeral object to track state across command invocations
pub struct CommandState {
    /// represents the input as-is when the transition started.
    /// this value never changes across all commands
    pub input: serde_json::Value,
    /// represents the last output as-is from when the transition started.
    /// this value never changes during the course of a transition. its purpose is
    /// to hold the value of the resource's last output from the last time it ran.
    /// for create transitions this will be set as null
    pub output: serde_json::Value,
    /// represents the accumulated output across commands. after each command runs, its
    /// output is merged into this object, and after all commands run, the accumulator
    /// gets returned as the output for this transition.
    pub accum: serde_json::Value,
}

pub fn process_json_path(
    jpq: &JpQuery,
    val: &serde_json::Value
) -> Result<serde_json::Value, String> {
    let mut vals = jsonpath_rust::query::js_path_process(&jpq, val)
        .map_err(|e| format!("failed to process json path '{}': {:?}", jpq.to_string(), e))?;
    let mut vals: Vec<serde_json::Value> = vals.drain(..).map(|x| x.val().clone()).collect();
    // TODO: check for not found results.. jsonpath_rust will return an empty array if the lookup fails!
    let val = if vals.len() == 1 {
        vals.pop().unwrap()
    } else {
        serde_json::Value::Array(vals)
    };
    Ok(val)
}

impl CommandState {
    pub fn new(input: serde_json::Value, last_output: Option<serde_json::Value>) -> Self {
        let output = last_output.unwrap_or(serde_json::Value::Object(Default::default()));
        Self {
            input,
            output: output.clone(),
            // accumulator starts as the same as last output
            accum: output,
        }
    }

    pub fn evaluate_arg_transform(&self, name: &str, jpq: &JpQuery) -> Result<serde_json::Value, String> {
        // json path queries in arg transforms must start with either:
        // - $.input
        // - $.output
        // - $.accum
        // - $.name
        let mut cloned_segments = jpq.segments.clone();
        if cloned_segments.len() == 0 {
            return Err(format!("arg transform json path query for resource '{}' must have at least 1 segment", name));
        }
        let first_segment = cloned_segments.remove(0);
        let val = match first_segment {
            jsonpath_rust::parser::model::Segment::Selector(selector) => match selector {
                jsonpath_rust::parser::model::Selector::Name(x) => match x.as_str() {
                    "input" => &self.input,
                    "output" => &self.output,
                    "accum" => &self.accum,
                    "name" => return Ok(serde_json::Value::String(name.to_string())),
                    _ => return Err(format!("json path query '{}' for resource '{}' must start with $.input $.output $.accum or $.name", jpq, name)),
                }
                _ => return Err(format!("json path query '{}' for resource '{}' must start with $.input $.output $.accum or $.name", jpq, name))
            }
            _ => return Err(format!("json path query '{}' for resource '{}' must start with $.input $.output $.accum or $.name", jpq, name)),
        };
        // we removed the first segment which pointed at either input,output, or accum
        // and now we can evaluate the rest of the path query
        let jpq_eval = JpQuery { segments: cloned_segments };
        let val = process_json_path(&jpq_eval, val)?;
        Ok(val)
    }

    pub fn insert_accum(&mut self, name: &str, src_path: &JpQuery, accum_path: &JpQuery) -> Result<(), String> {
        // first, get the value referenced by src_path:
        let value = self.evaluate_arg_transform(name, src_path)?;
        // then recursively access values from the accum path segments,
        // creating empty objects if it references a path that doesnt exist yet
        let mut segments = accum_path.segments.clone();
        set_value_recursively(&mut self.accum, value, &mut segments)
    }

    pub fn insert(&mut self, val: &serde_json::Value, src_path: &Option<JpQuery>, dest_path: &JpQuery) -> Result<(), String> {
        // first, get the value referenced by the src path, or the entire value
        // if no src path was specified:
        let val = match src_path {
            Some(jpq) => {
                process_json_path(jpq, val)?
            }
            None => val.clone(),
        };
        // now insert that value into the dest path of the accumulator
        let mut segments = dest_path.segments.clone();
        set_value_recursively(&mut self.accum, val, &mut segments)
    }
}

pub fn set_value_recursively(
    current_val_ref: &mut serde_json::Value,
    set_val: serde_json::Value,
    remaining_selectors: &mut Vec<Segment>,
) -> Result<(), String> {
    if remaining_selectors.is_empty() {
        *current_val_ref = set_val;
        return Ok(());
    }
    let accessor = remaining_selectors.remove(0);
    match accessor {
        jsonpath_rust::parser::model::Segment::Selector(selector) => {
            match selector {
                jsonpath_rust::parser::model::Selector::Name(key) => set_value_at_key(key, current_val_ref, set_val, remaining_selectors),
                jsonpath_rust::parser::model::Selector::Index(i) => {
                    if i < 0 {
                        return Err(format!("invalid accum_path selector index '{}' indices must be positive", i))
                    }
                    set_value_at_index(i as usize, current_val_ref, set_val, remaining_selectors)
                },
                x => return Err(format!("accum_path selector can only be a name or an index, instead found '{}'", x)),
            }
        }
        x => return Err(format!("accum_path can only contain selectors, instead found '{}'", x)),
    }
}

pub fn set_value_at_key(
    key: String,
    current_val_ref: &mut serde_json::Value,
    set_val: serde_json::Value,
    remaining_selectors: &mut Vec<Segment>,   
) -> Result<(), String> {
    // if there's nothing at key, create it as an empty object
    if current_val_ref.get(&key).is_none() {
        if let Some(obj) = current_val_ref.as_object_mut() {
            obj.insert(key.clone(), serde_json::Value::Object(Map::new()));
        } else {
            return Err(format!("tried to access accumulator at $ ... '{}'. could not set that key as the value is not an object {:?}", key, current_val_ref))
        }
    }

    if let Some(obj) = current_val_ref.get_mut(&key) {
        set_value_recursively(obj, set_val, remaining_selectors)
    } else {
        return Err(format!("failed to access accumulator at $ ... '{}'", key))
    }
}

pub fn set_value_at_index(
    index: usize,
    current_val_ref: &mut serde_json::Value,
    set_val: serde_json::Value,
    remaining_selectors: &mut Vec<Segment>,   
) -> Result<(), String> {
    // if there's nothing at key, create it as an empty object
    if current_val_ref.get(index).is_none() {
        if let Some(arr) = current_val_ref.as_array_mut() {
            while arr.len() <= index {
                arr.push(serde_json::Value::Object(Map::new()));
            }
        } else {
            return Err(format!("tried to access accumulator at $ ... '[{}]'. could not set that index as the value is not an array {:?}", index, current_val_ref))
        }
    }

    if let Some(obj) = current_val_ref.get_mut(index) {
        set_value_recursively(obj, set_val, remaining_selectors)
    } else {
        return Err(format!("failed to access accumulator at $ ... '[{}]'", index))
    }    
}

pub async fn run_template(
    logger: &'static dyn Log,
    resource_name: &str,
    template_name: &str,
    mut transition: Transition,
    transition_type: &str,
    input: serde_json::Value,
    last_input: Option<serde_json::Value>,
    last_output: Option<serde_json::Value>,
) -> Result<serde_json::Value, String> {
    log::info!(logger: logger, "{} '{}'", transition_gerund(transition_type), resource_name);
    let mut command_state = CommandState::new(input, last_output);
    let mut rng = fastrand::Rng::new();
    // TODO: process directives...
    for (i, command) in transition.cli_commands.drain(..).enumerate() {
        // last input should only be set for update transitions.
        // if its an update, we check if we should run this command:
        if let Some(last_input) = &last_input {
            if !should_run_update_cmd(&command.directives, last_input, &command_state.input)? {
                continue;
            }
        }
        let mut should_drop_output = command.directives.iter().any(|d|
            match d {
                Directive::DropOutput { .. } => true,
                _ => false
            }
        );
        let has_insert_directive = command.directives.iter().find(|d| {
            match d {
                Directive::Insert { .. } => true,
                _ => false,
            }
        }).is_some();
        // if there's an insert directive, drop the output
        if !should_drop_output {
            should_drop_output = has_insert_directive;
        }
        let val = match command.cmd {
            deploy_language::parse::template::CmdOrBuiltin::Command(cli_command) => {
                let arg_set = create_arg_set(resource_name, &command_state, &cli_command.arg_transforms)
                    .map_err(|e| format!(
                        "resource '{}' failed to create arg set from template '{}' {}[{}]: {}",
                        resource_name,
                        template_name,
                        transition_type,
                        i, e
                    ))?;
                run_command(arg_set, cli_command).await
                    .map_err(|e| format!(
                        "resource '{}' failed to run {}[{}] command from template '{}': {}",
                        resource_name,
                        transition_type,
                        i,
                        template_name,
                        e
                    ))?
            }
            deploy_language::parse::template::CmdOrBuiltin::Builtin(b) => {
                run_builtin::run_builtin(&mut rng, resource_name, b, &command_state)
                    .map_err(|e| format!(
                        "resource '{}' failed to run {}[{}] builtin from template '{}': {}",
                        resource_name,
                        transition_type,
                        i,
                        template_name,
                        e
                    ))?
            }
        };

        // TODO: allow user to define how values are merged via directives...
        // for now we will use default behavior which will be merging objects
        // or if its not an object, then we will take the latest value
        if !should_drop_output {
            command_state.accum = default_value_merge(command_state.accum, val);
        } else {
            // if we did not merge into the accumulator, check any insert directives
            // to explicitly insert into accumulator instead of merging the entire output:
            for d in command.directives.iter() {
                if let Directive::Insert { src_path, dest_path, .. } = d {
                    command_state.insert(&val, src_path, dest_path)?;
                }
            }
        }

        // after merge/insert, check for any accumulator directives
        for d in command.directives.iter() {
            if let Directive::Accum { src_path, accum_path, .. } = d {
                command_state.insert_accum(resource_name, src_path, accum_path)?;
            }
        }
    }
    Ok(command_state.accum)
}

pub fn should_run_update_cmd(
    directives: &[Directive],
    previous: &serde_json::Value,
    current: &serde_json::Value,
) -> Result<bool, String> {
    // if there are no directives, return true => we can run the command
    if directives.len() == 0 {
        return Ok(true)
    }
    let mut has_diff_or_same_directive = false;
    // if any directive passes, return true
    // since they are ORed together
    for directive in directives.iter() {
        match directive {
            Directive::Diff { query, .. } => {
                has_diff_or_same_directive = true;
                if all_previous_current_differ(query, previous, current)? {
                    // prev != current => all are different => diff check succeeds
                    return Ok(true)
                }
            }
            Directive::Same { query, .. } => {
                has_diff_or_same_directive = true;
                if all_previous_current_match(query, previous, current)? {
                    // prev == current => all are same => same check succeeds
                    return Ok(true)
                }
            }
            _ => {
                // other directive types are not relevant to update
            }
        }
    }
    // if there were directives but none were diff/same directives, then we can update:
    if !has_diff_or_same_directive {
        return Ok(true);
    }
    // none of the directives passed, this command should not run
    Ok(false)
}

pub fn all_previous_current_differ(
    queries: &[JpQuery],
    previous: &serde_json::Value,
    current: &serde_json::Value
) -> Result<bool, String> {
    // AND together the queries
    // return true if NONE match
    for q in queries.iter() {
        if previous_matches_current(q, previous, current)? {
            return Ok(false)
        }
    }
    Ok(true)
}

pub fn all_previous_current_match(
    queries: &[JpQuery],
    previous: &serde_json::Value,
    current: &serde_json::Value
) -> Result<bool, String> {
    // AND together the queries
    // return true if ALL match
    for q in queries.iter() {
        if !previous_matches_current(q, previous, current)? {
            return Ok(false)
        }
    }
    Ok(true)
}

pub fn previous_matches_current(
    query: &JpQuery,
    previous: &serde_json::Value,
    current: &serde_json::Value
) -> Result<bool, String> {
    let mut prev_val = jsonpath_rust::query::js_path_process(query, previous)
        .map_err(|e| format!("failed to evaluate json path query '{}' for previous state entry {:?}. error: {:?}", query, previous, e))?;
    let mut current_val = jsonpath_rust::query::js_path_process(query, current)
        .map_err(|e| format!("failed to evaluate json path query '{}' for current entry {:?}. error: {:?}", query, current, e))?;
    
    let mut prev_iter = prev_val.drain(..);
    let mut curr_iter = current_val.drain(..);
    loop {
        let prev = prev_iter.next();
        let curr = curr_iter.next();
        match (prev, curr) {
            (None, None) => break,
            (None, Some(_)) => return Ok(false),
            (Some(_), None) => return Ok(false),
            (Some(p), Some(c)) => {
                if p.val() != c.val() {
                    return Ok(false)
                }
            }
        }
    }

    // if we got here all values matched
    Ok(true)
}

pub fn default_value_merge(
    previous: serde_json::Value,
    current: serde_json::Value
) -> serde_json::Value {
    match (previous, current) {
        (serde_json::Value::Object(mut prev), serde_json::Value::Object(curr)) => {
            for (key, val) in curr {
                prev.insert(key, val);
            }
            serde_json::Value::Object(prev)
        }
        (_, curr) => curr
    }
}

pub async fn run_command(arg_set: ArgSet, command: CliCommand) -> Result<serde_json::Value, String> {
    let command_name = command.command.s;
    let mut cmd = Command::new(command_name.clone());
    for prefix_arg in command.prefix_args {
        cmd.arg(prefix_arg.s);
    }
    for instruction in arg_set {
        let prefix = match instruction.opt_type {
            ArgOptType::Long => "--",
            ArgOptType::Short => "-",
            ArgOptType::Verbatim => "",
        };
        cmd.arg(format!("{}{}", prefix, instruction.name));
        let val_string = match instruction.value {
            serde_json::Value::Null => {
                //TODO: should nulls be omitted?
                "null".to_string()
            }
            serde_json::Value::Bool(b) => {
                b.to_string()
            }
            serde_json::Value::Number(number) => {
                number.to_string()
            }
            serde_json::Value::String(s) => s,
            // arrays and objects serialize as json string:
            x => {
                serde_json::to_string(&x)
                    .map_err(|e| format!("failed to serialize json value of arg with key '{}': {:?}", instruction.name, e))?
            }
        };
        cmd.arg(val_string);
    }

    let output = cmd.output().await
        .map_err(|e| format!("failed to invoke command '{}': {:?}", command_name, e))?;
    if !output.status.success() {
        let cmd = cmd.as_std();
        let mut command_formatted = command_name.clone();
        for arg in cmd.get_args() {
            command_formatted.push(' ');
            command_formatted.push_str(&arg.to_string_lossy().to_string());
        }
        let code = output.status.code().unwrap_or(1);
        let stderr = String::from_utf8_lossy(&output.stderr).to_string();
        let stdout = String::from_utf8_lossy(&output.stdout).to_string();
        return Err(format!("command '{}' exited with non success code '{}'. stdout: {}\nstderr: {}\n", command_formatted, code, stdout, stderr));
    }
    let stdout = String::from_utf8(output.stdout)
        .map_err(|_| format!("command '{}' returned non-utf8 stdout", command_name))?;
    // try to parse as json. if it fails fallback to treating it as a string:
    let out_val = serde_json::from_str(&stdout)
        .unwrap_or_else(|_| serde_json::Value::String(stdout));
    Ok(out_val)
}

pub fn create_arg_set(
    name: &str,
    command_state: &CommandState,
    arg_transforms: &[ArgTransform]
) -> Result<ArgSet, String> {
    let mut arg_set = ArgSet::new();
    for transform in arg_transforms {
        match transform {
            ArgTransform::Destructure { val: jp_query , .. } => {
                let val = command_state.evaluate_arg_transform(name, jp_query)?;
                let map = match val {
                    serde_json::Value::Object(map) => map,
                    x => return Err(format!("json path query '{}' did not return a json object. found: {:?}", jp_query.to_string(), x))
                };
                // add in alphabetical order. default to long opts for keys of length > 1, short opt otherwise.
                // TODO: allow directives on top of arg transforms to specify things such as order, long vs short vs verbatim, etc.
                let mut map_flat: Vec<_> = map.into_iter().collect();
                map_flat.sort_by(|a, b| a.0.cmp(&b.0));
                for (key, value) in map_flat {
                    let opt_type = if key.len() > 1 {
                        ArgOptType::Long
                    } else {
                        ArgOptType::Short
                    };
                    arg_set.push(ArgInstruction { opt_type, name: key, value });
                }
            }
            ArgTransform::Remove { val: string_at_line, .. } => {
                // TODO: should remove remove all, last or first occurrence of this key?
                // for now i think makes sense to only remove the last found.
                // perhaps a different remove syntax like "!! xyz" can remove all occurrances
                if let Some(index) = arg_set.iter().rposition(|x| x.name == string_at_line.s) {
                    arg_set.remove(index);
                }
            }
            ArgTransform::Add { key: string_at_line, val: jp_query, .. } => {
                // get value from user's input:
                let value = command_state.evaluate_arg_transform(name, jp_query)?;
                arg_set.push(ArgInstruction { opt_type: ArgOptType::Verbatim, name: string_at_line.s.clone(), value });
            }
        }
    }

    Ok(arg_set)
}

#[cfg(test)]
mod test {
    use assert_matches::assert_matches;
    use str_at_line::StringAtLine;

    use super::*;

    fn prev_match_curr(jpq: &str, prev: serde_json::Value, curr: serde_json::Value) -> bool {
        let jpq = jsonpath_rust::parser::parse_json_path(jpq).expect("failed to parse json path query");
        previous_matches_current(&jpq, &prev, &curr)
            .expect("failed to query json paths")
    }

    #[test]
    fn update_diff_works() {
        assert_eq!(true,
            prev_match_curr("$.input.somefield",
                serde_json::json!({"input": {"somefield": "a"}}),
                serde_json::json!({"input": {"somefield": "a"}}),
            )
        );

        // different value
        assert_eq!(false,
            prev_match_curr("$.input.somefield",
                serde_json::json!({"input": {"somefield": "a"}}),
                serde_json::json!({"input": {"somefield": "b"}}),
            )
        );
        // different type
        assert_eq!(false,
            prev_match_curr("$.input.somefield",
                serde_json::json!({"input": {"somefield": false}}),
                serde_json::json!({"input": {"somefield": "b"}}),
            )
        );

        // entire object
        assert_eq!(true,
            prev_match_curr("$",
                serde_json::json!({"input": {"somefield": "b"}}),
                serde_json::json!({"input": {"somefield": "b"}}),
            )
        );
        assert_eq!(false,
            prev_match_curr("$",
                serde_json::json!({"input": {"somefield": "b"}}),
                serde_json::json!({"input": {"somefield": {"b": "b"}}}),
            )
        );
    }

    macro_rules! same {
        ($($x:literal),*) => {
            Directive::Same { kw: Default::default(), query_src: Default::default(), query: vec![$(jsonpath_rust::parser::parse_json_path($x).unwrap(),)*]}
        };
    }

    macro_rules! diff {
        ($($x:literal),*) => {
            Directive::Diff { kw: Default::default(), query_src: Default::default(), query: vec![$(jsonpath_rust::parser::parse_json_path($x).unwrap(),)*]}
        };
    }

    #[test]
    fn should_run_update_cmd_works_simple_same() {
        // a must be the same for it to run the update command
        let directives = vec![
            same!("$.a")
        ];
        let previous = serde_json::json!({"a":"a"});
        let current = serde_json::json!({"a":"a"});
        assert_eq!(true, should_run_update_cmd(&directives, &previous, &current).unwrap());

        // the value of a is now different, it should not run this command
        let current = serde_json::json!({"a":"b"});
        assert_eq!(false, should_run_update_cmd(&directives, &previous, &current).unwrap());
    }

    #[test]
    fn can_insert_into_accumulator() {
        let mut cs = CommandState::new(serde_json::json!({"inputa": "inputa"}), Some(serde_json::json!({"a": "a"})));
        let src_path = jsonpath_rust::parser::parse_json_path("$.input.inputa").unwrap();
        let accum_path = jsonpath_rust::parser::parse_json_path("$.b").unwrap();
        cs.insert_accum("a", &src_path, &accum_path).unwrap();
        assert_eq!(cs.accum, serde_json::json!({"a": "a", "b": "inputa"}));
    }

    #[test]
    fn can_insert_into_accumulator_fields_that_dont_exist() {
        let mut cs = CommandState::new(serde_json::json!({"inputa": "inputa"}), Some(serde_json::json!({})));
        let src_path = jsonpath_rust::parser::parse_json_path("$.input.inputa").unwrap();
        let accum_path = jsonpath_rust::parser::parse_json_path("$.this.is.entirely.new").unwrap();
        cs.insert_accum("a", &src_path, &accum_path).unwrap();
        assert_eq!(cs.accum, serde_json::json!({"this": {"is": {"entirely": {"new": "inputa"}}}}));
    }

    #[test]
    fn cannot_change_accumulator_field_type() {
        let mut cs = CommandState::new(serde_json::json!({"inputa": "inputa"}), Some(serde_json::json!({"this": "is already a string"})));
        let src_path = jsonpath_rust::parser::parse_json_path("$.input.inputa").unwrap();
        let accum_path = jsonpath_rust::parser::parse_json_path("$.this.is.entirely.new").unwrap();
        let err = cs.insert_accum("a", &src_path, &accum_path).expect_err("it should err");
        assert_eq!(err, "tried to access accumulator at $ ... 'is'. could not set that key as the value is not an object String(\"is already a string\")");
    }

    #[test]
    fn can_insert_into_accumulator_fields_that_do_exist() {
        let mut cs = CommandState::new(serde_json::json!({"inputa": "inputa"}), Some(serde_json::json!({"this": "should be replaced"})));
        let src_path = jsonpath_rust::parser::parse_json_path("$.input.inputa").unwrap();
        let accum_path = jsonpath_rust::parser::parse_json_path("$.this").unwrap();
        cs.insert_accum("a", &src_path, &accum_path).unwrap();
        assert_eq!(cs.accum, serde_json::json!({"this": "inputa"}));
    }

    #[test]
    fn can_insert_into_accumulator_fields_via_index() {
        let mut cs = CommandState::new(serde_json::json!({"inputa": "inputa"}), Some(serde_json::json!({"this": ["will grow"]})));
        let src_path = jsonpath_rust::parser::parse_json_path("$.input.inputa").unwrap();
        let accum_path = jsonpath_rust::parser::parse_json_path("$.this[3]").unwrap();
        cs.insert_accum("a", &src_path, &accum_path).unwrap();
        assert_eq!(cs.accum, serde_json::json!({"this": ["will grow", {}, {}, "inputa"]}));

        let mut cs = CommandState::new(serde_json::json!({"inputa": "inputa"}), Some(serde_json::json!({"this": ["will grow"]})));
        let accum_path = jsonpath_rust::parser::parse_json_path("$.this[3].nested").unwrap();
        cs.insert_accum("a", &src_path, &accum_path).unwrap();
        assert_eq!(cs.accum, serde_json::json!({"this": ["will grow", {}, {}, {"nested": "inputa"}]}));
    }

    #[test]
    fn can_insert_entire_output_into_accum() {
        let mut cs = CommandState::new(serde_json::json!({"inputa": "inputa"}), None);
        let dest_path = jsonpath_rust::parser::parse_json_path("$.some_madeup_field").unwrap();
        cs.insert(&serde_json::json!({"a": "b"}), &None, &dest_path).unwrap();
        assert_eq!(cs.accum, serde_json::json!({"some_madeup_field": {"a": "b"}}));
    }

    #[test]
    fn can_insert_nested_output_val_into_accum() {
        let mut cs = CommandState::new(serde_json::json!({}), None);
        let src_path = jsonpath_rust::parser::parse_json_path("$.a.b.c").unwrap();
        let dest_path = jsonpath_rust::parser::parse_json_path("$.some_madeup_field.nested").unwrap();
        cs.insert(&serde_json::json!({"a": {"b": {"c": ["hello"]}}}), &Some(src_path), &dest_path).unwrap();
        assert_eq!(cs.accum, serde_json::json!({"some_madeup_field": {"nested": ["hello"]}}));
    }

    #[test]
    fn cannot_insert_into_accum_if_its_not_an_object() {
        let mut cs = CommandState::new(serde_json::json!({}), Some(serde_json::json!("this is a string")));
        let dest_path = jsonpath_rust::parser::parse_json_path("$.some_madeup_field.nested").unwrap();
        let err = cs.insert(&serde_json::json!({"a": "b"}), &None, &dest_path).expect_err("it should fail");
        assert_eq!(err, "tried to access accumulator at $ ... 'some_madeup_field'. could not set that key as the value is not an object String(\"this is a string\")");
    }

    #[test]
    fn should_run_update_cmd_works_simple_diff() {
        // a must be different for it to run the update command
        let directives = vec![
            diff!("$.a")
        ];
        let previous = serde_json::json!({"a":"a"});
        let current = serde_json::json!({"a":"a"});
        // command is not to be ran because the value of a has not changed
        assert_eq!(false, should_run_update_cmd(&directives, &previous, &current).unwrap());

        // the value of a is now different, it should run
        let current = serde_json::json!({"a":"b"});
        assert_eq!(true, should_run_update_cmd(&directives, &previous, &current).unwrap());
    }

    #[test]
    fn should_run_update_cmd_works_multiple_same() {
        // a AND b must be the same for it to run the update command
        let directives = vec![
            same!("$.a", "$.b")
        ];
        let previous = serde_json::json!({"a":"a", "b": "b"});
        let current = serde_json::json!({"a":"a", "b": "b"});
        // command is to be ran because the value of a and b are the same in prev and current
        assert_eq!(true, should_run_update_cmd(&directives, &previous, &current).unwrap());

        // the value of a is now different, it should not run
        let current = serde_json::json!({"a":"b", "b": "b"});
        assert_eq!(false, should_run_update_cmd(&directives, &previous, &current).unwrap());

        // the value of b is now different, it should not run:
        let current = serde_json::json!({"a":"a", "b": "c"});
        assert_eq!(false, should_run_update_cmd(&directives, &previous, &current).unwrap());

        // both are now different, it should not run:
        let current = serde_json::json!({"a":"1", "b": "2"});
        assert_eq!(false, should_run_update_cmd(&directives, &previous, &current).unwrap());

        // missing one of the values: it should not run
        let current = serde_json::json!({"a":"a"});
        assert_eq!(false, should_run_update_cmd(&directives, &previous, &current).unwrap());

        // missing all values: it should not run
        let current = serde_json::json!({});
        assert_eq!(false, should_run_update_cmd(&directives, &previous, &current).unwrap());

        // entirely different type: it should not run
        let current = serde_json::json!(null);
        assert_eq!(false, should_run_update_cmd(&directives, &previous, &current).unwrap());
    }

    #[test]
    fn should_run_update_cmd_works_without_diff_directives() {
        // the only directive is a drop output directive, so should_run_update_cmd
        // should not be affected by this directive. it should return true
        let directives = vec![
            Directive::DropOutput { kw: Default::default() }
        ];
        let previous = serde_json::json!({"a":"a", "b": "b"});
        let current = serde_json::json!({"a":"a", "b": "b"});
        assert_eq!(true, should_run_update_cmd(&directives, &previous, &current).unwrap());
    }

    #[test]
    fn should_run_update_cmd_works_multiple_diff() {
        // a AND b must BOTH be different for it to run the update command
        let directives = vec![
            diff!("$.a", "$.b")
        ];
        let previous = serde_json::json!({"a":"a", "b": "b"});
        let current = serde_json::json!({"a":"a", "b": "b"});
        // command is NOT to be ran because the value of a and b are the same in prev and current
        assert_eq!(false, should_run_update_cmd(&directives, &previous, &current).unwrap());

        // the value of a is now different, it should still not run because b is the same
        let current = serde_json::json!({"a":"b", "b": "b"});
        assert_eq!(false, should_run_update_cmd(&directives, &previous, &current).unwrap());

        // the value of b is now different, it should still not run because a is the same:
        let current = serde_json::json!({"a":"a", "b": "c"});
        assert_eq!(false, should_run_update_cmd(&directives, &previous, &current).unwrap());

        // both are now different, it should run:
        let current = serde_json::json!({"a":"1", "b": "2"});
        assert_eq!(true, should_run_update_cmd(&directives, &previous, &current).unwrap());

        // missing one of the values: it should not run because a is still the same
        let current = serde_json::json!({"a":"a"});
        assert_eq!(false, should_run_update_cmd(&directives, &previous, &current).unwrap());

        // missing all values: it should run because both values are missing therefore both different
        let current = serde_json::json!({});
        assert_eq!(true, should_run_update_cmd(&directives, &previous, &current).unwrap());

        // entirely different type: it should run
        let current = serde_json::json!(null);
        assert_eq!(true, should_run_update_cmd(&directives, &previous, &current).unwrap());
    }

    #[test]
    fn should_run_update_cmd_works_directives_ored_together() {
        // a AND b must BOTH be different for it to run the update command
        // OR
        // c must be the same
        let directives = vec![
            diff!("$.a", "$.b"),
            same!("$.c")
        ];
        let previous = serde_json::json!({"a":"a", "b": "b", "c": "c"});
        let current = serde_json::json!({"a":"a", "b": "b", "c": "c"});
        // command is to be ran because c is the same
        assert_eq!(true, should_run_update_cmd(&directives, &previous, &current).unwrap());

        // c is not the same, but a and b both differ, so it should run
        let current = serde_json::json!({"a":"nota", "b": "notb", "c": "notc"});
        assert_eq!(true, should_run_update_cmd(&directives, &previous, &current).unwrap());

        // it passes both conditions: c is the same AND a and b differ
        let current = serde_json::json!({"a":"nota", "b": "notb", "c": "c"});
        assert_eq!(true, should_run_update_cmd(&directives, &previous, &current).unwrap());

        // it passes neither conditions: neither a nor b differ, AND c is not the same:
        let current = serde_json::json!({"a":"a", "b": "b", "c": "notc"});
        assert_eq!(false, should_run_update_cmd(&directives, &previous, &current).unwrap());
    }

    #[test]
    fn can_evaluate_arg_transform_from_command_state_input() {
        let cs = CommandState::new(serde_json::json!({"a": "aval"}), None);
        let jpq = jsonpath_rust::parser::parse_json_path("$.input").unwrap();
        let evaluated = cs.evaluate_arg_transform("resource1", &jpq).expect("it should not error");
        assert_eq!(evaluated, serde_json::json!({"a":"aval"}));
        let jpq = jsonpath_rust::parser::parse_json_path("$.input.a").unwrap();
        let evaluated = cs.evaluate_arg_transform("resource1", &jpq).expect("it should not error");
        assert_eq!(evaluated, serde_json::json!("aval"));
    }

    #[test]
    fn can_evaluate_arg_transform_from_command_state_output() {
        let cs = CommandState::new(serde_json::json!({"a": "aval"}), Some(serde_json::json!({"aout": "aoutval"})));
        let jpq = jsonpath_rust::parser::parse_json_path("$.output").unwrap();
        let evaluated = cs.evaluate_arg_transform("resource1", &jpq).expect("it should not error");
        assert_eq!(evaluated, serde_json::json!({"aout":"aoutval"}));
        let jpq = jsonpath_rust::parser::parse_json_path("$.output.aout").unwrap();
        let evaluated = cs.evaluate_arg_transform("resource1", &jpq).expect("it should not error");
        assert_eq!(evaluated, serde_json::json!("aoutval"));

        // for create TRs, there wont be an output, it will be null.
        // if for some reason a template create section references output it shouldnt panic, but just return null:
        let cs = CommandState::new(serde_json::json!({"a": "aval"}), None);
        let jpq = jsonpath_rust::parser::parse_json_path("$.output").unwrap();
        let evaluated = cs.evaluate_arg_transform("resource1", &jpq).expect("it should not error");
        assert_eq!(evaluated, serde_json::Value::Object(Default::default()));
        let jpq = jsonpath_rust::parser::parse_json_path("$.output.some.value").unwrap();
        let evaluated = cs.evaluate_arg_transform("resource1", &jpq).expect("it should not error");
        assert_eq!(evaluated, serde_json::Value::Array(vec![]));
    }

    #[test]
    fn can_evaluate_arg_transform_from_command_state_accum() {
        let mut cs = CommandState::new(serde_json::json!({"a": "aval"}), None);
        let jpq = jsonpath_rust::parser::parse_json_path("$.accum").unwrap();
        let evaluated = cs.evaluate_arg_transform("resource1", &jpq).expect("it should not error");
        assert_eq!(evaluated, serde_json::json!({}));
        let jpq = jsonpath_rust::parser::parse_json_path("$.accum.aout").unwrap();
        let evaluated = cs.evaluate_arg_transform("resource1", &jpq).expect("it should not error");
        assert_eq!(evaluated, serde_json::json!([]));

        // now accum actually has a value, it should be look-up-able
        cs.accum = serde_json::json!({"aout": "aoutval"});
        let jpq = jsonpath_rust::parser::parse_json_path("$.accum.aout").unwrap();
        let evaluated = cs.evaluate_arg_transform("resource1", &jpq).expect("it should not error");
        assert_eq!(evaluated, serde_json::json!("aoutval"));
    }

    #[test]
    fn can_evaluate_arg_transform_from_command_state_resource_name() {
        let cs = CommandState::new(serde_json::json!({"a": "aval"}), None);
        let jpq = jsonpath_rust::parser::parse_json_path("$.name").unwrap();
        let evaluated = cs.evaluate_arg_transform("resource1", &jpq).expect("it should not error");
        assert_eq!(evaluated, serde_json::json!("resource1"));
    }

    #[test]
    fn evaluate_arg_transform_errs_on_non_input_output_accum_name() {
        let cs = CommandState::new(serde_json::json!({"a": "aval"}), None);
        let jpq = jsonpath_rust::parser::parse_json_path("$.this.is.invalid").unwrap();
        let err = cs.evaluate_arg_transform("resource1", &jpq).expect_err("it should error");
        assert_eq!(err, "json path query '$thisisinvalid' for resource 'resource1' must start with $.input $.output $.accum or $.name");
        let jpq = jsonpath_rust::parser::parse_json_path("$").unwrap();
        let err = cs.evaluate_arg_transform("resource1", &jpq).expect_err("it should error");
        assert_eq!(err, "arg transform json path query for resource 'resource1' must have at least 1 segment");
        let jpq = jsonpath_rust::parser::parse_json_path("$[0]").unwrap();
        let err = cs.evaluate_arg_transform("resource1", &jpq).expect_err("it should error");
        assert_eq!(err, "json path query '$0' for resource 'resource1' must start with $.input $.output $.accum or $.name");
    }

    #[test]
    fn can_create_arg_set_destructure_long_short() {
        let cs = CommandState::new(serde_json::json!({"a": "b", "long": 123.4}), None);
        let arg_transforms = vec![
            ArgTransform::destructure(jsonpath_rust::parser::parse_json_path("$.input").unwrap())
        ];
        let arg_set = create_arg_set("somename", &cs, &arg_transforms).unwrap();
        assert_eq!(arg_set.len(), 2);
        // a should be destructured to a short opt since its 1 character
        assert_eq!(&arg_set[0].name, "a");
        assert_eq!(arg_set[0].value, serde_json::json!("b"));
        assert_matches!(&arg_set[0].opt_type, ArgOptType::Short);

        // this should become a long opt because its more than 1 character
        assert_eq!(&arg_set[1].name, "long");
        assert_eq!(arg_set[1].value, serde_json::json!(123.4));
        assert_matches!(&arg_set[1].opt_type, ArgOptType::Long);
    }

    #[test]
    fn can_create_arg_set_verbatim() {
        let cs = CommandState::new(serde_json::json!({"a": "b", "long": 123.4}), None);
        let mut verbatim_s = StringAtLine::default();
        verbatim_s.s = "verbatim".to_string();
        let arg_transforms = vec![
            ArgTransform::add(verbatim_s, jsonpath_rust::parser::parse_json_path("$.input.a").unwrap()),
        ];
        let arg_set = create_arg_set("somename", &cs, &arg_transforms).unwrap();
        assert_eq!(arg_set.len(), 1);
        // the arg was added verbatim:
        assert_eq!(&arg_set[0].name, "verbatim");
        assert_eq!(arg_set[0].value, serde_json::json!("b"));
        assert_matches!(&arg_set[0].opt_type, ArgOptType::Verbatim);
    }

    #[test]
    fn can_create_arg_set_remove_only_last_occurrance() {
        let cs = CommandState::new(serde_json::json!({"verbatim": "b", "xyz": "xyz"}), None);
        let mut verbatim_s = StringAtLine::default();
        verbatim_s.s = "verbatim".to_string();
        let arg_transforms = vec![
            // verbatim xyz
            ArgTransform::add(verbatim_s.clone(), jsonpath_rust::parser::parse_json_path("$.input.xyz").unwrap()),
            // --verbatim b
            ArgTransform::destructure(jsonpath_rust::parser::parse_json_path("$.input").unwrap()),
            // removes the last occurrance of key "verbatim" which should
            // remove the one that added --verbatim b
            ArgTransform::remove(verbatim_s.clone())
        ];
        let arg_set = create_arg_set("somename", &cs, &arg_transforms).unwrap();
        assert_eq!(arg_set.len(), 2);
        assert_eq!(&arg_set[0].name, "verbatim");
        assert_eq!(arg_set[0].value, serde_json::json!("xyz"));
        assert_matches!(&arg_set[0].opt_type, ArgOptType::Verbatim);

        assert_eq!(&arg_set[1].name, "xyz");
        assert_eq!(arg_set[1].value, serde_json::json!("xyz"));
        assert_matches!(&arg_set[1].opt_type, ArgOptType::Long);

        // now in reverse, we destructure first, then add. it should remove the added "verbatim" key
        let arg_transforms = vec![
            // --verbatim b
            ArgTransform::destructure(jsonpath_rust::parser::parse_json_path("$.input").unwrap()),
            // verbatim xyz
            ArgTransform::add(verbatim_s.clone(), jsonpath_rust::parser::parse_json_path("$.input.xyz").unwrap()),
            // removes the last occurrance of key "verbatim" which should
            // remove the one that added --verbatim b
            ArgTransform::remove(verbatim_s.clone())
        ];
        let arg_set = create_arg_set("somename", &cs, &arg_transforms).unwrap();
        assert_eq!(arg_set.len(), 2);
        assert_eq!(&arg_set[0].name, "verbatim");
        assert_eq!(arg_set[0].value, serde_json::json!("b"));
        assert_matches!(&arg_set[0].opt_type, ArgOptType::Long);

        assert_eq!(&arg_set[1].name, "xyz");
        assert_eq!(arg_set[1].value, serde_json::json!("xyz"));
        assert_matches!(&arg_set[1].opt_type, ArgOptType::Long);
    }
}

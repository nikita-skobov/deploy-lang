use std::fmt::Display;

use dcl_language::parse::template::{ArgTransform, CliCommand, CliCommandWithDirectives, Directive, Transition};
use jsonpath_rust::parser::model::JpQuery;
use log::Log;
use tokio::process::Command;

pub type ArgSet = serde_json::Map<String, serde_json::Value>;

struct Gerund<'a> {
    pub verb: &'a str,
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

pub async fn run_template(
    logger: &'static dyn Log,
    resource_name: &str,
    template_name: &str,
    mut transition: Transition,
    transition_type: &str,
    input: serde_json::Value,
    last_input: Option<serde_json::Value>,
) -> Result<serde_json::Value, String> {
    log::info!(logger: logger, "{} '{}'", transition_gerund(transition_type), resource_name);
    let mut out_val = serde_json::Value::Object(Default::default());
    // TODO: process directives...
    for (i, command) in transition.cli_commands.drain(..).enumerate() {
        // last input should only be set for update transitions.
        // if its an update, we check if we should run this command:
        if let Some(last_input) = &last_input {
            if !should_run_update_cmd(&command.directives, last_input, &input)? {
                continue;
            }
        }
        let arg_set = create_arg_set(&input, &command.cmd.arg_transforms)
            .map_err(|e| format!(
                "resource '{}' failed to create arg set from template '{}' {}[{}]: {}",
                resource_name,
                template_name,
                transition_type,
                i, e
            ))?;
        let val = run_command(arg_set, command.cmd).await
            .map_err(|e| format!(
                "resource '{}' failed to run {}[{}] command from template '{}': {}",
                resource_name,
                transition_type,
                i,
                template_name,
                e
            ))?;
        // TODO: allow user to define how values are merged via directives...
        // for now we will use default behavior which will be merging objects
        // or if its not an object, then we will take the latest value
        out_val = default_value_merge(out_val, val);
    }
    Ok(out_val)
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
    // if any directive passes, return true
    // since they are ORed together
    for directive in directives.iter() {
        match directive {
            Directive::Diff { query, .. } => {
                if all_previous_current_differ(query, previous, current)? {
                    // prev != current => all are different => diff check succeeds
                    return Ok(true)
                }
            }
            Directive::Same { query, .. } => {
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
    // add all the args from the arg set, serializing if it's not a simple type:
    let mut flat_arg_set = Vec::with_capacity(arg_set.len());
    for (key, value) in arg_set.into_iter() {
        match value {
            serde_json::Value::Null => {
                // TODO: should nulls be omitted?
                flat_arg_set.push((key, "null".to_string()));
            },
            serde_json::Value::Bool(v) => {
                flat_arg_set.push((key, v.to_string()));
            }
            serde_json::Value::Number(number) => {
                flat_arg_set.push((key, number.to_string()));
            }
            serde_json::Value::String(s) => {
                flat_arg_set.push((key, s));
            }
            serde_json::Value::Array(values) => {
                let s = serde_json::to_string(&values)
                    .map_err(|e| format!("failed to serialize array values of key '{}': {:?}", key, e))?;
                flat_arg_set.push((key, s));
            }
            serde_json::Value::Object(map) => {
                let s = serde_json::to_string(&map)
                    .map_err(|e| format!("failed to serialize object values of key '{}': {:?}", key, e))?;
                flat_arg_set.push((key, s));
            }
        }
    }
    // want consistent ordering so convert arg set to a flat list, sorting by key:
    flat_arg_set.sort_by(|a, b| a.0.cmp(&b.0));
    for (key, val) in flat_arg_set {
        // TODO: allow user to determine the format of each key. for now
        // we assume long option syntax for all
        cmd.arg(format!("--{}", key));
        cmd.arg(val);
    }

    let output = cmd.output().await
        .map_err(|e| format!("failed to invoke command '{}': {:?}", command_name, e))?;
    if !output.status.success() {
        let code = output.status.code().unwrap_or(1);
        let stderr = String::from_utf8_lossy(&output.stderr).to_string();
        let stdout = String::from_utf8_lossy(&output.stdout).to_string();
        return Err(format!("command '{}' exited with non success code '{}'. stdout: {}\nstderr: {}\n", command_name, code, stdout, stderr));
    }
    let stdout = String::from_utf8(output.stdout)
        .map_err(|_| format!("command '{}' returned non-utf8 stdout", command_name))?;
    // try to parse as json. if it fails fallback to treating it as a string:
    let out_val = serde_json::from_str(&stdout)
        .unwrap_or_else(|_| serde_json::Value::String(stdout));
    Ok(out_val)
}

pub fn create_arg_set(
    user_input: &serde_json::Value,
    arg_transforms: &[ArgTransform]
) -> Result<ArgSet, String> {
    let mut arg_set = ArgSet::new();
    for transform in arg_transforms {
        match transform {
            ArgTransform::Destructure(jp_query) => {
                let mut vals = jsonpath_rust::query::js_path_process(jp_query, user_input).map_err(|e| format!("failed to process json path '{}': {:?}", jp_query.to_string(), e))?;
                if vals.len() != 1 {
                    return Err(format!(
                        "json path query '{}' did not return exactly 1 json object. found: {:?}",
                        jp_query.to_string(),
                        vals.iter().map(|x| x.clone().val().clone()).collect::<Vec<_>>(),
                    ));
                }
                let val = vals.pop().unwrap();
                let map = match val.val() {
                    serde_json::Value::Object(map) => map,
                    x => return Err(format!("json path query '{}' did not return a json object. found: {:?}", jp_query.to_string(), x))
                };
                arg_set.extend(map.clone());
            }
            ArgTransform::Remove(string_at_line) => {
                arg_set.remove(&string_at_line.s);
            }
            ArgTransform::Add(string_at_line, jp_query) => {
                // get value from user's input:
                let mut vals = jsonpath_rust::query::js_path_process(jp_query, user_input)
                    .map_err(|e| format!("failed to process json path query '{}'. error: {:?}", jp_query.to_string(), e))?;
                let mut vals: Vec<serde_json::Value> = vals.drain(..).map(|x| x.val().clone()).collect();
                // TODO: check for not found results.. jsonpath_rust will return an empty array if the lookup fails!
                let val = if vals.len() == 1 {
                    vals.pop().unwrap()
                } else {
                    serde_json::Value::Array(vals)
                };
                arg_set.insert(string_at_line.s.clone(), val);
            }
        }
    }

    Ok(arg_set)
}

#[cfg(test)]
mod test {
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

    #[test]
    fn diff_directive_checks_work_multiple_diff() {
        // we say this command should only run
        // if field A has changed AND field B has changed
        
    }
}

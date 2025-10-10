use std::fmt::Display;

use dcl_language::parse::template::{ArgTransform, CliCommand, Transition};
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
    input: serde_json::Value
) -> Result<serde_json::Value, String> {
    log::info!(logger: logger, "{} '{}'", transition_gerund(transition_type), resource_name);
    let mut out_val = serde_json::Value::Object(Default::default());
    // TODO: process directives...
    for (i, command) in transition.cli_commands.drain(..).enumerate() {
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

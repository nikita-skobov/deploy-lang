//! module for evaluating constant items/functions before processing the rest of the .dpl file

use std::collections::{HashMap, HashSet};

use deploy_language::{parse::constant::ConstSection, validate::unquote_bracketed_selector, DplFile};
use json_with_positions::{convert_from_serde_value_recursively, convert_to_serde_value_recursively, Position, Value};
use jsonpath_rust::{parser::model::JpQuery};
use str_at_line::StringAtLine;

use crate::{process_json_path_query, run_function::{run_function_ex, FunctionCall, Resource}};

/// read all resources in the dpl file that reference a constant, either
/// inline in a resource (eg `{"a": $.some_constant}`) or a function call passing a constant
/// (eg `{"a": $.some_func['some_constant']}`)
pub async fn evaluate_constants(
    logger: &'static dyn log::Log,
    dpl: &mut DplFile,
) -> Result<(), String> {
    // collect all known const names for easier determining what is a const and what is not:
    let mut const_map = HashMap::new();
    for constsection in dpl.constants.iter() {
        const_map.insert(constsection.const_name.s.clone(), constsection.clone());
    }
    // also collect all function names for easier determining what is a function call:
    let mut fn_set = HashSet::new();
    for fnsection in dpl.functions.iter() {
        fn_set.insert(fnsection.function_name.s.clone());
    }
    // first process all const references that are inline (a resource referencing a constant value)
    // replace these inline:
    let fn_call_map = replace_all_inline_constants(&const_map, &fn_set, dpl)?;
    // the inline replacement returned a map of function calls we need to do. we do this async because
    // the inline replacement needs to happen synchronously.
    perform_fn_calls(logger, dpl, fn_call_map).await?;
    Ok(())
}

pub struct NoopLogger;
impl log::Log for NoopLogger {
    fn enabled(&self, _metadata: &log::Metadata) -> bool {
        false
    }

    fn log(&self, _record: &log::Record) {}

    fn flush(&self) {}
}

async fn perform_fn_calls(
    logger: &'static dyn log::Log,
    dpl: &mut DplFile,
    mut fn_call_map: HashMap<FnCall, Vec<FnCallReplacement>>
) -> Result<(), String> {
    let mut task_set = tokio::task::JoinSet::new();
    for (fn_call, _replacements) in fn_call_map.iter() {
        // get the function and const via its name:
        let function = dpl.functions.iter().find(|x| x.function_name.as_str() == fn_call.function_name);
        let constant = dpl.constants.iter().find(|x| x.const_name.as_str() == fn_call.const_name);
        let (function, constant) = match (function, constant) {
            (Some(f), Some(c)) => (f.clone(), c.clone()),
            _ => return Err(format!("failed to perform function call for fn '{}' on const '{}': unable to locate function and/or constant", fn_call.function_name, fn_call.const_name))
        };
        let fn_call_clone = fn_call.clone();
        // TODO: this can spawn arbitrarily many tasks! we should
        // set a max parallel task somewhere to avoid issues with users
        // who have hundreds/thousands const function evaluations
        task_set.spawn(async move {
            let body = function.get_body();
            let call = FunctionCall {
                function_name: function.function_name.s,
                function_type: function.function_type.s,
                function_body: body,
                depends_on: Resource {
                    name: constant.const_name.s,
                    input: convert_to_serde_value_recursively(constant.body),
                    output: serde_json::Value::Null,
                }
            };
            // important: pass the NoopLogger, not the actual logger
            // because it will log something that makes sense in a normal function call,
            // but we dont want to show this to users for a const function call
            let res = run_function_ex(&NoopLogger, "", call).await;
            (fn_call_clone, res)
        });
    }
    // now that all the function calls have been spawned, poll until one completes
    // and once it completes, perform the replacement for all of its replacements in the map
    // once replacements have been performed, remove it from the map.
    while let Some(next) = task_set.join_next().await {
        // if theres a task error simply log it and continue
        // we will error later when we check that the map still has entries it didnt fill.
        let (fn_call, res) = match next {
            Ok(o) => o,
            Err(join_err) => {
                log::error!(logger: logger, "Join error processing const function call: {:?}", join_err);
                continue;
            }
        };
        let replace_val = match res {
            Ok(o) => o,
            Err(e) => {
                // log now, will error later when we see that this key is still in the map
                log::error!(logger: logger, "Failed to process function '{}' on constant '{}': {}", fn_call.function_name, fn_call.const_name, e);
                continue;
            }
        };
        // look up all the replacements for this function call:
        let replacements = match fn_call_map.remove(&fn_call) {
            Some(r) => r,
            None => {
                log::warn!(logger: logger, "Function call for fn '{}' on const '{}' succeeded but failed to look up replacements", fn_call.function_name, fn_call.const_name);
                vec![]
            }
        };
        perform_fn_call_replacements(dpl, replace_val, replacements)?;
    }
    // after all replacements done, the map should be empty. if it is not empty, there was some error we already logged
    // so we can return a simple error message here:
    if !fn_call_map.is_empty() {
        return Err("failed to process one or more const function calls".to_string());
    }
    Ok(())
}

fn perform_fn_call_replacements(
    dpl: &mut DplFile,
    value: serde_json::Value,
    replacements: Vec<FnCallReplacement>
) -> Result<(), String> {
    let val_converted = convert_from_serde_value_recursively(value);
    for replacement in replacements {
        let resource = match dpl.resources.iter_mut().find(|x| x.resource_name.as_str() == replacement.resource_name) {
            Some(r) => r,
            None => {
                return Err(format!("failed to lookup resource '{}' for const function call replacement", replacement.resource_name));
            }
        };
        let inp = std::mem::take(&mut resource.input);
        let input = inp.to_value_with_replaced_json_paths(&mut |val, pos| {
            // if it matches the replacement, return the value, otherwise return the json path as-is:
            if val == replacement.val && pos == replacement.pos {
                Ok(val_converted.clone())
            } else {
                Ok(Value::JsonPath { pos, val })
            }
        })?;
        resource.input = input;
    }
    Ok(())
}

/// used as the key when calculating a hashmap of function calls to perform.
/// the key should be hashed to the function name and the constant it will evaluate, therefore
/// multiple fn calls of the same function to the same constant should evaluate only once
#[derive(Hash, PartialEq, Clone, Eq)]
struct FnCall {
    pub function_name: String,
    pub const_name: String,
}

/// represents the value to replace when the fn call completes.
/// a single fn call may have multiple replacements
struct FnCallReplacement {
    pub resource_name: String,
    pub val: StringAtLine,
    pub pos: Position,
}

fn replace_all_inline_constants(
    const_map: &HashMap<String, ConstSection>,
    fn_set: &HashSet<String>,
    dpl: &mut DplFile
) -> Result<HashMap<FnCall, Vec<FnCallReplacement>>, String> {
    let mut out: HashMap<FnCall, Vec<FnCallReplacement>> = HashMap::new();
    for resource in dpl.resources.iter_mut() {
        // replace the resource json value inline
        let inp = std::mem::take(&mut resource.input);
        let input = inp.to_value_with_replaced_json_paths(&mut |val, pos| {
            let mut jpq = match jsonpath_rust::parser::parse_json_path(val.as_str()) {
                Ok(o) => o,
                // dont error here. we can error with a more user friendly error later. for now
                // we ignore this, and return it as a value as-is
                Err(_) => return Ok(json_with_positions::Value::JsonPath { pos, val }),
            };
            // get the first segment, if first segment is a known const section
            // then evaluate it, otherwise return it as a json path as-is
            let const_jpq_seg = match jpq.segments.first() {
                Some(s) => s,
                None => return Ok(json_with_positions::Value::JsonPath { pos, val }),
            };
            let mut const_section_name = const_jpq_seg.to_string();
            unquote_bracketed_selector(&mut const_section_name);
            // first, check if this section name is actually a function, if its a function, and the 2nd
            // segment is a const item, then add it to the fn calls that we should make later:
            if fn_set.contains(&const_section_name) {
                // its a function call. is the 2nd segment a const?
                let mut second_segment_name = jpq.segments.iter().nth(1).map(|x| x.to_string()).unwrap_or_default();
                unquote_bracketed_selector(&mut second_segment_name);
                if const_map.contains_key(&second_segment_name) {
                    // add it to the fn call map, and return the json path as is.
                    // this will be replaced later.
                    let fn_call = FnCall {
                        function_name: const_section_name,
                        const_name: second_segment_name,
                    };
                    let replacement = FnCallReplacement { resource_name: resource.resource_name.s.clone(), val: val.clone(), pos: pos.clone() };
                    match out.get_mut(&fn_call) {
                        Some(existing) => {
                            existing.push(replacement);
                        }
                        None => {
                            out.insert(fn_call, vec![replacement]);
                        }
                    }
                    return Ok(json_with_positions::Value::JsonPath { pos, val });
                }
            }
            let const_section = match const_map.get(&const_section_name) {
                Some(s) => s,
                _ => return Ok(json_with_positions::Value::JsonPath { pos, val }),
            };
            // this is an inline const reference. replace the current json path with the const item
            // remove the first segment from the jpq because it's referencing the const name, which we already have:
            // this is safe because we already checked it has at least 1 segment
            jpq.segments.remove(0);
            let val = get_const_section_value(const_section, jpq)?;
            Ok(val)
        })?;
        resource.input = input;
    }
    Ok(out)
}

pub fn get_const_section_value(const_section: &ConstSection, jpq: JpQuery) -> Result<Value, String> {
    // this transformation is safe since const sections are required to be valid json:
    // (ie: theres no chance it inserts a fake object instead of a json path)
    let const_val = const_section.body.clone().to_serde_json_value();
    let mut result_vals = process_json_path_query(&jpq, &const_val).map_err(|e| {
        format!("failed to evaluate const section '{}'. json path error: {:?}", const_section.const_name, e)
    })?;
    // if the value looked up was a single value, use it as a single value
    let val = if result_vals.len() == 1 {
        result_vals.pop().unwrap_or_default()
    } else {
        // otherwise user likely indeed wanted it to be an array:
        let vals = result_vals;
        serde_json::Value::Array(vals)
    };
    Ok(convert_from_serde_value_recursively(val))
}

#[cfg(test)]
mod test {
    use assert_matches::assert_matches;
    use deploy_language::parse_and_validate;
    use str_at_line::StringAtLine;

    use crate::test_log::VecLogger;

    use super::*;

    #[test]
    fn can_eval_const_section() {
        let js = serde_json::json!({ "a": "b" });
        let body = convert_from_serde_value_recursively(js);
        let cs = ConstSection {
            const_name: StringAtLine::default(),
            body,
            end_line: 0,
        };
        let jpq = jsonpath_rust::parser::parse_json_path("$.a").unwrap();
        let val = get_const_section_value(&cs, jpq).unwrap();
        assert_matches!(val, Value::String { val, .. } => {
            assert_eq!(val, "b");
        });
    }

    #[tokio::test]
    async fn can_eval_const_inline_simple() {
        let logger = VecLogger::leaked();
        let document = r#"
const my_const
    { "a": "b" }

template some_template
  create
    echo hello

resource some_template(resource_a)
  {
    "const_evaluated": $.my_const
  }
"#.to_string();
        let mut dpl = parse_and_validate(&document).unwrap();
        evaluate_constants(logger, &mut dpl).await.expect("const evaluation should not fail");
        let resource_a = dpl.resources.iter().find(|x| x.resource_name.as_str() == "resource_a").unwrap();
        let resource_a_val = resource_a.input.clone().to_serde_json_value();
        assert_eq!(resource_a_val, serde_json::json!({"const_evaluated": {"a": "b"}}));
    }

    #[tokio::test]
    async fn can_eval_const_function_calls() {
        let logger = VecLogger::leaked();
        let document = r#"
const my_const
    { "a": "b" }

template some_template
  create
    echo hello

function javascript(my_func)
  return { "func": "evaluated!" }

resource some_template(resource_a)
  {
    "const_evaluated": $.my_func['my_const']
  }
"#.to_string();
        let mut dpl = parse_and_validate(&document).unwrap();
        evaluate_constants(logger, &mut dpl).await.expect("const evaluation should not fail");
        let resource_a = dpl.resources.iter().find(|x| x.resource_name.as_str() == "resource_a").unwrap();
        let resource_a_val = resource_a.input.clone().to_serde_json_value();
        assert_eq!(resource_a_val, serde_json::json!({"const_evaluated": {"func": "evaluated!"}}));
    }

    // this test simply tests that errors work from function calls
    // but also confirms an invariant: that the function is truly called twice
    // for multiple consts passed into the function, and it fails
    // because the file already exists that it created in the first function call
    #[tokio::test]
    async fn can_eval_const_function_calls_multiple_err() {
        // delete the test case file to keep test idempotent:
        let _ = std::fs::remove_file("/tmp/testcase_const_function_calls_multiple_err.txt");
        let logger = VecLogger::leaked();
        log::set_max_level(log::LevelFilter::Trace);
        let document = r#"
const my_const
    { "a": "b" }

const my_const2
    { "a": "b" }

template some_template
  create
    echo hello

function javascript(my_func)
  const fs = require('fs');
  fs.writeFileSync('/tmp/testcase_const_function_calls_multiple_err.txt', 'e', {flag: 'wx'});
  return { "func": "evaluated!" }

resource some_template(resource_a)
  {
    "const_evaluated": $.my_func['my_const'],
    "const_evaluated": $.my_func['my_const2']
  }
"#.to_string();
        let mut dpl = parse_and_validate(&document).unwrap();
        let err = evaluate_constants(logger, &mut dpl).await.expect_err("it should fail due to the file being created multiple times");
        assert_eq!(err, "failed to process one or more const function calls");
        // check the logs:
        let logs = logger.get_logs();
        let mut existance_log_err = false;
        for log in logs.iter() {
            if log.contains("file already exists") { existance_log_err = true }
        }
        assert!(existance_log_err, "logs did not contain 'file already exists' {:?}", logs);
    }

    // important test that function calls are efficient.
    // if there are multiple function calls with the same constant value to the same function
    // those should only be processed once!
    // this test confirms this because if it calls the javascript function more than once, it would fail
    #[tokio::test]
    async fn can_eval_const_function_calls_multiple_efficient() {
        // delete the test case file to keep test idempotent:
        let _ = std::fs::remove_file("/tmp/testcase_const_function_calls_multiple_efficient.txt");
        let logger = VecLogger::leaked();
        log::set_max_level(log::LevelFilter::Trace);
        let document = r#"
const my_const
    { "a": "b" }

template some_template
  create
    echo hello

function javascript(my_func)
  const fs = require('fs');
  fs.writeFileSync('/tmp/testcase_const_function_calls_multiple_efficient.txt', 'e', {flag: 'wx'});
  return "a"

resource some_template(resource_a)
  {
    "const_evaluated": [
        $.my_func['my_const'],
        $.my_func['my_const'],
        $.my_func['my_const'],
        $.my_func['my_const'],
        $.my_func['my_const']
    ]
  }
"#.to_string();
        let mut dpl = parse_and_validate(&document).unwrap();
        evaluate_constants(logger, &mut dpl).await.expect("it should not have ran my_func multiple times");
        let resource_a = dpl.resources.iter().find(|x| x.resource_name.as_str() == "resource_a").unwrap();
        let resource_a_val = resource_a.input.clone().to_serde_json_value();
        // it should only have ran my_func once with input 'my_const' and then the value should simply
        // have been copied 5 times into the resource where the multiple function calls were defined
        assert_eq!(resource_a_val, serde_json::json!({"const_evaluated": ["a", "a", "a", "a", "a"]}));
    }

    #[tokio::test]
    async fn can_eval_const_inline_deep_and_multiple() {
        let logger = VecLogger::leaked();
        let document = r#"
const my_const
    { "a": ["b", {"c": false}] }

template some_template
  create
    echo hello

resource some_template(resource_a)
  {
    "const_evaluated1": $.my_const.a[0],
    "const_evaluated2": $.my_const.a[1].c
  }
"#.to_string();
        let mut dpl = parse_and_validate(&document).unwrap();
        evaluate_constants(logger, &mut dpl).await.expect("const evaluation should not fail");
        let resource_a = dpl.resources.iter().find(|x| x.resource_name.as_str() == "resource_a").unwrap();
        let resource_a_val = resource_a.input.clone().to_serde_json_value();
        assert_eq!(resource_a_val, serde_json::json!({"const_evaluated1": "b", "const_evaluated2": false}));
    }
}

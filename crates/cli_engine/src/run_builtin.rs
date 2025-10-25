use deploy_language::parse::template::Builtin;

use crate::run_template::CommandState;

pub fn run_builtin(
    rng: &mut fastrand::Rng,
    resource_name: &str,
    builtin: Builtin,
    cs: &CommandState,
) -> Result<serde_json::Value, String> {
    let val = match builtin {
        Builtin::Strcat { values, .. } => {
            let mut out = "".to_string();
            for val in values {
                // append the string to the output string, erroring if its an object, or array.
                // if its a json path, look it up from the command state, and append to the output
                // cs.evaluate_arg_transform(resource_name, jpq);
                let val = match val {
                    json_with_positions::Value::Null { .. } => serde_json::Value::Null,
                    json_with_positions::Value::Bool { val, .. } => serde_json::Value::Bool(val),
                    json_with_positions::Value::Number { val, .. } => {
                        let num = match val {
                            json_with_positions::Number::Float(f) => serde_json::Number::from_f64(f),
                            json_with_positions::Number::Int(i) => serde_json::Number::from_i128(i as _),
                        }.unwrap_or(serde_json::Number::from(0));
                        serde_json::Value::Number(num)
                    }
                    json_with_positions::Value::String { val, .. } => serde_json::Value::String(val.s),
                    json_with_positions::Value::JsonPath { val, .. } => {
                        let jpq = jsonpath_rust::parser::parse_json_path(&val.s)
                            .map_err(|e| format!("cannot evaluate /strcat builtin. failed to parse json path query '{}': {:?}", val.s, e))?;
                        let val = cs.evaluate_arg_transform(resource_name, &jpq)
                            .map_err(|e| format!("cannot evaluate /strcat builtin: failed to evaluate arg transform: {}", e))?;
                        val
                    }
                    _x => serde_json::Value::Object(Default::default()),
                };
                match val {
                    serde_json::Value::Null => {
                        out.push_str("null");
                    },
                    serde_json::Value::Bool(b) => {
                        if b {
                            out.push_str("true");
                        } else {
                            out.push_str("false");
                        }
                    }
                    serde_json::Value::Number(number) => {
                        let s = number.to_string();
                        out.push_str(&s);
                    }
                    serde_json::Value::String(s) => out.push_str(&s),
                    _ => return Err(format!("cannot evaluate /strcat builtin: one of the elements is a json object or json array. can only concatenate strings with strings,numbers,booleans, or evaluated json paths")),
                }
            }
            serde_json::Value::String(out)
        },
        Builtin::Random { len, .. } => {
            use std::iter::repeat_with;
            // TODO: support other random types dependning on r_type
            let s: String = repeat_with(|| rng.alphanumeric()).take(len).collect();
            serde_json::Value::String(s)
        }
    };
    Ok(val)
}

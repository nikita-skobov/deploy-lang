use log::Log;
use serde::{Deserialize, Serialize};
use tokio::process::Command;

const JAVASCRIPT_FN_TEMPLATE: &'static str = include_str!("./function_template.js");

#[derive(Deserialize, Serialize, Debug)]
pub struct Resource {
    pub name: String,
    pub input: serde_json::Value,
    pub output: serde_json::Value,
}

#[derive(Deserialize, Serialize, Debug)]
pub struct FunctionCall {
    pub function_name: String,
    pub function_type: String,
    pub function_body: String,
    pub depends_on: Resource,
}

pub async fn run_function(
    logger: &'static dyn Log,
    _resource_name: &str,
    input: serde_json::Value,
) -> Result<serde_json::Value, String> {
    let func_call: FunctionCall = serde_json::from_value(input)
        .map_err(|e| format!("failed to deserialize function input into a function call shape: {:?}", e))?;
    run_function_ex(logger, _resource_name, func_call).await
}

pub async fn run_function_ex(
    logger: &'static dyn Log,
    _resource_name: &str,
    func_call: FunctionCall
) -> Result<serde_json::Value, String> {
    log::info!(logger: logger, "calling function '{}({})'", func_call.function_name, func_call.depends_on.name);
    let script = fill_javascript_function_template(&func_call);
    let input_ser = serde_json::to_string(&func_call.depends_on.input)
        .map_err(|e| format!("failed to serialize input to function: {:?}", e))?;
    let output_ser = serde_json::to_string(&func_call.depends_on.output)
        .map_err(|e| format!("failed to serialize output to function: {:?}", e))?;
    
    // NO_COLOR=true node --eval 'console.log(JSON.stringify({}))'
    let cmd = Command::new("node")
        .arg("--eval")
        .arg(script)
        .env("NO_COLOR", "true")
        .env("RESOURCE_NAME", func_call.depends_on.name.clone())
        .env("RESOURCE_INPUT", input_ser)
        .env("RESOURCE_OUTPUT", output_ser)
        .output().await
        .map_err(|e| format!("failed to run function '{}({})': {:?}", func_call.function_name, func_call.depends_on.name, e))?;
    if !cmd.status.success() {
        let stderr = String::from_utf8_lossy(&cmd.stderr).to_string();
        return Err(format!("failed to run function '{}({})': {}", func_call.function_name, func_call.depends_on.name, stderr));
    }
    let val: serde_json::Value = serde_json::from_slice(&cmd.stdout)
        .map_err(|e| format!("failed to read output of function call '{}({})' as json: {:?}", func_call.function_name, func_call.depends_on.name, e))?;
    Ok(val)
}

pub fn fill_javascript_function_template(fn_call: &FunctionCall) -> String {
    let template = JAVASCRIPT_FN_TEMPLATE.to_string();
    // replace the name with the user's function name:
    let template = template.replace("USER_FUNCTION_NAME", &fn_call.function_name);
    let mut out = String::with_capacity(template.len() + fn_call.function_body.len() + 2);
    for (i, line) in template.lines().enumerate() {
        // all lines passed as-is, except the 2nd line, thats where
        // we insert the user's function body
        if i == 1 {
            out.push_str(&fn_call.function_body);
        } else {
            out.push_str(line);
        }
        out.push('\n');
    }
    out
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn js_fn_fill_works() {
        let fn_call = FunctionCall {
            function_name: "meefunc".to_string(),
            function_type: "javascript".to_string(),
            function_body: "return true".to_string(),
            depends_on: Resource {
                name: "r1".to_string(),
                input: serde_json::json!({}),
                output: serde_json::json!({}),
            }
        };
        let filled = fill_javascript_function_template(&fn_call);
        assert_eq!(filled, r#"function meefunc(input, output, name) {
return true
}
// user function gets inlined above ^


const out = meefunc(
    JSON.parse(process.env.RESOURCE_INPUT),
    JSON.parse(process.env.RESOURCE_OUTPUT),
    process.env.RESOURCE_NAME,
);
console.log(JSON.stringify(out))
"#);
    }
}
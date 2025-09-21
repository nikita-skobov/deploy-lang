//! declarative-cli supports providing resources where some values are calculated dynamically. for example:
//! ```json
//! {
//!     "resource": {
//!         "id": "123",
//!         "name": name,
//!     }
//! }
//! ```
//! this is not valid json, and would fail to deserialize into a rust object,
//! but we want it to, and we want the "name" field to be replaced with the value of `name`, which
//! is some variable.
//! this is done by parsing with serde multiple times. each time it gets an error it does the following:
//! - is the error a syntax error? if not, can exit and error: the user simply had a mistake in their JSON
//! - get the line/column position of the error. locate the string at the error position,
//!   is the first character an alphanumeric character? if not, can exit and error: the user
//!   had another syntax error such as using the wrong closing brace or something
//! - is the line/column position exactly the same as the last time this ran (if first time, ignore this)
//!   if it is, exit since we're not advancing, and we would otherwise get stuck repeatedly on the same error
//! - collect the string between the start of the error position, and the next comma
//! - is this word within the known data fields that can be replaced? if not, can error now:
//!   the user is referencing a value that doesnt exist in their data
//! - if the value is located, replace it into the JSON string, and repeat the process
//! 

use std::collections::HashMap;

use jsonpath_rust::JsonPath;
use serde_json::{Map, Number};

const PATH_QUERY_KEY: &str = "__DCL_PATH_QUERY_PRIVATE_FIELD_DO_NOT_USE__";

/// same as serde_json::Value but with an added variant: PathQuery.
/// this is used to capture arbitrary json with arbitrary PathQueries.
#[derive(PartialEq, Eq, Debug)]
pub enum DynamicValue {
    Null,
    Bool(bool),
    Number(Number),
    String(String),
    Array(Vec<DynamicValue>),
    Object(HashMap<String, DynamicValue>),
    /// represent a json path query as a string
    PathQuery(String),
}

/// used to represent a path query as an object temporarily
pub fn create_path_query_obj(path_query: String) -> HashMap<String, serde_json::Value> {
    let mut out = HashMap::new();
    out.insert(PATH_QUERY_KEY.to_string(), serde_json::Value::String(path_query));
    out
}

/// returns Some(path_query) if the value is an object
/// with only one key: `PATH_QUERY_KEY`.
/// returns None otherwise
pub fn get_path_query(m: &Map<String, serde_json::Value>) -> Option<String> {
    if m.len() != 1 {
        return None;
    }
    match m.get(PATH_QUERY_KEY) {
        Some(val) => return val.as_str().map(|x| x.to_string()),
        None => return None,
    }
}

pub fn value_to_dynamic_value(val: serde_json::Value) -> DynamicValue {
    match val {
        serde_json::Value::Null => DynamicValue::Null,
        serde_json::Value::Bool(b) => DynamicValue::Bool(b),
        serde_json::Value::Number(number) => DynamicValue::Number(number),
        serde_json::Value::String(s) => DynamicValue::String(s),
        serde_json::Value::Array(values) => {
            let mut out = Vec::with_capacity(values.len());
            for val in values {
                out.push(value_to_dynamic_value(val));
            }
            DynamicValue::Array(out)
        }
        serde_json::Value::Object(map) => {
            if let Some(path_query) = get_path_query(&map) {
                DynamicValue::PathQuery(path_query)
            } else {
                let mut out: HashMap<String, DynamicValue> = HashMap::with_capacity(map.len());
                for (key, val) in map {
                    out.insert(key, value_to_dynamic_value(val));
                }
                DynamicValue::Object(out)
            }
        }
    }
}

pub fn parse_to_dynamic_value<S: AsRef<str>>(
    user_input: S,
) -> Result<DynamicValue, String> {
    let mut json_str = user_input.as_ref().to_string();
    let mut last_position: Option<[usize; 2]> = None;
    loop {
        let err = match serde_json::from_str::<serde_json::Value>(&json_str) {
            Ok(o) => return Ok(value_to_dynamic_value(o)),
            Err(e) => e,
        };
        if !err.is_syntax() {
            return Err(format!("failed to parse json string: {:?}", err));
        }
        let line = err.line();
        let col = err.column();
        // account for serde json 1 based indexing:
        let line = if line == 0 { 0 } else { line - 1 };
        let col = if col == 0 { 0 } else { col - 1 };
        // // check if this is the same exact error:
        if let Some([last_line, last_col]) = last_position {
            if last_line == line && last_col == col {
                return Err(format!("failed to replace dynamic json: infinite loop detected from error: {:?}", err));
            }
        }
        // get the line where the error occurred:
        let mut json_str_lines: Vec<String> = json_str.lines().map(|x| x.to_string()).collect();
        let error_line = json_str_lines.get_mut(line)
            .ok_or(&format!("found json syntax error on line {} but user input has fewer lines", line))?;
        let error_line_len = error_line.len();
        let error_substr = error_line.get_mut(col..)
            .ok_or(&format!("found json syntax error on line {}, col {} but that line is only {} long", line, col, error_line_len))?;
        if !error_substr.starts_with("$.") && !error_substr.starts_with("$[") {
            // can error since its not a valid json path query
            return Err(format!("failed to parse json string: {:?}", err));
        }
        let json_path_query = extract_json_path_query(&error_substr)?;
        let replace_range = col..col+json_path_query.len();
        let replace_with = create_path_query_obj(json_path_query);
        let replace_str = serde_json::to_string(&replace_with)
            .map_err(|e| format!("failed to serialize json path query replacement object: {:?}", e))?;
        error_line.replace_range(replace_range, &replace_str);
        // now that we successfully replaced the word with its json value,
        // put the string back together and try again:
        json_str = json_str_lines.join("\n");
        last_position = Some([line, col]);
    }
}

pub fn evaluate_json_path_recursively(dynamic_json_val: DynamicValue, known_values: &serde_json::Value) -> Result<serde_json::Value, String> {
    match dynamic_json_val {
        DynamicValue::Null => Ok(serde_json::Value::Null),
        DynamicValue::Bool(b) => Ok(serde_json::Value::Bool(b)),
        DynamicValue::Number(number) => Ok(serde_json::Value::Number(number)),
        DynamicValue::String(s) => Ok(serde_json::Value::String(s)),
        DynamicValue::Array(dynamic_values) => {
            let mut out = Vec::with_capacity(dynamic_values.len());
            for val in dynamic_values {
                out.push(evaluate_json_path_recursively(val, known_values)?);
            }
            Ok(serde_json::Value::Array(out))
        }
        DynamicValue::Object(hash_map) => {
            let mut out = Map::with_capacity(hash_map.len());
            for (key, val) in hash_map {
                out.insert(key, evaluate_json_path_recursively(val, known_values)?);
            }
            Ok(serde_json::Value::Object(out))
        }
        DynamicValue::PathQuery(json_query) => {
            let mut values = known_values.query(&json_query)
                .map_err(|e| format!("failed to lookup json path query '{}': {:?}", json_query, e))?;
            // TODO: needs to be more robust
            // what about cases where the user indeed wanted an array of 1 item?
            if values.len() == 1 {
                // assume that query was for a single value, try to extract the value:
                let val = unsafe { *values.get_unchecked(0) };
                return Ok(val.clone());
            }
            Ok(serde_json::Value::Array(values.drain(..).map(|x| x.clone()).collect()))
        }
    }
}

/// repeatedly parse the user input into a serde json Value object,
/// checking if there was a syntax error, and attempting to replace the syntax error with
/// the value from `known_values`. example:
/// ```
/// use dcl_language::dynamic_json::replace_dynamic_json;
/// let values = serde_json::json!({"world": "you are my world"});
/// let out = replace_dynamic_json(r#"{"hello": world}"#, &values).unwrap();
/// assert_eq!(out["hello"], "you are my world");
/// ```
pub fn replace_dynamic_json<S: AsRef<str>>(
    user_input: S,
    known_values: &serde_json::Value,
) -> Result<serde_json::Value, String> {
    let dynamic_json_val = parse_to_dynamic_value(user_input)?;
    let val = evaluate_json_path_recursively(dynamic_json_val, known_values)?;
    Ok(val)
}

/// given a substring of an expected json path query, construct a string
/// containing the json path query, stopping once detecting the remainder of the json document
/// (commas, brace, end of line, etc)
fn extract_json_path_query(error_substr: &str) -> Result<String, String> {    
    // estimate max size of a json path query to avoid many repeated allocations
    // for small-medium size words:
    let mut out = String::with_capacity(60);
    for char in error_substr.chars() {
        // TODO: this needs to be more robust...
        if char == ',' || char == '}' || char.is_ascii_whitespace() {
            break;
        }
        out.push(char);
    }
    if out.is_empty() {
        return Err(format!("failed to locate word to replace from error line: '{}'", error_substr));
    }
    Ok(out)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn basic_json_path_replace_works() {
        let input = r#"{"hello": $.world }"#;
        let value = parse_to_dynamic_value(input).expect("it should parse");
        match value {
            DynamicValue::Object(hash_map) => {
                assert_eq!(hash_map["hello"], DynamicValue::PathQuery("$.world".to_string()));
            }
            _ => panic!("wrong type"),
        }
    }

    #[test]
    fn json_path_conversion_works() {
        let input = r#"{"hello": $.world }"#;
        let known_values = serde_json::json!({ "world": "you are my world" });
        let value = replace_dynamic_json(input, &known_values).expect("it should replace");
        assert_eq!(value, serde_json::json!({"hello": "you are my world"}));
    }

    #[test]
    fn basic_replace_works() {
        let values = serde_json::json!({"world": "you are my world"});
        let cases = [
            r#"{"hello": $.world}"#,
            r#"{"hello": $.world }"#,
            r#"{"hello":$.world }"#,
            r#"{"hello":$.world}"#,
            r#"{
            "hello": $.world
            }"#,
            r#"{
            "hello": $.world}"#,
            r#"{
            "hello": $.world,
            "other": "thing"
            }"#,
            r#"{
            "hello": $.world ,
            "other": "thing"
            }"#,
        ];
        for case in cases {
            let out = replace_dynamic_json(case, &values).unwrap();
            assert_eq!(out["hello"], "you are my world");
        }
    }

    #[test]
    fn can_replace_multiple() {
        let values = serde_json::json!({
            "a": "1",
            "b": "2",
            "c": "3",
        });
        let input = r#"
        {
            "should1": $.a,
            "should2": $.b, "should3": $.c
        }
        "#;
        let out = replace_dynamic_json(input, &values).unwrap();
        assert_eq!(out["should1"], "1");
        assert_eq!(out["should2"], "2");
        assert_eq!(out["should3"], "3");
    }

    #[test]
    fn can_insert_complex_objects() {
        let values = serde_json::json!({
            "a_obj": {
                "hello": ["1", 2, {"b":"b"}, false],
            }
        });
        let input = r#"
        {
            "some_obj": $.a_obj,
            "should_be_two": $.a_obj.hello[1]
        }
        "#;
        let out = replace_dynamic_json(input, &values).unwrap();
        let expected = serde_json::json!({
            "hello": ["1", 2, {"b":"b"}, false]
        });
        assert_eq!(out["some_obj"], expected);
        assert_eq!(out["should_be_two"], serde_json::json!(2));
    }
}
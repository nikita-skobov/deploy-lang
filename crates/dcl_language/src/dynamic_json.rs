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
    let mut json_str = user_input.as_ref().to_string();
    let mut last_position: Option<[usize; 2]> = None;
    loop {
        let err = match serde_json::from_str(&json_str) {
            Ok(o) => return Ok(o),
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
        // check if this is the same exact error:
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
        if !error_substr.starts_with(char::is_alphanumeric) {
            // can error since its not a valid replace word
            return Err(format!("failed to parse json string: {:?}", err));
        }
        let replace_word = extract_word_to_replace(&error_substr)?;
        let replace_range = col..col+replace_word.len();
        match known_values.get(&replace_word) {
            Some(val) => {
                // found the value to replace
                let val_str = serde_json::to_string(val)
                    .map_err(|e| format!("failed to substitute dynamic json value for '{}'. serialization error: {:?}",
                        replace_word,
                        e,
                    ))?;
                error_line.replace_range(replace_range, &val_str);
            }
            None => {
                return Err(format!(
                    "attempted to substitute dynamic json value for '{}' but this does not exist in the known values",
                    replace_word,
                ));
            }
        }
        // now that we successfully replaced the word with its json value,
        // put the string back together and try again:
        json_str = json_str_lines.join("\n");
        last_position = Some([line, col]);
    }
}

fn extract_word_to_replace(error_line: &str) -> Result<String, String> {
    // estimate max size of a replace word to avoid many repeated allocations
    // for small-medium size words:
    let mut out = String::with_capacity(60);
    for char in error_line.chars() {
        if char == ',' || char.is_whitespace() || (!char.is_alphanumeric() && char != '_') {
            break;
        }
        out.push(char);
    }
    if out.is_empty() {
        return Err(format!("failed to locate word to replace from error line: '{}'", error_line));
    }
    Ok(out)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn basic_replace_works() {
        let values = serde_json::json!({"world": "you are my world"});
        let cases = [
            r#"{"hello": world}"#,
            r#"{"hello": world }"#,
            r#"{"hello":world }"#,
            r#"{"hello":world}"#,
            r#"{
            "hello": world
            }"#,
            r#"{
            "hello": world}"#,
            r#"{
            "hello": world,
            "other": "thing"
            }"#,
            r#"{
            "hello": world ,
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
            "should1": a,
            "should2": b, "should3": c
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
            "some_obj": a_obj
        }
        "#;
        let out = replace_dynamic_json(input, &values).unwrap();
        let expected = serde_json::json!({
            "hello": ["1", 2, {"b":"b"}, false]
        });
        assert_eq!(out["some_obj"], expected);
    }
}
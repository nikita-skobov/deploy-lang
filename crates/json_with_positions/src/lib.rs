

#[cfg(test)]
mod test {
    use std::path::PathBuf;

    /// runs through all the test file cases in JSONTestSuite
    /// ignoring any of the files that start with i_ and
    /// loads them as a string (ignoring any invalid utf8 strings)
    /// and then calls the provided function with the json test case
    /// and the function returns Ok() if parsing succeeds, or an error otherwise.
    /// this function then panics if the expected result doesnt match the parsing result
    fn test_all_json_cases(f: fn(String) -> Result<(), String>) {
        let mut root_workspace_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        // out 1 level and into the crates directory:
        root_workspace_path.pop();
        // out 1 level into the root of the workspace
        root_workspace_path.pop();
        // out of the root workspace:
        root_workspace_path.pop();
        // into the repository with the test cases:
        root_workspace_path.push("JSONTestSuite");
        // directory with the test cases:
        root_workspace_path.push("test_parsing");
        let rd = std::fs::read_dir(&root_workspace_path).expect("failed to read test case directory");
        for de in rd {
            let dir_entry = de.expect("failed to read dir entry");
            let file_name = dir_entry.file_name().to_string_lossy().to_string();
            let first_char = file_name.chars().nth(0).expect("it should have 1 char");
            if first_char != 'y' && first_char != 'n' {
                // ignore files that start with i_
                // they are ambiguous, i dont care about that for now
                continue;
            }
            if !file_name.ends_with(".json") {
                continue;
            }
            let contents = std::fs::read(dir_entry.path()).expect("failed to read test case contents");
            let contents_utf8 = match String::from_utf8(contents) {
                Ok(o) => o,
                Err(_) => {
                    println!("SKIP {} not valid utf8", file_name);
                    continue;
                }
            };
            let json_value = f(contents_utf8);
            match json_value {
                Ok(_) => {
                    if first_char == 'y' {
                        println!("OK {}", file_name);
                    } else {
                        panic!("ER {} expected failure but it parsed", file_name);
                    }
                }
                Err(e) => {
                    if first_char == 'n' {
                        println!("OK {}", file_name);
                    } else {
                        panic!("ER {} expected parse but it failed. parse error: {}", file_name, e);
                    }
                }
            }
        }
    }

    fn parse_json_serde(json_str: String) -> Result<(), String> {
        let _ = serde_json::from_str::<serde_json::Value>(&json_str).map_err(|e| e.to_string())?;
        Ok(())
    }

    #[test]
    #[ignore = "this test assumes you have the minefield test case repository cloned already and is adjacent to the root workspace"]
    fn run_all_minefield_tests_serde() {
        test_all_json_cases(parse_json_serde);
    }
}

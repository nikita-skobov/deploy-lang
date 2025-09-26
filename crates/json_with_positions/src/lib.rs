#[cfg(test)]
mod test {
    use std::path::PathBuf;

    #[test]
    #[ignore = "this test assumes you have the minefield test case repository cloned already and is adjacent to the root workspace"]
    fn run_all_minefield_tests() {
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
            let json_value = serde_json::from_slice::<serde_json::Value>(&contents);
            match json_value {
                Ok(_) => {
                    if first_char == 'y' {
                        println!("OK {}", file_name);
                    } else {
                        println!("ER {} expected failure but it parsed", file_name);
                    }
                }
                Err(_) => {
                    if first_char == 'n' {
                        println!("OK {}", file_name);
                    } else {
                        println!("ER {} expected parse but it failed", file_name);
                    }
                }
            }
        }
    }
}

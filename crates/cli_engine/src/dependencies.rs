//! helper module to deal with dependency ordering

use std::collections::{HashMap, HashSet};

use deploy_language::{parse::{resource::ResourceSection}};

use crate::{get_resource_name_from_segment, ResourceInState, StateFile, TrWithTemplate, TransitionableResource};


pub fn get_immediate_deps_from_current_entry(current_entry: &ResourceSection) -> Result<Vec<String>, String> {
    // TODO: parse and extract just the first segment
    let all_json_paths = current_entry.input.get_all_json_paths();
    let num_json_paths = all_json_paths.len();
    let json_paths = all_json_paths
        .iter()
        // we ignore errors here, only returning the successfully parsed json paths
        // because that validation should have happened already by the deploy_language crate
        .filter_map(|x| jsonpath_rust::parser::parse_json_path(&x.s).ok());
    let mut out = Vec::with_capacity(num_json_paths);
    for jpq in json_paths {
        let resource_name = jpq.segments.first().ok_or_else(|| {
            format!("json path for resource '{}' does not reference any resource", current_entry.resource_name)
        })?;
        let tr_name = current_entry.resource_name.as_str();
        let resource_name = get_resource_name_from_segment(tr_name, resource_name)?;
        out.push(resource_name);
    }
    Ok(out)
}

/// returns a list of dependencies that are immediate: this current resource depends on resource A, B, C, ...
/// dependencies are other resource names from json paths that are referenced by this current resource in its input json
pub fn get_immediate_dependencies(current: &TrWithTemplate) -> Result<Vec<String>, String> {
    let mut immediate_deps = match &current.tr {
        TransitionableResource::Create { current_entry } |
        TransitionableResource::Update { current_entry, .. } => {
            get_immediate_deps_from_current_entry(current_entry)?
        }
        TransitionableResource::Delete { state_entry } => state_entry.depends_on.clone(),
    };
    immediate_deps.dedup();
    Ok(immediate_deps)
}

pub fn get_all_transient_dependencies_map(
    trs: &[TrWithTemplate],
    done_resources: &HashMap<String, ResourceInState>,
) -> Result<HashMap<String, Vec<String>>, String> {
    // first, collect a map of all immediate dependencies:
    let mut immediate_dep_map = HashMap::with_capacity(trs.len());
    for tr in trs {
        let immediate_deps = get_immediate_dependencies(tr)?;
        immediate_dep_map.insert(tr.tr.get_resource_name().to_string(), immediate_deps);
    }
    // ensure to also add all of the done resources, otherwise some lookups will fail
    // where a tr depends on a resource thats already done
    for resource in done_resources.values() {
        immediate_dep_map.insert(resource.resource_name.clone(), resource.depends_on.clone());
    }
    let mut transient_dep_map = HashMap::with_capacity(immediate_dep_map.len());
    // next, for each Tr, get all of its transient dependencies by building a vec
    // recursively by looking up its deps in the map
    for (tr_name, immediate_deps) in immediate_dep_map.iter() {
        let mut visited = Vec::with_capacity(immediate_deps.len() + 1);
        let mut all_transient_deps = Vec::with_capacity(immediate_deps.len());
        visited.push(tr_name);
        collect_all_transient_deps(&tr_name, &mut visited, &mut all_transient_deps, &immediate_dep_map)?;
        transient_dep_map.insert(tr_name.clone(), all_transient_deps);
    }
    Ok(transient_dep_map)
}

/// returns a map of resource names to a Vec of resources that must be deleted prior to
/// the key being deleted. eg:
/// `{"A": [], "B": [A]}`
/// implies A can be deleted now, and B must wait for A to be deleted before B is to be deleted
pub fn get_delete_order_map(trs: &[TrWithTemplate]) -> Result<HashMap<String, Vec<String>>, String> {
    // first collect a set of all resource names that are to be deleted:
    let delete_resources_set: HashSet<_> = trs.iter().map(|tr| tr.tr.get_resource_name()).collect();
    let mut out = HashMap::with_capacity(trs.len());
    for tr in trs {
        let mut immediate_deps = get_immediate_dependencies(tr)?;
        // for the immediate deps of this resource, remove any that are not in
        // the batch of resources to be deleted:
        immediate_deps.retain(|d| delete_resources_set.contains(d.as_str()));
        out.insert(tr.tr.get_resource_name().to_string(), immediate_deps);
    }
    // now that we have a dependency order map, invert it
    // so go from A depends on B, B depends on nothing
    // { A: [B], B: []}
    // to B must wait for A to be deleted, A does not need to wait for anything to be deleted
    // { A: [], B: [A]}
    let mut inverted: HashMap<String, Vec<String>> = HashMap::with_capacity(delete_resources_set.len());
    for key in out.keys() {
        inverted.insert(key.to_string(), vec![]);
    }
    for (key, deps) in out.drain() {
        for dep in deps {
            if let Some(existing) = inverted.get_mut(&dep) {
                existing.push(key.clone());
                existing.sort();
                existing.dedup();
            } else {
                inverted.insert(dep.clone(), vec![key.clone()]);
            }
        }
    }

    Ok(inverted)
}

pub fn collect_all_transient_deps<'a>(
    lookup: &str,
    visited: &mut Vec<&'a String>,
    collected: &mut Vec<String>,
    immediate_dep_map: &'a HashMap<String, Vec<String>>
) -> Result<(), String> {
    let immediate_deps = immediate_dep_map.get(lookup).ok_or_else(|| {
        format!("resource '{}' was not found in immediate dep map", lookup)
    })?;
    for dep in immediate_deps {
        if dep == lookup {
            return Err(format!("resource '{}' cannot depend on itself", lookup));
        }
        if let Some((i, dep_name)) = visited.iter().enumerate().find(|x| *x.1 == dep) {
            if i == 0 {
                // the first visited item must be the one that started the search
                // if dep == the start of the search then there's a circular dependency
                return Err(format!("circular dependency detected: resource '{}' depends transiently on '{}' which depends on '{}'", dep_name, lookup, dep));
            }
            // otherwise, it's simply a dependency that we've already visited. if we've already visited, then
            // we must not recurse
            continue;
        }
        collected.push(dep.clone());
        visited.push(dep);
        collect_all_transient_deps(dep, visited, collected, immediate_dep_map)?;
    }
    collected.sort();
    collected.dedup();
    Ok(())
}

pub fn get_all_transient_dependencies(
    done_resources: &HashMap<String, ResourceInState>,
    current: &TrWithTemplate,
    trs: &[TrWithTemplate],
) -> Result<Vec<String>, String> {
    let mut map = get_all_transient_dependencies_map(trs, done_resources)?;
    map.remove(current.tr.get_resource_name()).ok_or_else(|| format!("resource '{}' not found in map of all resources", current.tr.get_resource_name()))
}

/// a TR can be transitioned iff all of its dependencies are done
pub fn can_tr_be_transitioned(tr: &TrWithTemplate, dep_map: &HashMap<String, Vec<String>>, state: &StateFile) -> bool {
    let no_deps = vec![];
    let dep_list = dep_map.get(tr.tr.get_resource_name()).unwrap_or(&no_deps);
    dep_list.iter().all(|dep| state.resources.contains_key(dep))
}

#[cfg(test)]
mod test {
    use deploy_language::{parse::template::{ArgTransform, CliCommand, CliCommandWithDirectives, TemplateSection}, DplFile};

    use crate::{perform_update, test_log::VecLogger};

    use super::*;

    macro_rules! create_tr {
        ($name: literal; $input: literal) => {
            TrWithTemplate {
                tr: TransitionableResource::Create { current_entry: ResourceSection {
                    resource_name: $name.into(),
                    template_name: "t".into(),
                    input: json_with_positions::parse_json_value($input).unwrap(),
                    end_line: 0,
                } },
                template: Default::default()
            }
        };
    }

    #[test]
    fn can_get_immediate_dependencies_for_create() {
        let current = TrWithTemplate {
            tr: TransitionableResource::Create {
                current_entry: ResourceSection {
                    resource_name: "a".into(),
                    template_name: "template".into(),
                    input: json_with_positions::parse_json_value(r#"{
                        "thing1": $.resourceB.output.name,
                        "thing2": $.resourceC.input.something
                    }"#).unwrap(),
                    end_line: 0,
                },
            },
            template: TemplateSection::default(),
        };
        let mut deps = get_immediate_dependencies(&current).expect("it shouldnt error");
        deps.sort();
        assert_eq!(deps.len(), 2);
        assert_eq!(deps[0], "resourceB");
        assert_eq!(deps[1], "resourceC");
    }

    #[test]
    fn can_get_immediate_dependencies_bracketed() {
        let current = TrWithTemplate {
            tr: TransitionableResource::Create {
                current_entry: ResourceSection {
                    resource_name: "a".into(),
                    template_name: "template".into(),
                    input: json_with_positions::parse_json_value(r#"{
                        "thing1": $['resourceB'].output.name,
                        "thing2": $["resourceC"].input.something
                    }"#).unwrap(),
                    end_line: 0,
                },
            },
            template: TemplateSection::default(),
        };
        let mut deps = get_immediate_dependencies(&current).expect("it shouldnt error");
        deps.sort();
        assert_eq!(deps.len(), 2);
        assert_eq!(deps[0], "resourceB");
        assert_eq!(deps[1], "resourceC");
    }

    #[test]
    fn get_immediate_dependencies_should_err_for_non_segment_selectors() {
        let current = TrWithTemplate {
            tr: TransitionableResource::Create {
                current_entry: ResourceSection {
                    resource_name: "a".into(),
                    template_name: "template".into(),
                    input: json_with_positions::parse_json_value(r#"{
                        "thing1": $[0].output.name
                    }"#).unwrap(),
                    end_line: 0,
                },
            },
            template: TemplateSection::default(),
        };
        let err = get_immediate_dependencies(&current).expect_err("it should error");
        assert_eq!(err, "json path for resource 'a' must start with a segment selector that references another resource. instead found '0'");
        
        let current = TrWithTemplate {
            tr: TransitionableResource::Create {
                current_entry: ResourceSection {
                    resource_name: "a".into(),
                    template_name: "template".into(),
                    input: json_with_positions::parse_json_value(r#"{
                        "thing1": $..output.name
                    }"#).unwrap(),
                    end_line: 0,
                },
            },
            template: TemplateSection::default(),
        };
        let err = get_immediate_dependencies(&current).expect_err("it should error");
        assert_eq!(err, "json path for resource 'a' must start with a segment selector that references another resource. instead found '..output'");
    }

    #[test]
    fn can_get_immediate_dependencies_for_update() {
        let current = TrWithTemplate {
            tr: TransitionableResource::Update {
                current_entry: ResourceSection {
                    resource_name: "a".into(),
                    template_name: "template".into(),
                    input: json_with_positions::parse_json_value(r#"{
                        "thing1": $.resourceB.output.name,
                        "thing2": $.resourceC.input.something
                    }"#).unwrap(),
                    end_line: 0,
                },
                state_entry: ResourceInState {
                    // these are deps from the last time this resource was ran
                    // these shouldnt be returned.. get_immediate_dependencies should
                    // return the deps from the current entry
                    depends_on: vec!["a".to_string(), "b".to_string()],
                    ..Default::default()
                }
            },
            template: TemplateSection::default(),
        };
        let mut deps = get_immediate_dependencies(&current).expect("it shouldnt error");
        deps.sort();
        assert_eq!(deps.len(), 2);
        assert_eq!(deps[0], "resourceB");
        assert_eq!(deps[1], "resourceC");
    }

    #[test]
    fn can_get_immediate_dependencies_for_delete() {
        let current = TrWithTemplate {
            tr: TransitionableResource::Delete {
                state_entry: ResourceInState {
                    // a deleteable resource's immediate deps are from state as-is:
                    depends_on: vec!["resourceB".to_string(), "resourceC".to_string()],
                    ..Default::default()
                }
            },
            template: TemplateSection::default(),
        };
        let mut deps = get_immediate_dependencies(&current).expect("it shouldnt error");
        deps.sort();
        assert_eq!(deps.len(), 2);
        assert_eq!(deps[0], "resourceB");
        assert_eq!(deps[1], "resourceC");
    }

    #[test]
    fn all_transient_deps_works() {
        let mut immediate_dep_map = HashMap::new();
        immediate_dep_map.insert("A".to_string(), vec!["B".to_string(), "C".to_string()]);
        immediate_dep_map.insert("B".to_string(), vec!["C".to_string()]);
        immediate_dep_map.insert("C".to_string(), vec!["D".to_string()]);
        immediate_dep_map.insert("D".to_string(), vec![]);
        let a = "A".to_string();
        let mut visited = vec![&a];
        let mut collected = vec![];
        collect_all_transient_deps("A", &mut visited, &mut collected, &immediate_dep_map).unwrap();
        assert_eq!(collected, vec!["B", "C", "D"]);
    }

    #[test]
    fn all_transient_can_detect_circular_dependency() {
        let mut immediate_dep_map = HashMap::new();
        immediate_dep_map.insert("A".to_string(), vec!["B".to_string(), "C".to_string()]);
        immediate_dep_map.insert("B".to_string(), vec!["C".to_string()]);
        immediate_dep_map.insert("C".to_string(), vec!["A".to_string()]);
        let a = "A".to_string();
        let mut visited = vec![&a];
        let mut collected = vec![];
        let err = collect_all_transient_deps("A", &mut visited, &mut collected, &immediate_dep_map).expect_err("it should error");
        assert_eq!(err, "circular dependency detected: resource 'A' depends transiently on 'C' which depends on 'A'");
    }

    #[test]
    fn can_get_all_transient_deps_duplicate_ok() {
        let done_resources = HashMap::new();
        let a = create_tr!("a"; r#"{"1": $.b, "2": $.c}"#);
        let b = create_tr!("b"; r#"{"1": $.c}"#);
        let c = create_tr!("c"; r#"{}"#);
        let trs = [a, b, c];
        let current = &trs[0];
        // A depends on B and C
        // B depends on C
        // C depends on nothing.
        // this should be valid, and all transient deps of A should be B and C
        let mut deps = get_all_transient_dependencies(&done_resources, current, &trs).expect("it shouldnt error");
        deps.sort();
        assert_eq!(deps, ["b", "c"]);
    }

    #[test]
    fn can_get_all_transient_deps_simple_ok() {
        let done_resources = HashMap::new();
        let a = create_tr!("a"; r#"{"1": $.b}"#);
        let b = create_tr!("b"; r#"{"1": $.c}"#);
        let c = create_tr!("c"; r#"{}"#);
        let trs = [a, b, c];
        let current = &trs[0];
        // A depends on B
        // B depends on C
        // C depends on nothing.
        // this should be valid, and all transient deps of A should be B and C
        let mut deps = get_all_transient_dependencies(&done_resources, current, &trs).expect("it shouldnt error");
        deps.sort();
        assert_eq!(deps, ["b", "c"]);
    }

    #[test]
    fn transient_deps_checks_for_self_dep_err() {
        let done_resources = HashMap::new();
        let a = create_tr!("a"; r#"{"1": $.a}"#);
        let trs = [a];
        let current = &trs[0];
        // A depends on A which should be an error
        let err = get_all_transient_dependencies(&done_resources, current, &trs).expect_err("it should error");
        assert_eq!(err, "resource 'a' cannot depend on itself");
    }

    #[test]
    fn transient_deps_checks_for_self_dep_err_deep() {
        let done_resources = HashMap::new();
        let a = create_tr!("a"; r#"{"1": $.b}"#);
        let b = create_tr!("b"; r#"{"1": $.c}"#);
        let c = create_tr!("c"; r#"{"1": $.c}"#);
        let trs = [a, b, c];
        let current = &trs[0];
        // C depends on C which should be an error
        let err = get_all_transient_dependencies(&done_resources, current, &trs).expect_err("it should error");
        assert_eq!(err, "resource 'c' cannot depend on itself");
    }

    #[test]
    fn can_get_all_transient_deps_deep_circle() {
        let done_resources = HashMap::new();
        let a = create_tr!("a"; r#"{"1": $.b}"#);
        let b = create_tr!("b"; r#"{"1": $.c}"#);
        let c = create_tr!("c"; r#"{"1": $.d}"#);
        let d = create_tr!("d"; r#"{"1": $.a}"#);
        let trs = [a, b, c, d];
        let current = &trs[0];
        // A depends on B
        // B depends on C
        // C depends on D
        // D depends on A
        // this should be invalid because A transiently depends on D which depends on A.
        let err = get_all_transient_dependencies(&done_resources, current, &trs).expect_err("it should error");
        assert!(err.starts_with("circular dependency detected"));
    }

    #[test]
    fn can_get_all_transient_deps_with_some_being_done() {
        let mut done_resources = HashMap::new();
        let a = create_tr!("a"; r#"{"1": $.b}"#);
        done_resources.insert("d".to_string(), ResourceInState {
            resource_name: "d".to_string(),
            ..Default::default()
        });
        done_resources.insert("c".to_string(), ResourceInState {
            resource_name: "c".to_string(),
            depends_on: vec!["d".to_string()],
            ..Default::default()
        });
        done_resources.insert("b".to_string(), ResourceInState {
            resource_name: "b".to_string(),
            depends_on: vec!["c".to_string()],
            ..Default::default()
        });
        let trs = [a];
        let current = &trs[0];
        // A depends on B
        // B depends on C
        // C depends on D
        // D depends on nothing
        // A is the only resource thats being transitioned
        // all of the other ones are done (no update necessary since their input hasnt changed)
        // we should still successfully get the transient deps of A to be [B, C, D]
        let mut deps = get_all_transient_dependencies(&done_resources, current, &trs).expect("it shouldnt error");
        deps.sort();
        assert_eq!(deps, ["b", "c", "d"]);
    }

    #[tokio::test]
    async fn perform_update_resources_dependencies_processed_in_order() {
        let logger = VecLogger::leaked();
        log::set_max_level(log::LevelFilter::Trace);
        let mut dpl = DplFile::default();
        let state = StateFile::default();
        dpl.resources.push(ResourceSection {
            resource_name: "A".into(),
            template_name: "template".into(),
            input: json_with_positions::parse_json_value(r#"{"this": "will be echoed"}"#).unwrap(),
            end_line: 0,
        });
        dpl.resources.push(ResourceSection {
            resource_name: "B".into(),
            template_name: "template".into(),
            input: json_with_positions::parse_json_value(r#"{"resourceA": $.A.output}"#).unwrap(),
            end_line: 0,
        });
        let mut template = TemplateSection::default();
        template.template_name.s = "template".to_string();
        let mut cli_cmd = CliCommand::default();
        cli_cmd.command.s = "echo".to_string();
        cli_cmd.arg_transforms.push(ArgTransform::destructure(jsonpath_rust::parser::parse_json_path("$.input").unwrap()));
        template.create.cli_commands.push(CliCommandWithDirectives { cmd: cli_cmd.into(), ..Default::default() });
        dpl.templates.push(template);
        let mut out_state = perform_update(logger, dpl, state).await.expect("it should not error");
        assert_eq!(out_state.resources.len(), 2);
        let a_resource = out_state.resources.remove("A").expect("it should have an output resource 'A'");
        assert_eq!(a_resource.depends_on.len(), 0);
        assert_eq!(a_resource.last_input, serde_json::json!({"this": "will be echoed"}));
        assert_eq!(a_resource.template_name, "template");
        assert_eq!(a_resource.output.as_str().unwrap(), "--this will be echoed\n");
        let b_resource = out_state.resources.remove("B").expect("it should have an output resource 'B'");
        assert_eq!(b_resource.depends_on, vec!["A"]);
        assert_eq!(b_resource.output.as_str().unwrap(), "--resourceA --this will be echoed\n\n");
        let logs = logger.get_logs();
        assert_eq!(logs, vec!["creating 'A'", "resource 'A' OK", "creating 'B'", "resource 'B' OK"]);
    }
}

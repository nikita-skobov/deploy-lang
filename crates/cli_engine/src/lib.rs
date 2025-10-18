use std::collections::{HashMap, HashSet};

use deploy_language::{parse::{resource::ResourceSection, template::TemplateSection, SpannedDiagnostic}, DplFile};
use json_with_positions::Position;
use jsonpath_rust::{parser::model::JpQuery, query::{js_path_process, state::State, Query}};
use serde::{Deserialize, Serialize};
use str_at_line::StringAtLine;

pub mod run_template;
pub mod run_function;

/// during a deploy, if a resource references a function, we actually create a separate ephemeral resource
/// for that function call, and it will have this prefix such that we dont accidentally
/// add it to state afterwards, and such that users cant have conflicting resource names
pub const FUNCTION_RESOURCE_PREFIX: &str = "__priv_internal_function_resource__";

#[derive(Serialize, Deserialize, Default, Debug)]
pub struct StateFile {
    /// reserved root field for later metadata, currently unused and values will show up as null
    #[serde(default)]
    pub dpl_metadata: serde_json::Value,
    /// map of resources and their state representation.
    /// note the key is the resource name, thus must be unique across all resources.
    /// also note the ResourceInState also contains the resource name for convenience
    pub resources: HashMap<String, ResourceInState>,
}

pub fn save_state(state_path: &str, state: &StateFile) -> Result<(), String> {
    let serialized = serde_json::to_string_pretty(state).map_err(|e| format!("Failed to serialize state to json: {:?}", e))?;
    std::fs::write(state_path, serialized)
        .map_err(|e| format!("failed to save state back to file '{}': {:?}", state_path, e))
}

// TODO: need tp update DplFile definitions to preserve section line positions.
// current reporting all errors on line 0 :/
/// loads the state file and parses as a json object. if the state file doesnt exist, it will be
/// created, and treated as an empty {} object
pub fn load_state(dpl: &DplFile) -> Result<(StateFile, String), SpannedDiagnostic> {
    let state = dpl.state.as_ref().ok_or("no state file provided")
        .map_err(|e| SpannedDiagnostic::new(e, 0, 999))?;
    let state_file = state.file.clone();
    let file = match std::fs::OpenOptions::new().read(true).open(&state.file) {
        Ok(o) => o,
        Err(e) => match e.kind() {
            std::io::ErrorKind::NotFound => {
                let default_statefile = StateFile::default();
                let serialized = serde_json::to_string_pretty(&default_statefile).map_err(|e| {
                    SpannedDiagnostic::new(format!("failed to serialize empty state file: {:?}", e), 0, 999)
                })?;
                // create a new empty state file:
                std::fs::write(&state.file, serialized)
                    .map_err(|e| SpannedDiagnostic::new(
                        format!("Failed to create empty state file '{}': {:?}", state.file, e),
                        0, 999)
                    )?;
                return Ok((default_statefile, state_file));
            },
            e => {
                return Err(SpannedDiagnostic::new(
                    format!("Failed to read state file '{}': {:?}", state.file, e),
                    0, 999
                ));
            }
        }
    };
    let buf = std::io::BufReader::new(file);
    let out: StateFile = serde_json::from_reader(buf)
        .map_err(|e| format!("Failed to read state file '{}' as json: {:?}", state.file, e))
        .map_err(|e| SpannedDiagnostic::new(e, 0, 999))?;
    Ok((out, state_file))
}

/// represents a resource that has successfully been created/updated in a state file
#[derive(Deserialize, Serialize, Debug, Default, Clone)]
pub struct ResourceInState {
    /// name of the resource. unique across all resources
    pub resource_name: String,
    /// name of the template this resource references. used to lookup
    /// the template in case a deletion is necessary
    pub template_name: String,
    /// the entire input of this resource at the time
    /// that it was passed to its template (ie: it does not have any json paths, all
    /// json paths have been evaluated already and thus it is representable as a serde_json Value
    /// instead of a json_with_positions Value). this is used
    /// to compare against it's current input to see which fields, if any, have changed
    pub last_input: serde_json::Value,
    /// the entire output of this resource after it was done being processed by its template.
    pub output: serde_json::Value,
    /// all of the other resources that this resource depended on at the time it was last deployed.
    /// in other words: this is a list of all of the unique resource names that this resource had in its
    /// input as json paths the last time it was deployed
    pub depends_on: Vec<String>
}

#[derive(Debug, Clone)]
pub enum TransitionableResource {
    /// a resource is Createable if it exists in the current .dpl file
    /// but has no prior state entry
    Create { current_entry: ResourceSection },
    /// a resource is Updaeable if it exists in the current .dpl file
    /// and it also has a prior state entry. Note: the existance of an Update TransitionableResource
    /// implies that it's current input and last input differ. if a resource's current input is exactly the same
    /// as its input from the state entry, it should not be represented here. There is a caveat which is
    /// resources whose inputs have a json path, which implies we cannot yet compare their input, as it has not been resolved yet.
    /// these resources may be unchanged, but they will be represented as an update temporarily until their dependencies finish
    /// and then this resource's input can be represented as a serde_json::Value, at which point
    /// we can then compare the current input against the last state input
    Update { state_entry: ResourceInState, current_entry: ResourceSection },
    /// a resource is Deleteable if it does not exist in the current .dpl file
    /// but it has a prior state entry. for this reason the state entry must preserve the template name
    /// that this resource applies to. we can then look up the template and run its deletion lifecycle command(s).
    /// it is required that the template exists, otherwise it should be a runtime error to indicate to the user
    /// that they must add back the template, or otherwise accept to have a detached resource no longer managed by dpl
    Delete { state_entry: ResourceInState },
}

impl Default for TransitionableResource {
    fn default() -> Self {
        Self::Delete { state_entry: Default::default() }
    }
}

impl TransitionableResource {
    /// returns the template name of the resource to be transitioned.
    /// for update transitions will error if the state entry template name differs from the current entry template name
    pub fn get_template_name(&self) -> Result<&str, String> {
        match self {
            TransitionableResource::Create { current_entry } => Ok(current_entry.template_name.as_str()),
            TransitionableResource::Update { state_entry, current_entry } => {
                let state_template_name = state_entry.template_name.as_str();
                let current_template_name = current_entry.template_name.as_str();
                if state_template_name != current_template_name {
                    return Err(format!(
                        "resource '{}' currently references template '{}' but it was previously deployed with template '{}'",
                        current_entry.resource_name,
                        current_template_name,
                        state_template_name
                    ));
                }
                Ok(current_template_name)
            }
            TransitionableResource::Delete { state_entry } => Ok(state_entry.template_name.as_str())
        }
    }
    pub fn get_resource_name(&self) -> &str {
        match self {
            TransitionableResource::Create { current_entry } => current_entry.resource_name.as_str(),
            TransitionableResource::Update { current_entry, .. } => current_entry.resource_name.as_str(),
            TransitionableResource::Delete { state_entry } => state_entry.resource_name.as_str(),
        }
    }
    pub fn is_delete(&self) -> bool {
        match self {
            TransitionableResource::Delete { .. } => true,
            _ => false,
        }
    }
    pub fn is_create(&self) -> bool {
        match self {
            TransitionableResource::Create { .. } => true,
            _ => false,
        }
    }
    pub fn is_update(&self) -> bool {
        match self {
            TransitionableResource::Update { .. } => true,
            _ => false,
        }
    }
}

/// transitionable resource with template
#[derive(Debug, Default)]
pub struct TrWithTemplate {
    pub tr: TransitionableResource,
    pub template: TemplateSection,
}

/// iterate the currently known resources, and return a list of TransitionableResources
/// where each resource is either to be created (no state entry for that resource)
/// updated (the resource has a prior entry in state and the resource either has a json path [which means
/// we dont know yet if it has changed or not] or the resource's input has changed from its prior state entry)
/// or deleted (any state entries that remain that dont have a corresponding resource in the dpl file. these are resources
/// that were previously created and now must be deleted).
/// after calling this function, the state's resources will only contain no-op resources whose input has not changed
pub fn get_transitionable_resources(
    state: &mut StateFile,
    dpl: &mut DplFile,
) -> Vec<TransitionableResource> {
    // first collect the known resources that are to be created or updated:
    let mut out = Vec::with_capacity(state.resources.len());
    let mut potentially_done_updates = vec![];
    let mut done_resources = HashMap::new();
    for resource in dpl.resources.drain(..) {
        match state.resources.remove(&resource.resource_name.s) {
            Some(state_entry) => {
                // if we can determine this resource has not changed, then we can omit it
                // from the transitionable resources:
                match resource_input_has_been_changed(&resource, &state_entry) {
                    Some(true) => {
                        // its input has changed since last deploy, it needs to be updated
                        out.push(TransitionableResource::Update { state_entry, current_entry: resource })
                    },
                    Some(false) => {
                        // its input has not changed, dont add it to the
                        // list of transitionable resources. instead, treat it as
                        // done so other resources can read its output
                        done_resources.insert(state_entry.resource_name.clone(), state_entry);
                    },
                    None => {
                        // its unknown if this resource will be updated since it references other resources dynamically
                        // we temporarily add it to "potentially_done_updates" and later
                        // check all of its references to see if they are done, and we can then update this value to check if
                        // its trully a transitionable update, or perhaps its already done
                        potentially_done_updates.push((state_entry, resource));
                    },
                }
            }
            None => {
                // this resource is to be created since there's no corresponding state entry
                out.push(TransitionableResource::Create { current_entry: resource });
            }
        }
    }
    // check all of the potentially done updates and for any update that has all of its dependencies already done
    // we can remove it from transitionable updates and mark it as done now
    for (state_entry, current_entry) in potentially_done_updates {
        let resolved_input = match resolve_current_input_value(&current_entry.input, current_entry.resource_name.as_str(), &done_resources) {
            Ok(v) => v,
            Err(_) => {
                // if we for some reason fail to lookup the immediate deps
                // or its not done yet, we can simply treat this as an updateable resource.
                // later, once its dependencies finish, we will try to resolve the input again
                // where it might succeed, or if it fails then we'll error. but for now we cannot know
                // if this is done yet or not
                out.push(TransitionableResource::Update { state_entry, current_entry });
                continue;
            }
        };
        // we were able to get its current input, now check if it differs from the last time it was deployed
        // if not, we can treat this as already done
        if resolved_input == state_entry.last_input {
            done_resources.insert(current_entry.resource_name.s, state_entry);
        } else {
            out.push(TransitionableResource::Update { state_entry, current_entry });
        }
    }
    // now, check all of the resources in the state file that do not have corresponding
    // entry in the current dpl file, these resources are to be deleted:
    for (_, state_entry) in state.resources.drain() {
        out.push(TransitionableResource::Delete { state_entry })
    }
    // afterwards, put back all of the resources that had no-op updates into the state
    // such that other resources can look up their outputs:
    state.resources = done_resources;
    out
}

/// given a list of resources to be transitioned, check all of their dynamic values
/// for json path references to function calls. replace those references to be simple
/// resource references such as '$.__fake_resource.output' where the fake resource
/// is a new ephemeral resource we insert into the output vec whose output is the entire value
/// of the function call output
pub fn insert_function_resources(
    dpl: &DplFile,
    state: &StateFile,
    out: &mut Vec<TransitionableResource>,
) -> Result<(), String> {
    let mut fake_resources: Vec<ResourceSection> = vec![];
    let mut all_resource_names: Vec<String> = out.iter().map(|x| x.get_resource_name().to_string()).collect();
    for (done_resource, _) in state.resources.iter() {
        all_resource_names.push(done_resource.clone());
    }
    all_resource_names.dedup();
    for tr in out.iter_mut() {
        let input = match tr {
            TransitionableResource::Create { current_entry } |
            TransitionableResource::Update { current_entry, .. } => &mut current_entry.input,
            TransitionableResource::Delete { .. } => continue,
        };
        let inp = std::mem::replace(input, json_with_positions::Value::Null { pos: Default::default(), val: () });
        let inp = inp.to_value_with_replaced_json_paths(&mut |s, pos| {
            let jpq = jsonpath_rust::parser::parse_json_path(s.as_str())
                .map_err(|e| format!("failed to parse json path '{}': {:?}", s, e))?;
            // check if its a function call:
            let first = jpq.segments.first()
                .ok_or("json path must have at least 1 segment")?;
            let mut fn_call_name = first.to_string();
            unquote_bracketed_selector(&mut fn_call_name);
            if let Some(func) = dpl.functions.iter().find(|x| &x.function_name.s == &fn_call_name) {
                let resource_arg = jpq.segments.iter().nth(1)
                    .ok_or("function call must be followed by resource to be passed into said function")?;
                let mut resource_arg = resource_arg.to_string();
                unquote_bracketed_selector(&mut resource_arg);
                if all_resource_names.iter().find(|r| *r == &resource_arg).is_none() {
                    return Err(format!("failed to find resource to pass into function"));
                }
                // create a new resource that will be running this function:
                let mut map: HashMap<_, json_with_positions::Value> = HashMap::new();
                let mut key = StringAtLine::default();
                key.s = "function_name".to_string();
                map.insert(key, json_with_positions::Value::String { pos: Position::default(), val: func.function_name.clone() });
                let mut key = StringAtLine::default();
                key.s = "function_type".to_string();
                map.insert(key, json_with_positions::Value::String { pos: Position::default(), val: func.function_type.clone() });
                let mut key = StringAtLine::default();
                key.s = "function_body".to_string();
                let mut val = StringAtLine::default();
                val.s = func.get_body();
                map.insert(key, json_with_positions::Value::String { pos: Position::default(), val });
                let mut key = StringAtLine::default();
                key.s = "depends_on".to_string();
                map.insert(key, json_with_positions::parse_json_value(&format!("$.{}", resource_arg))?);
                let fake_input = json_with_positions::Value::Object { pos: Position::default(), val: map };
                let mut fake_resource = ResourceSection {
                    resource_name: Default::default(),
                    template_name: Default::default(),
                    input: fake_input,
                };
                fake_resource.resource_name.s = format!("{}_{}_{}", FUNCTION_RESOURCE_PREFIX, func.function_name.as_str(), resource_arg);
                fake_resource.template_name.s = FUNCTION_RESOURCE_PREFIX.to_string();
                // replace the json path reference with a reference instead to the fake resource
                // which will resolve to the value of the function output
                let mut fake_json_path = s.clone();
                fake_json_path.s = format!("$.{}.output", fake_resource.resource_name.s);
                // dont add duplicates. this allows us to copy the same function call on the same resource
                // instead of running the function multiple times for the same resource.
                // this works as long as every function call to the resource X is always named consistently:
                if fake_resources.iter().find(|x| x.resource_name.as_str() == fake_resource.resource_name.as_str()).is_none() {
                    fake_resources.push(fake_resource);
                }
                Ok(json_with_positions::Value::JsonPath { pos, val: fake_json_path })
            } else {
                // its not a function, return the json path as it was:
                Ok(json_with_positions::Value::JsonPath { pos, val: s })
            }
        })?;
        *input = inp;
    }
    // finally, add all of the fake resources into the list of transitionable resources
    for fake in fake_resources {
        out.push(TransitionableResource::Create { current_entry: fake });
    }
    Ok(())
}

/// returns None if we cannot know if the input has changed due to
/// the current input referencing json paths.
/// returns Some(true) if the current input != the last input
/// returns Some(false) if the current input has unchanged since the last input
pub fn resource_input_has_been_changed(current: &ResourceSection, previous: &ResourceInState) -> Option<bool> {
    let current_input = current.input.to_serde_json_value_pure()?;
    return Some(current_input != previous.last_input)
}

pub fn match_resources_with_template(mut transitionable: Vec<TransitionableResource>, dpl: &DplFile) -> Result<Vec<TrWithTemplate>, String> {
    let mut out = Vec::with_capacity(transitionable.len());
    for tr in transitionable.drain(..) {
        let template_name = tr.get_template_name()?;
        let resource_name = tr.get_resource_name();
        let is_delete = tr.is_delete();
        if template_name == FUNCTION_RESOURCE_PREFIX {
            // this is a fake, ephemeral resource, which doesnt correspond to a real
            // template, add it to TrWithTemplate with a fake template:
            out.push(TrWithTemplate { tr, template: TemplateSection::default() });
            continue;
        }
        let template = dpl.templates.iter()
            .find(|t| t.template_name == template_name)
            .ok_or_else(|| {
                // important error here: we failed to find a template that a user referenced.
                // for creates/updates this might just be some logic error/they forgot/etc.
                // but importantly for deletes we must present this error seriously to the user.
                // having a resource that was removed, but exists in state means we must delete the resource.
                // but if there's no template defined on how to delete that resource, that resource may linger forever
                // out of state.
                if is_delete {
                    return format!(r#"
resource '{}' is to be deleted, but its template '{}' does not exist in the current DPL file.
the following are your options, in order from most recommended to least recommended:
1. if you edited the DPL file and removed template '{}', simply add that template back, and run deploy again.
2. if you wish to delete the resource manually, delete it manually first, then re run deploy using 'TODO some flag for explicitly deleting from state'. then run deploy again.
3. if you wish to ignore this resource temporarily and deploy everything else, re run deploy with 'TODO some flag for ignoring this resource'.
4. if you wish to detach this resource from state permanently, re run deploy with 'TODO some flag for explicitly detaching'. The real resource may persist indefinitely, and this program will not be able to manage it. use with caution"#,
                        resource_name,
                        template_name,
                        template_name
                    );
                }
                format!("unable to find template '{}' referenced by resource '{}'", template_name, resource_name)
            })?;
        out.push(TrWithTemplate { tr, template: template.clone() });
    }

    Ok(out)
}

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

pub fn get_resource_name_from_segment(
    current_name: &str,
    segment: &jsonpath_rust::parser::model::Segment
) -> Result<String, String> {
    let resource_name = match segment {
        jsonpath_rust::parser::model::Segment::Selector(selector) => match selector {
            jsonpath_rust::parser::model::Selector::Name(s) => s,
            x => return Err(format!("json path for resource '{}' must start with a segment selector that references another resource. instead found '{}'", current_name, x)),
        },
        x => return Err(format!("json path for resource '{}' must start with a segment selector that references another resource. instead found '{}'", current_name, x)),
    };
    // json path parsing for some reason maintains the quotes in bracketed selectors.
    // we wish to unquote:
    let mut resource_name = resource_name.to_owned();
    unquote_bracketed_selector(&mut resource_name);
    Ok(resource_name)
}

pub fn unquote_bracketed_selector(s: &mut String) {
    remove_bracketed_selector_quotes::<'"'>(s);
    remove_bracketed_selector_quotes::<'\''>(s);
}

pub fn remove_bracketed_selector_quotes<const C: char>(s: &mut String) {
    while s.starts_with(C) && s.ends_with(C) && s.len() > 1 {
        s.remove(0);
        s.pop();
    }
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

pub fn error_statically_cannot_perform_transition(
    resource_name: &str,
    template_name: &str,
    transition_type: &str,
) -> String {
    return format!(r#"resource '{}' is to be {}d but template '{}' does not define any {} commands
your options are:
1. define what a {} behavior should be for template '{}'
2. if you intend to never {} this resource annotate it with TODO: define some resource directive to prevent {}s
3. if you want to skip {}s for this resource temporarily, rerun with -- TODO: define cli option prevent {}s for resource by name"#,
    resource_name, transition_type, template_name, transition_type, transition_type, template_name, transition_type,
    transition_type, transition_type, transition_type
    )
}

pub fn verify_transitions(
    trs: &[TrWithTemplate]
) -> Result<(), String> {
    for tr in trs.iter() {
        match &tr.tr {
            // creates are always transitionable because a template must have a create section:
            TransitionableResource::Create { .. } => {}
            // need to check that template has an update subsection
            // otherwise error
            TransitionableResource::Update { current_entry, .. } => {
                if tr.template.update.is_none() {
                    return Err(error_statically_cannot_perform_transition(
                        current_entry.resource_name.as_str(),
                        current_entry.template_name.as_str(),
                        "update"
                    ))
                }
            }
            // need to check that template has a delete subsection
            // otherwise error
            TransitionableResource::Delete { state_entry } => {
                if tr.template.delete.is_none() {
                    return Err(error_statically_cannot_perform_transition(
                        state_entry.resource_name.as_str(),
                        state_entry.template_name.as_str(),
                        "delete"
                    ))
                }
            }
        }
    }
    Ok(())
}

pub async fn perform_update(
    logger: &'static dyn log::Log,
    mut dpl: DplFile,
    mut state: StateFile,
) -> Result<StateFile, String> {
    // first, collect resources into create/update/or delete, discarding
    // any resources that dont need to be updated
    let mut transitionable_resources = get_transitionable_resources(&mut state, &mut dpl);
    // add fake resources that will correspond to the output of function calls:
    insert_function_resources(&dpl, &state,&mut transitionable_resources)?;
    // next, ensure every resource can be matched with a template. error otherwise:
    let mut transitionable_resources = match_resources_with_template(transitionable_resources, &dpl)?;
    // ensure every resource to be transitioned *can* be transitioned before starting any tasks:
    verify_transitions(&transitionable_resources)?;
    // split out deletes
    let deletes: Vec<TrWithTemplate> = transitionable_resources.extract_if(.., |tr| tr.tr.is_delete()).collect();
    let create_or_updates = transitionable_resources;
    // process creates/updates first then deletes
    perform_update_batch(logger, &mut state, create_or_updates).await?;
    perform_delete_batch(logger, deletes).await?;
    Ok(state)
}

pub async fn perform_update_batch(
    logger: &'static dyn log::Log,
    state: &mut StateFile,
    mut batch: Vec<TrWithTemplate>,
) -> Result<(), String> {
    // get map of names of resources to a flat list of all transient dependencies of that resource
    let dependency_map = get_all_transient_dependencies_map(&batch, &state.resources)?;
    let dep_map = &dependency_map;
    if batch.is_empty() { return Ok(()) }

    let mut task_set = tokio::task::JoinSet::new();
    // fire off async tasks for all trs that can be updated now, remove them from the list
    // so that they aren't processed multiple times
    spawn_all_currently_transitionable(logger, &state, &mut batch, dep_map, &mut task_set)?;
    if task_set.is_empty() {
        return Err(format!("error: unable to spawn any transitionable resource jobs"))
    }
    while let Some(next) = task_set.join_next().await {
        let res = next.map_err(|e| format!("deployment task panicked: {:?}", e))?;
        // TODO: should 1 resource failure stop all other resources? for now it will...
        let res = res.map_err(|(resource_name, e)| {
            if !resource_name.starts_with(FUNCTION_RESOURCE_PREFIX) {
                log::error!(logger: logger, "resource '{}' failed to transition. error: {}", resource_name, e);
            }
            e
        })?;
        if res.template_name != FUNCTION_RESOURCE_PREFIX {
            log::info!(logger: logger, "resource '{}' OK", res.resource_name);
        } else {
            let (fn_name, resource_name) = extract_fn_call_names(&res.resource_name).unwrap_or_default();
            log::info!(logger: logger, "function call '{}({})' OK", fn_name, resource_name);
        }
        // add this to the state, then look up all the resources that depend on this resource and try to
        // spawn tasks for any other resource that can be transitioned now
        let resource_name = res.resource_name.clone();
        state.resources.insert(resource_name.clone(), res);
        // spawn everything thats now transitionable: the resource that just finished may have made it possible to spawn more
        spawn_all_currently_transitionable(logger, &state, &mut batch, dep_map, &mut task_set)?;
    }
    // if theres any remaining TRs in the batch, error out as the transition cannot be counted a success:
    if !batch.is_empty() {
        return Err(format!("{} resources were not transitioned: {:?}", batch.len(), batch.iter().map(|x| x.tr.get_resource_name()).collect::<Vec<_>>()))
    }
    // remove all ephemeral resources from state. we had to insert them temporarily
    // so their dependencies can run from their output shape
    // but we dont want them in the final state:
    state.resources.retain(|k, _| {
        if k.starts_with(FUNCTION_RESOURCE_PREFIX) {
            return false;
        }
        true
    });
    // finally, check all dependencies of the state resources and remove any that start with __priv
    // we dont want those ephemeral dependencies to persist:
    for (_, resource) in state.resources.iter_mut() {
        resource.depends_on.retain(|d| !d.starts_with(FUNCTION_RESOURCE_PREFIX));
    }
    Ok(())
}

pub fn extract_fn_call_names(resource_name: &str) -> Option<(&str, &str)> {
    let (remaining, resource_name) = resource_name.rsplit_once("_")?;
    let (_, function_name) = remaining.rsplit_once("_")?;
    return Some((function_name, resource_name));
}

pub async fn perform_delete_batch(
    logger: &'static dyn log::Log,
    mut batch: Vec<TrWithTemplate>,
) -> Result<(), String> {
    if batch.is_empty() { return Ok(()) }
    // get an order map that says "this resource must wait for other resource(s) to be deleted first"
    let delete_order_map = get_delete_order_map(&batch)?;
    let mut already_deleted = HashSet::new();

    let mut task_set = tokio::task::JoinSet::new();
    // fire off async tasks for all trs that can be deleted now, remove them from the list
    // so that they aren't processed multiple times
    spawn_all_currently_deleteable(logger, &mut batch, &already_deleted, &delete_order_map, &mut task_set)?;
    if task_set.is_empty() {
        return Err(format!("error: unable to spawn any deleteable resource jobs"))
    }
    while let Some(next) = task_set.join_next().await {
        let res = next.map_err(|e| format!("deployment task panicked: {:?}", e))?;
        // TODO: should 1 resource failure stop all other resources? for now it will...
        let res = res.map_err(|(resource_name, e)| {
            log::error!(logger: logger, "resource '{}' failed to delete. error: {}", resource_name, e);
            e
        })?;
        log::info!(logger: logger, "resource '{}' OK", res.resource_name);
        // add it to the set of deleted resources. note deletes dont need to update the statefile
        // since they've already been removed from state. and after deletion we dont add them back obviously
        already_deleted.insert(res.resource_name);
        // spawn everything thats now deleteable: the resource that just finished may have made it possible to spawn more
        spawn_all_currently_deleteable(logger, &mut batch, &already_deleted, &delete_order_map, &mut task_set)?;
    }
    // if theres any remaining TRs in the batch, error out as the transition cannot be counted a success:
    if !batch.is_empty() {
        return Err(format!("{} resources were not deleted: {:?}", batch.len(), batch.iter().map(|x| x.tr.get_resource_name()).collect::<Vec<_>>()))
    }
    Ok(())
}

pub fn retain_mut_err<T, E>(v: &mut Vec<T>, mut f: impl FnMut(&mut T) -> Result<bool, E>) -> Result<(), E> {
    let mut error: Option<E> = None;
    v.retain_mut(|t| {
        match f(t) {
            Ok(b) => return b,
            Err(e) => error = Some(e)
        }
        true
    });
    if let Some(e) = error {
        return Err(e);
    }
    Ok(())
}

pub fn spawn_all_currently_transitionable(
    logger: &'static dyn log::Log,
    state: &StateFile,
    list: &mut Vec<TrWithTemplate>,
    dep_map: &HashMap<String, Vec<String>>,
    task_set: &mut tokio::task::JoinSet<Result<ResourceInState, (String, String)>>,
) -> Result<(), String> {
    retain_mut_err::<_, String>(list, |tr| {
        if can_tr_be_transitioned(tr, dep_map, &state) {
            let tr = std::mem::take(tr);
            // now that we know all of its dependencies are done
            // turn its input into a serde_json::Value
            let input = prepare_tr_for_transition(&tr, &state)?;
            let dependencies = dep_map.get(tr.tr.get_resource_name())
                // shouldnt be possible here, but error to be safe
                .ok_or("failed to lookup resource from dependency_map")?
                .to_vec();
            task_set.spawn(async move {
                transition_single(logger, tr, input, dependencies).await
            });
            return Ok(false)
        }
        Ok(true)
    })
}

pub fn spawn_all_currently_deleteable(
    logger: &'static dyn log::Log,
    list: &mut Vec<TrWithTemplate>,
    already_deleted: &HashSet<String>,
    delete_order_map: &HashMap<String, Vec<String>>,
    task_set: &mut tokio::task::JoinSet<Result<ResourceInState, (String, String)>>,
) -> Result<(), String> {
    retain_mut_err::<_, String>(list, |tr| {
        // look up the list of resources it needs to wait for deletion:
        let delete_first = delete_order_map.get(tr.tr.get_resource_name())
            .ok_or_else(|| format!("resource '{}' is to be deleted but it doesnt exist in the delete_order map", tr.tr.get_resource_name()))?;
        let can_be_deleted_now = delete_first.iter().all(|d| already_deleted.contains(d));
        if can_be_deleted_now {
            let tr = std::mem::take(tr);
            let input = match &tr.tr {
                TransitionableResource::Delete { state_entry } => state_entry.last_input.clone(),
                _ => return Err(format!("resource '{}' was in delete batch but its not a delete TR", tr.tr.get_resource_name())),
            };
            task_set.spawn(async move {
                transition_single(logger, tr, input, vec![]).await
            });
            return Ok(false)
        }
        Ok(true)
    })
}

/// annoyingly will process json path query twice.
/// we need to run it twice because jsonpath-rust doesnt expose a way to know if results
/// are truly empty, or if the lookup returned an empty array.
/// for that reason we run process first, check if it returned no results
/// and if no results to error. otherwise to run again, and return
pub fn process_json_path_query(jpq: &JpQuery, val: &serde_json::Value) -> Result<Vec<serde_json::Value>, String> {
    let processed_state = jpq.segments.process(State::root(val));
    if processed_state.is_nothing() {
        return Err(format!("json path query failed to lookup a value"));
    }
    let mut res = js_path_process(jpq, val)
        .map_err(|e| format!("could not process json path query lookup: {:?}", e))?;
    return Ok(res.drain(..).map(|x| x.val().clone()).collect())
}

pub fn resolve_current_input_value(
    current_json: &json_with_positions::Value,
    current_resource_name: &str,
    done_resources: &HashMap<String, ResourceInState>
) -> Result<serde_json::Value, String> {
    current_json.to_serde_json_value_with_replace_func(&mut |s| {
        let mut jpq = jsonpath_rust::parser::parse_json_path(s)
            .map_err(|e| format!(
                "cannot create/update resource '{}'. invalid json path string '{}' error: {:?}",
                current_resource_name, s, e
            ))?;
        // the first segment in the query should correspond to an existing resource in the state file
        let first_seg = if jpq.segments.len() == 0 {
            return Err(format!("cannot create/update resource '{}'. json path string '{}' does not reference anything", current_resource_name, s));
        } else {
            jpq.segments.remove(0)
        };
        let resource_name = get_resource_name_from_segment(current_resource_name, &first_seg)?;
        let resource = done_resources.get(&resource_name).ok_or_else(|| format!("cannot create/update resource '{}'. json path references '{}' but this resource does not exist in state", current_resource_name, resource_name))?;
        if current_resource_name.starts_with(FUNCTION_RESOURCE_PREFIX) {
            // this is a function call resource, so dont perform any of the below validation
            // the input value should be resolved to the entire input/output/name shape
            // of the resource:
            let name = resource.resource_name.clone();
            let output = resource.output.clone();
            let input = resource.last_input.clone();
            return Ok(serde_json::json!({
                "input": input,
                "output": output,
                "name": name,
            }))
        }
        let next_seg = if jpq.segments.len() == 0 {
            return Err(format!("cannot create/update resource '{}'. json path string '{}' does not reference input or output of resource '{}'", current_resource_name, s, resource_name))
        } else {
            jpq.segments.remove(0)
        };
        let input_or_output = get_resource_name_from_segment(current_resource_name, &next_seg)?;
        let value_to_lookup = match input_or_output.as_str() {
            "input" => &resource.last_input,
            "output" => &resource.output,
            "name" => return Ok(serde_json::Value::String(resource_name)),
            x => return Err(format!("cannot create/update resource '{}'. json path string '{}' after resource name '{}' must be 'input', 'output', or 'name'. instead found '{}'", current_resource_name, s, resource_name, x))
        };
        // now that we have removed the first few segments of the json path, we can use jsonpath_rust to
        // query the serde value:
        let mut result_vals = process_json_path_query(&jpq, value_to_lookup).map_err(|e| {
            format!("cannot create/update resource '{}'. json path string '{}' error: {}", current_resource_name, s, e)
        })?;
        // if the value looked up was a single value, use it as a single value
        let val = if result_vals.len() == 1 {
            result_vals.pop().unwrap_or_default()
        } else {
            // otherwise user likely indeed wanted it to be an array:
            let vals = result_vals;
            serde_json::Value::Array(vals)
        };

        Ok(val)
    })
}

/// process all of the json paths in this tr's current input
/// and return a serde_json::Value representing its current input
/// that will be passed to the deployment function.
pub fn prepare_tr_for_transition(
    tr: &TrWithTemplate,
    state: &StateFile
) -> Result<serde_json::Value, String> {
    let current_json = match &tr.tr {
        TransitionableResource::Create { current_entry } |
        TransitionableResource::Update { current_entry, .. } => &current_entry.input,
        TransitionableResource::Delete { state_entry } => return Ok(state_entry.last_input.clone())
    };
    let current_input = resolve_current_input_value(
        current_json,
        tr.tr.get_resource_name(),
        &state.resources
    )?;

    Ok(current_input)
}

/// a TR can be transitioned iff all of its dependencies are done
pub fn can_tr_be_transitioned(tr: &TrWithTemplate, dep_map: &HashMap<String, Vec<String>>, state: &StateFile) -> bool {
    let no_deps = vec![];
    let dep_list = dep_map.get(tr.tr.get_resource_name()).unwrap_or(&no_deps);
    dep_list.iter().all(|dep| state.resources.contains_key(dep))
}

pub async fn transition_single(
    logger: &'static dyn log::Log,
    tr: TrWithTemplate,
    current_input: serde_json::Value,
    depends_on: Vec<String>
) -> Result<ResourceInState, (String, String)> {
    // first check if this is an update:
    if let TransitionableResource::Update { state_entry, .. } = &tr.tr {
        // if the input is the same (now that it's been able to be resolved)
        // we can exit early. we still want to return its state such that our caller
        // adds it to the statefile and this can be considered done.
        if state_entry.last_input == current_input {
            let mut state_entry = state_entry.clone();
            state_entry.depends_on = depends_on;
            log::trace!(logger: logger, "resource '{}' input has not changed since last transition. returning noop", tr.tr.get_resource_name());
            return Ok(state_entry)
        }
    }

    let TrWithTemplate { tr, template } = tr;
    match tr {
        TransitionableResource::Create { current_entry } => {
            if current_entry.template_name.as_str() == FUNCTION_RESOURCE_PREFIX {
                let output = run_function::run_function(
                    logger,
                    current_entry.resource_name.as_str(),
                    current_input.clone(),
                ).await.map_err(|e| (current_entry.resource_name.s.clone(), e))?;
                return Ok(ResourceInState {
                    resource_name: current_entry.resource_name.s,
                    template_name: FUNCTION_RESOURCE_PREFIX.to_string(),
                    last_input: current_input,
                    output,
                    depends_on,
                })
            }
            let output = run_template::run_template(
                logger,
                current_entry.resource_name.as_str(),
                &template.template_name.s,
                template.create,
                "create",
                current_input.clone(),
                None,
                None,
            ).await.map_err(|e| (current_entry.resource_name.s.clone(), e))?;
            Ok(ResourceInState {
                resource_name: current_entry.resource_name.s,
                template_name: template.template_name.s,
                last_input: current_input,
                output,
                depends_on,
            })
        }
        TransitionableResource::Update { state_entry, current_entry } => {
            let output = run_template::run_template(
                logger,
                current_entry.resource_name.as_str(),
                &template.template_name.s,
                template.update.ok_or_else(||
                    // by the time we get here, all updates should have been verified to have
                    // an update template. so not going to worry about a detailed error message here
                    (current_entry.resource_name.s.clone(), "update missing template".to_string()))?,
                "update",
                current_input.clone(),
                Some(state_entry.last_input),
                Some(state_entry.output),
            ).await.map_err(|e| (current_entry.resource_name.s.clone(), e))?;
            Ok(ResourceInState {
                resource_name: current_entry.resource_name.s,
                template_name: template.template_name.s,
                last_input: current_input,
                output,
                depends_on,
            })
        }
        TransitionableResource::Delete { state_entry } => {
            let _output = run_template::run_template(
                logger,
                &state_entry.resource_name,
                &template.template_name.s,
                template.delete.ok_or_else(||
                    // by the time we get here, all deletes should have been verified to have
                    // a delete template. so not going to worry about a detailed error message here
                    (state_entry.resource_name.clone(), "delete missing template".to_string()))?,
                "delete",
                current_input.clone(),
                Some(state_entry.last_input),
                Some(state_entry.output),
            ).await.map_err(|e| (state_entry.resource_name.clone(), e))?;
            // deletes can drop their output, input, and dependency information, we're not saving it to state.
            Ok(ResourceInState {
                resource_name: state_entry.resource_name,
                template_name: template.template_name.s,
                last_input: serde_json::Value::Null,
                output: serde_json::Value::Null,
                depends_on: vec![],
            })
        }
    }
}

#[cfg(test)]
mod test {
    use std::{path::PathBuf, sync::Mutex};

    use super::*;
    use assert_matches::assert_matches;
    use deploy_language::parse::template::{ArgTransform, CliCommand, CliCommandWithDirectives};

    #[test]
    fn can_determine_createable_resources() {
        let mut dpl = DplFile::default();
        let mut state = StateFile::default();
        dpl.resources.push(ResourceSection {
            resource_name: "a".into(),
            template_name: "t".into(),
            input: json_with_positions::parse_json_value(r#"{
                "a": "a"
            }"#).unwrap(),
        });
        // no prior state for resource 'a' so it should be created:
        let mut transitionable = get_transitionable_resources(&mut state, &mut dpl);
        assert_eq!(transitionable.len(), 1);
        let resource = transitionable.pop().unwrap();
        assert_matches!(resource, TransitionableResource::Create { .. });
        assert_eq!(state.resources.len(), 0);
    }

    #[test]
    fn can_determine_deleteable_resources() {
        let mut dpl = DplFile::default();
        let mut state = StateFile::default();
        state.resources.insert("a".to_string(), ResourceInState {
            resource_name: "a".to_string(),
            template_name: "t".to_string(),
            last_input: serde_json::json!({"a": "a"}),
            ..Default::default()
        });
        // no current entry for resource 'a', but state does have a prior entry for resource 'a'
        // so it should be deleted
        let mut transitionable = get_transitionable_resources(&mut state, &mut dpl);
        assert_eq!(transitionable.len(), 1);
        let resource = transitionable.pop().unwrap();
        assert_matches!(resource, TransitionableResource::Delete { .. });
        assert_eq!(state.resources.len(), 0);
    }

    #[test]
    fn can_determine_updateable_resources() {
        let mut dpl = DplFile::default();
        let mut state = StateFile::default();
        dpl.resources.push(ResourceSection {
            resource_name: "a".into(),
            template_name: "t".into(),
            input: json_with_positions::parse_json_value(r#"{
                "a": "this current input != the resource's last input"
            }"#).unwrap(),
        });
        state.resources.insert("a".to_string(), ResourceInState {
            resource_name: "a".to_string(),
            template_name: "t".to_string(),
            last_input: serde_json::json!({"a": "a"}),
            ..Default::default()
        });
        // a has a prior state, but its input has since changed. it should be updateable
        let mut transitionable = get_transitionable_resources(&mut state, &mut dpl);
        assert_eq!(transitionable.len(), 1);
        let resource = transitionable.pop().unwrap();
        assert_matches!(resource, TransitionableResource::Update { .. });
        assert_eq!(state.resources.len(), 0);
    }

    #[test]
    fn can_determine_updateable_resources_due_to_json_path() {
        let mut dpl = DplFile::default();
        let mut state = StateFile::default();
        dpl.resources.push(ResourceSection {
            resource_name: "a".into(),
            template_name: "t".into(),
            input: json_with_positions::parse_json_value(r#"{
                "a": $.i.depend.on.some.other.resource
            }"#).unwrap(),
        });
        state.resources.insert("a".to_string(), ResourceInState {
            resource_name: "a".to_string(),
            template_name: "t".to_string(),
            last_input: serde_json::json!({"a": "a"}),
            ..Default::default()
        });
        // a has a prior state, but its current input is dynamic (has a json path)
        // so it should be considered updateable until its current input can be resolved
        // to an explicit value
        let mut transitionable = get_transitionable_resources(&mut state, &mut dpl);
        assert_eq!(transitionable.len(), 1);
        let resource = transitionable.pop().unwrap();
        assert_matches!(resource, TransitionableResource::Update { .. });
        assert_eq!(state.resources.len(), 0);
    }

    #[test]
    fn can_determine_noop_updates_if_input_unchanged() {
        let mut dpl = DplFile::default();
        let mut state = StateFile::default();
        dpl.resources.push(ResourceSection {
            resource_name: "a".into(),
            template_name: "t".into(),
            input: json_with_positions::parse_json_value(r#"{
                "a": {"v1": "v1", "v2": "v2" }
            }"#).unwrap(),
        });
        state.resources.insert("a".to_string(), ResourceInState {
            resource_name: "a".to_string(),
            template_name: "t".to_string(),
            last_input: serde_json::json!({"a": {"v1": "v1", "v2": "v2" }}),
            ..Default::default()
        });
        // a has a prior state, but its last input is the same as its current input
        // therefore it should not be transitionable
        let transitionable = get_transitionable_resources(&mut state, &mut dpl);
        assert_eq!(transitionable.len(), 0);
        // it should be in the state still because its treated as "already done"
        assert_matches!(state.resources.get("a"), Some(r) => {
            assert_eq!(r.resource_name, "a");
            assert_eq!(r.template_name, "t");
            assert_eq!(r.last_input, serde_json::json!({"a":{"v1":"v1","v2":"v2"}}));
        });
        assert_eq!(state.resources.len(), 1);
    }

    #[test]
    fn can_match_resources_with_their_templates() {
        let mut dpl = DplFile::default();
        let mut state = StateFile::default();
        // dpl file has 1 template 't'
        let mut template = TemplateSection::default();
        template.template_name.s = "t".to_string();
        dpl.templates.push(template);
        // and 3 resources. one to be created, one to be updated, and one to be deleted.
        // all point to template 't':

        // resources to be created because it doesnt have corresponding state entry:
        dpl.resources.push(ResourceSection {
            resource_name: "a".into(),
            template_name: "t".into(),
            input: json_with_positions::parse_json_value(r#"{
                "a": "a"
            }"#).unwrap(),
        });
        // resource to be updated because its state entry differs from current:
        dpl.resources.push(ResourceSection {
            resource_name: "b".into(),
            template_name: "t".into(),
            input: json_with_positions::parse_json_value(r#"{
                "b": "b"
            }"#).unwrap(),
        });
        state.resources.insert("b".to_string(), ResourceInState {
            resource_name: "b".to_string(),
            template_name: "t".to_string(),
            last_input: serde_json::json!({"b": {"v1": "v1", "v2": "v2" }}),
            ..Default::default()
        });
        // resource to be deleted because it has no current entry:
        state.resources.insert("c".to_string(), ResourceInState {
            resource_name: "c".to_string(),
            template_name: "t".to_string(),
            ..Default::default()
        });
        let transitionable = get_transitionable_resources(&mut state, &mut dpl);
        let matched = match_resources_with_template(transitionable, &dpl).expect("should not error");
        assert_eq!(matched.len(), 3);
        let mut create_found = false;
        let mut update_found = false;
        let mut delete_found = false;
        for m in matched {
            assert_eq!(m.template.template_name.s, "t");
            if m.tr.is_create() {
                create_found = true;
            }
            if m.tr.is_update() {
                update_found = true;
            }
            if m.tr.is_delete() {
                delete_found = true;
            }
        }
        assert!(create_found);
        assert!(update_found);
        assert!(delete_found);
    }

    #[test]
    fn match_resources_errors_if_template_not_found() {
        let dpl = DplFile::default();
        // resources to be created because it doesnt have corresponding state entry:
        let r = ResourceSection {
            resource_name: "a".into(),
            template_name: "t".into(),
            input: json_with_positions::parse_json_value(r#"{
                "a": "a"
            }"#).unwrap(),
        };
        // template 't' does not exist. matching to a template should fail:
        let transitionable = vec![TransitionableResource::Create { current_entry: r }];
        let err = match_resources_with_template(transitionable, &dpl).expect_err("it should error");
        assert_eq!(err, "unable to find template 't' referenced by resource 'a'");
    }

    #[test]
    fn match_resources_errors_with_big_scary_error_if_deleted_resources_template_removed() {
        let dpl = DplFile::default();
        // resources to be deleted because it doesnt have corresponding state entry:
        let r = ResourceInState {
            resource_name: "a".to_string(),
            template_name: "t".to_string(),
            ..Default::default()
        };
        // template 't' does not exist. matching to a template should fail:
        let transitionable = vec![TransitionableResource::Delete { state_entry: r }];
        let err = match_resources_with_template(transitionable, &dpl).expect_err("it should error");
        assert!(err.starts_with("\nresource 'a' is to be deleted, but its template 't' does not exist"));
        assert!(err.contains("your options, in order from most recommended to least recommended"));
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

    macro_rules! create_tr {
        ($name: literal; $input: literal) => {
            TrWithTemplate {
                tr: TransitionableResource::Create { current_entry: ResourceSection {
                    resource_name: $name.into(),
                    template_name: "t".into(),
                    input: json_with_positions::parse_json_value($input).unwrap(),
                } },
                template: Default::default()
            }
        };
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

    #[test]
    fn can_eval_json_paths_simple() {
        let state = StateFile::default();
        // nothing to process, it should just give a json object as its input
        let tr = create_tr!("a"; r#"{"a":"b"}"#);
        let input = prepare_tr_for_transition(&tr, &state).expect("it should not error");
        assert_eq!(input, serde_json::json!({"a":"b"}));
    }

    #[test]
    fn can_eval_json_paths_lookup_name() {
        let mut state = StateFile::default();
        state.resources.insert("b".to_string(), Default::default());
        // should simply resolve to the name of the resource "b"
        let tr = create_tr!("a"; r#"{"a": $.b.name}"#);
        let input = prepare_tr_for_transition(&tr, &state).expect("it should not error");
        assert_eq!(input, serde_json::json!({"a":"b"}));
    }

    #[test]
    fn can_eval_json_paths_lookup_input() {
        let mut state = StateFile::default();
        let resource = ResourceInState {
            last_input: serde_json::json!({ "hello": "world" }),
            ..Default::default()
        };
        state.resources.insert("b".to_string(), resource);
        // should resolve to input value of the resource "b"
        let tr = create_tr!("a"; r#"{"a": $.b.input}"#);
        let input = prepare_tr_for_transition(&tr, &state).expect("it should not error");
        assert_eq!(input, serde_json::json!({"a":{"hello":"world"}}));
    }

    #[test]
    fn can_eval_json_paths_lookup_input_deep() {
        let mut state = StateFile::default();
        let resource = ResourceInState {
            last_input: serde_json::json!({ "hello": { "something": ["", "", {"deep": "there"}, ""]} }),
            ..Default::default()
        };
        state.resources.insert("b".to_string(), resource);
        // should resolve to a nested value of the resource "b"
        let tr = create_tr!("a"; r#"{"a": $.b.input.hello.something[2].deep}"#);
        let input = prepare_tr_for_transition(&tr, &state).expect("it should not error");
        assert_eq!(input, serde_json::json!({"a":"there"}));
    }

    #[test]
    fn can_eval_json_paths_lookup_output() {
        let mut state = StateFile::default();
        let resource = ResourceInState {
            output: serde_json::json!({ "hello": "world" }),
            ..Default::default()
        };
        state.resources.insert("b".to_string(), resource);
        // should resolve to output value of the resource "b"
        let tr = create_tr!("a"; r#"{"a": $.b.output}"#);
        let input = prepare_tr_for_transition(&tr, &state).expect("it should not error");
        assert_eq!(input, serde_json::json!({"a":{"hello":"world"}}));
    }

    #[test]
    fn can_eval_json_paths_lookup_output_deep() {
        let mut state = StateFile::default();
        let resource = ResourceInState {
            output: serde_json::json!({ "hello": { "something": ["", "", {"deep": "there"}, ""]} }),
            ..Default::default()
        };
        state.resources.insert("b".to_string(), resource);
        // should resolve to a nested value of the resource "b" output
        let tr = create_tr!("a"; r#"{"a": $.b.output.hello.something[2].deep}"#);
        let input = prepare_tr_for_transition(&tr, &state).expect("it should not error");
        assert_eq!(input, serde_json::json!({"a":"there"}));
    }

    #[test]
    fn can_eval_json_paths_err_if_invalid_path() {
        let mut state = StateFile::default();
        let resource = ResourceInState {
            output: serde_json::json!({ "hello": "world" }),
            ..Default::default()
        };
        state.resources.insert("b".to_string(), resource);
        // should resolve to a nested value of the resource "b" output
        let tr = create_tr!("a"; r#"{"a": $.b.blah.two}"#);
        let err = prepare_tr_for_transition(&tr, &state).expect_err("it should error");
        assert_eq!(err, "cannot create/update resource 'a'. json path string '$.b.blah.two' after resource name 'b' must be 'input', 'output', or 'name'. instead found 'blah'");
    }

    #[test]
    fn can_eval_json_paths_err_if_resource_not_found() {
        let state = StateFile::default();
        let tr = create_tr!("a"; r#"{"a": $.b.blah.two}"#);
        let err = prepare_tr_for_transition(&tr, &state).expect_err("it should error");
        assert_eq!(err, "cannot create/update resource 'a'. json path references 'b' but this resource does not exist in state");
    }

    #[test]
    fn can_eval_json_paths_err_if_invalid_path2() {
        let mut state = StateFile::default();
        let resource = ResourceInState {
            output: serde_json::json!({ "hello": "world" }),
            ..Default::default()
        };
        state.resources.insert("b".to_string(), resource);
        // should resolve to a nested value of the resource "b" output
        let tr = create_tr!("a"; r#"{"a": $.b}"#);
        let err = prepare_tr_for_transition(&tr, &state).expect_err("it should error");
        assert_eq!(err, "cannot create/update resource 'a'. json path string '$.b' does not reference input or output of resource 'b'");
    }

    #[test]
    fn can_eval_json_paths_of_function_call_resources() {
        let mut state = StateFile::default();
        let resource = ResourceInState {
            resource_name: "b".to_string(),
            last_input: serde_json::json!({"b": "b"}),
            output: serde_json::json!({ "hello": "world" }),
            ..Default::default()
        };
        state.resources.insert("b".to_string(), resource);
        // should resolve to the entire input/output/name shape of the resource b
        let tr = create_tr!("__priv_internal_function_resource___myfunc_foo"; r#"{"a": $.b}"#);
        let val = prepare_tr_for_transition(&tr, &state).expect("it should be valid because its resolving a function call");
        assert_eq!(val, serde_json::json!({
            "a": {
                "input": { "b": "b"},
                "output": { "hello": "world" },
                "name": "b"
            }
        }));
    }

    #[test]
    fn can_eval_json_paths_err_if_invalid_path3() {
        let mut state = StateFile::default();
        let resource = ResourceInState {
            output: serde_json::json!({ "hello": "world" }),
            ..Default::default()
        };
        state.resources.insert("b".to_string(), resource);
        // should resolve to a nested value of the resource "b" output
        let tr = create_tr!("a"; r#"{"a": $.b.output[1]}"#);
        let err = prepare_tr_for_transition(&tr, &state).expect_err("it should error");
        assert_eq!(err, "cannot create/update resource 'a'. json path string '$.b.output[1]' error: json path query failed to lookup a value");
    }

    #[test]
    fn can_eval_json_paths_err_if_invalid_path4() {
        let mut state = StateFile::default();
        let resource = ResourceInState {
            output: serde_json::json!({ "hello": "world" }),
            ..Default::default()
        };
        state.resources.insert("b".to_string(), resource);
        // should resolve to a nested value of the resource "b" output
        let tr = create_tr!("a"; r#"{"a": $.b.output.hello.non.existant.path}"#);
        let err = prepare_tr_for_transition(&tr, &state).expect_err("it should error");
        assert_eq!(err, "cannot create/update resource 'a'. json path string '$.b.output.hello.non.existant.path' error: json path query failed to lookup a value");
    }

    #[test]
    fn can_eval_json_paths_to_empty_array() {
        let mut state = StateFile::default();
        let resource = ResourceInState {
            output: serde_json::json!({ "hello": [] }),
            ..Default::default()
        };
        state.resources.insert("b".to_string(), resource);
        // should resolve to a nested value of the resource "b" output
        let tr = create_tr!("a"; r#"{"a": $.b.output.hello}"#);
        let val = prepare_tr_for_transition(&tr, &state).expect("it should not error");
        assert_eq!(val, serde_json::json!({"a": []}));
    }

    #[test]
    fn can_tr_be_transitioned_works() {
        // it doesnt depend on anything
        let tr = create_tr!("a"; r#"{}"#);
        let mut state = StateFile::default();
        let mut dep_map = HashMap::new();
        dep_map.insert("a".to_string(), vec![]);
        assert_eq!(can_tr_be_transitioned(&tr, &dep_map, &state), true);

        // it depends on "b" which isnt done yet, so it should not be able to transition
        let tr = create_tr!("a"; r#"{"b": $.b.output.something}"#);
        dep_map.insert("a".to_string(), vec!["b".to_string()]);
        assert_eq!(can_tr_be_transitioned(&tr, &dep_map, &state), false);

        // now "b" is done in state, a should be able to transition now:
        state.resources.insert("b".to_string(), Default::default());
        assert_eq!(can_tr_be_transitioned(&tr, &dep_map, &state), true);
    }

    #[derive(Default)]
    pub struct VecLogger {
        pub logs: Mutex<Vec<String>>,
    }
    impl VecLogger {
        pub fn leaked() -> &'static VecLogger {
            let boxed = Box::new(VecLogger::default());
            Box::leak(boxed)
        }
        pub fn get_logs(&self) -> Vec<String> {
            self.logs.lock().unwrap().clone()
        }
    }
    impl log::Log for VecLogger {
        fn enabled(&self, _metadata: &log::Metadata) -> bool {
            true
        }
        fn log(&self, record: &log::Record) {
            self.logs.lock().unwrap().push(record.args().to_string());
        }
        fn flush(&self) {}
    }

    #[test]
    fn spawn_all_currently_transitionable_is_noop_for_empty_list() {
        let state = StateFile::default();
        let mut list = vec![];
        let dep_map = HashMap::new();
        let mut task_set = tokio::task::JoinSet::new();
        let logger = VecLogger::leaked();
        spawn_all_currently_transitionable(logger, &state, &mut list, &dep_map, &mut task_set).expect("it should not error");
        assert!(task_set.is_empty());
        assert!(list.is_empty());
    }

    #[tokio::test]
    async fn spawn_all_currently_transitionable_removed_from_list_if_it_can_be_transitioned() {
        let mut state = StateFile::default();
        let mut list = vec![
            create_tr!("a"; r#"{"b": $.b.name}"#)
        ];
        let mut dep_map = HashMap::new();
        dep_map.insert("a".to_string(), vec!["b".to_string()]);
        state.resources.insert("b".to_string(), Default::default());
        let mut task_set = tokio::task::JoinSet::new();
        let logger = VecLogger::leaked();
        spawn_all_currently_transitionable(logger, &state, &mut list, &dep_map, &mut task_set).expect("it should not error");
        assert_eq!(task_set.len(), 1);
        assert!(list.is_empty());
    }

    #[tokio::test]
    async fn perform_update_empty_ok() {
        let logger = VecLogger::leaked();
        let dpl = DplFile::default();
        let state = StateFile::default();
        perform_update(logger, dpl, state).await.expect("it should not error");
    }

    #[tokio::test]
    async fn perform_update_hello_world() {
        let logger = VecLogger::leaked();
        log::set_max_level(log::LevelFilter::Trace);
        let mut dpl = DplFile::default();
        let state = StateFile::default();
        dpl.resources.push(ResourceSection {
            resource_name: "A".into(),
            template_name: "template".into(),
            input: json_with_positions::parse_json_value(r#"{"this": "will be echoed"}"#).unwrap(),
        });
        let mut template = TemplateSection::default();
        template.template_name.s = "template".to_string();
        let mut cli_cmd = CliCommand::default();
        cli_cmd.command.s = "echo".to_string();
        cli_cmd.arg_transforms.push(ArgTransform::Destructure(jsonpath_rust::parser::parse_json_path("$.input").unwrap()));
        template.create.cli_commands.push(CliCommandWithDirectives { cmd: cli_cmd, ..Default::default() });
        dpl.templates.push(template);
        let mut out_state = perform_update(logger, dpl, state).await.expect("it should not error");
        assert_eq!(out_state.resources.len(), 1);
        let a_resource = out_state.resources.remove("A").expect("it should have an output resource 'A'");
        assert_eq!(a_resource.depends_on.len(), 0);
        assert_eq!(a_resource.last_input, serde_json::json!({"this": "will be echoed"}));
        assert_eq!(a_resource.template_name, "template");
        assert_eq!(a_resource.output.as_str().unwrap(), "--this will be echoed\n");
        let logs = logger.get_logs();
        assert_eq!(logs, vec!["creating 'A'", "resource 'A' OK"]);
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
        });
        dpl.resources.push(ResourceSection {
            resource_name: "B".into(),
            template_name: "template".into(),
            input: json_with_positions::parse_json_value(r#"{"resourceA": $.A.output}"#).unwrap(),
        });
        let mut template = TemplateSection::default();
        template.template_name.s = "template".to_string();
        let mut cli_cmd = CliCommand::default();
        cli_cmd.command.s = "echo".to_string();
        cli_cmd.arg_transforms.push(ArgTransform::Destructure(jsonpath_rust::parser::parse_json_path("$.input").unwrap()));
        template.create.cli_commands.push(CliCommandWithDirectives { cmd: cli_cmd, ..Default::default() });
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

    #[tokio::test]
    async fn perform_update_resources_updates_can_become_noops() {
        let logger = VecLogger::leaked();
        log::set_max_level(log::LevelFilter::Trace);
        let mut dpl = DplFile::default();
        let mut state = StateFile::default();
        dpl.resources.push(ResourceSection {
            resource_name: "A".into(),
            template_name: "template".into(),
            input: json_with_positions::parse_json_value(r#"{"this": "will be echoed"}"#).unwrap(),
        });
        // B has been deployed before, it should cause an update
        // but B depends on A, and so we dont know if it should actually be updated or not:
        // in this case, B should NOT be updated because its current input will be resolved
        // (after A completes) to be the exact same as its last input
        dpl.resources.push(ResourceSection {
            resource_name: "B".into(),
            template_name: "template".into(),
            input: json_with_positions::parse_json_value(r#"{"resourceA": $.A.output}"#).unwrap(),
        });
        state.resources.insert("B".to_string(), ResourceInState {
            template_name: "template".to_string(),
            resource_name: "B".to_string(),
            last_input: serde_json::json!({ "resourceA": "--this will be echoed\n" }),
            ..Default::default()
        });
        let mut template = TemplateSection::default();
        template.template_name.s = "template".to_string();
        let mut cli_cmd = CliCommand::default();
        cli_cmd.command.s = "echo".to_string();
        cli_cmd.arg_transforms.push(ArgTransform::Destructure(jsonpath_rust::parser::parse_json_path("$.input").unwrap()));
        template.create.cli_commands.push(CliCommandWithDirectives { cmd: cli_cmd, ..Default::default() });
        template.update = Some(Default::default());
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
        let logs = logger.get_logs();
        assert_eq!(logs, vec!["creating 'A'", "resource 'A' OK", "resource 'B' input has not changed since last transition. returning noop", "resource 'B' OK"]);
    }

    #[tokio::test]
    async fn perform_update_resources_can_do_updates() {
        let logger = VecLogger::leaked();
        log::set_max_level(log::LevelFilter::Trace);
        let mut dpl = DplFile::default();
        let mut state = StateFile::default();
        dpl.resources.push(ResourceSection {
            resource_name: "A".into(),
            template_name: "template".into(),
            input: json_with_positions::parse_json_value(r#"{"this": "will be updated"}"#).unwrap(),
        });
        // B has been deployed before, it should cause an update
        dpl.resources.push(ResourceSection {
            resource_name: "B".into(),
            template_name: "template".into(),
            input: json_with_positions::parse_json_value(r#"{"resourceA": $.A.output}"#).unwrap(),
        });
        state.resources.insert("B".to_string(), ResourceInState {
            template_name: "template".to_string(),
            resource_name: "B".to_string(),
            last_input: serde_json::json!({ "resourceA": "--this will be echoed\n" }),
            ..Default::default()
        });
        let mut template = TemplateSection::default();
        template.template_name.s = "template".to_string();
        let mut cli_cmd = CliCommand::default();
        cli_cmd.command.s = "echo".to_string();
        cli_cmd.arg_transforms.push(ArgTransform::Destructure(jsonpath_rust::parser::parse_json_path("$.input").unwrap()));
        template.create.cli_commands.push(CliCommandWithDirectives { cmd: cli_cmd.clone(), ..Default::default() });
        template.update = Some(Default::default());
        if let Some(upd) = &mut template.update {
            upd.cli_commands.push(CliCommandWithDirectives { cmd: cli_cmd, ..Default::default() });
        }
        dpl.templates.push(template);
        let mut out_state = perform_update(logger, dpl, state).await.expect("it should not error");
        assert_eq!(out_state.resources.len(), 2);
        let a_resource = out_state.resources.remove("A").expect("it should have an output resource 'A'");
        assert_eq!(a_resource.depends_on.len(), 0);
        assert_eq!(a_resource.last_input, serde_json::json!({"this": "will be updated"}));
        assert_eq!(a_resource.template_name, "template");
        assert_eq!(a_resource.output.as_str().unwrap(), "--this will be updated\n");
        let b_resource = out_state.resources.remove("B").expect("it should have an output resource 'B'");
        assert_eq!(b_resource.depends_on, vec!["A"]);
        assert_eq!(b_resource.output.as_str().unwrap(), "--resourceA --this will be updated\n\n");
        let logs = logger.get_logs();
        assert_eq!(logs, vec!["creating 'A'", "resource 'A' OK", "updating 'B'", "resource 'B' OK"]);
    }

    #[tokio::test]
    async fn perform_update_resources_can_do_updates_conditional_commands_none() {
        let logger = VecLogger::leaked();
        log::set_max_level(log::LevelFilter::Trace);
        let mut dpl = deploy_language::parse_and_validate(r#"
template xyz
  create
    echo hello
  update
    @diff $.a
    echo fielda changed
    @diff $.b
    echo fieldb changed
"#).expect("it should be a valid dpl");
        let mut state = StateFile::default();
        dpl.resources.push(ResourceSection {
            resource_name: "resourceA".into(),
            template_name: "xyz".into(),
            input: json_with_positions::parse_json_value(r#"{"a":"a","b":"b", "c":"c"}"#).unwrap(),
        });
        // resourceA has been deployed before, it should cause an update
        // resourceA's last input was unchanged
        state.resources.insert("resourceA".to_string(), ResourceInState {
            template_name: "xyz".to_string(),
            resource_name: "resourceA".to_string(),
            last_input: serde_json::json!({ "a": "a", "b": "b" }),
            ..Default::default()
        });
        let mut out_state = perform_update(logger, dpl, state).await.expect("it should not error");
        assert_eq!(out_state.resources.len(), 1);
        let a_resource = out_state.resources.remove("resourceA").expect("it should have an output resource 'A'");
        assert_eq!(a_resource.depends_on.len(), 0);
        // neither a nor b changed, so none of the echo commands ran in the update
        // and therefore a null value should be the output
        assert_eq!(a_resource.output, serde_json::json!(null));
        let logs = logger.get_logs();
        assert_eq!(logs, vec!["updating 'resourceA'", "resource 'resourceA' OK"]);
    }

    #[tokio::test]
    async fn perform_update_resources_can_do_updates_conditional_commands_only_one() {
        let logger = VecLogger::leaked();
        log::set_max_level(log::LevelFilter::Trace);
        let document = r#"
template xyz
  create
    echo hello
  update
    @diff $.a
    echo { "a": "achanged" }
    @diff $.b
    echo { "b": "bchanged" }
"#;
        let mut dpl = deploy_language::parse_and_validate(document).expect("it should be a valid dpl");
        let mut state = StateFile::default();
        dpl.resources.push(ResourceSection {
            resource_name: "resourceA".into(),
            template_name: "xyz".into(),
            input: json_with_positions::parse_json_value(r#"{"a":"achanged","b":"b"}"#).unwrap(),
        });
        // resourceA has been deployed before, it should cause an update
        state.resources.insert("resourceA".to_string(), ResourceInState {
            template_name: "xyz".to_string(),
            resource_name: "resourceA".to_string(),
            last_input: serde_json::json!({ "a": "a", "b": "b" }),
            ..Default::default()
        });
        let mut out_state = perform_update(logger, dpl, state).await.expect("it should not error");
        assert_eq!(out_state.resources.len(), 1);
        let a_resource = out_state.resources.remove("resourceA").expect("it should have an output resource 'A'");
        assert_eq!(a_resource.depends_on.len(), 0);
        // only a changed, so we should only see echo for a
        assert_eq!(a_resource.output, serde_json::json!({"a": "achanged"}));
        let logs = logger.get_logs();
        assert_eq!(logs, vec!["updating 'resourceA'", "resource 'resourceA' OK"]);

        // run the same test but this time bchanged:
        let logger = VecLogger::leaked();
        let mut dpl = deploy_language::parse_and_validate(document).expect("it should be a valid dpl");
        let mut state = StateFile::default();
        dpl.resources.push(ResourceSection {
            resource_name: "resourceA".into(),
            template_name: "xyz".into(),
            input: json_with_positions::parse_json_value(r#"{"a":"a","b":"bchanged"}"#).unwrap(),
        });
        // resourceA has been deployed before, it should cause an update
        state.resources.insert("resourceA".to_string(), ResourceInState {
            template_name: "xyz".to_string(),
            resource_name: "resourceA".to_string(),
            last_input: serde_json::json!({ "a": "a", "b": "b" }),
            ..Default::default()
        });
        let mut out_state = perform_update(logger, dpl, state).await.expect("it should not error");
        assert_eq!(out_state.resources.len(), 1);
        let a_resource = out_state.resources.remove("resourceA").expect("it should have an output resource 'A'");
        assert_eq!(a_resource.depends_on.len(), 0);
        // only a changed, so we should only see echo for b
        assert_eq!(a_resource.output, serde_json::json!({"b": "bchanged"}));
        let logs = logger.get_logs();
        assert_eq!(logs, vec!["updating 'resourceA'", "resource 'resourceA' OK"]);
    }

    #[tokio::test]
    async fn perform_update_resources_can_do_updates_conditional_commands_both() {
        let logger = VecLogger::leaked();
        log::set_max_level(log::LevelFilter::Trace);
        let document = r#"
template xyz
  create
    echo hello
  update
    @diff $.a
    echo { "a": "achanged" }
    @diff $.b
    echo { "b": "bchanged" }
"#;
        let mut dpl = deploy_language::parse_and_validate(document).expect("it should be a valid dpl");
        let mut state = StateFile::default();
        dpl.resources.push(ResourceSection {
            resource_name: "resourceA".into(),
            template_name: "xyz".into(),
            input: json_with_positions::parse_json_value(r#"{"a":"achanged","b":"bchanged"}"#).unwrap(),
        });
        // resourceA has been deployed before, it should cause an update
        state.resources.insert("resourceA".to_string(), ResourceInState {
            template_name: "xyz".to_string(),
            resource_name: "resourceA".to_string(),
            last_input: serde_json::json!({ "a": "a", "b": "b" }),
            ..Default::default()
        });
        let mut out_state = perform_update(logger, dpl, state).await.expect("it should not error");
        assert_eq!(out_state.resources.len(), 1);
        let a_resource = out_state.resources.remove("resourceA").expect("it should have an output resource 'A'");
        assert_eq!(a_resource.depends_on.len(), 0);
        // both a and b changed, so both the echoes should run
        // and the outputs should be merged:
        assert_eq!(a_resource.output, serde_json::json!({"a": "achanged", "b": "bchanged"}));
        let logs = logger.get_logs();
        assert_eq!(logs, vec!["updating 'resourceA'", "resource 'resourceA' OK"]);
    }

    #[tokio::test]
    async fn perform_update_resources_update_requires_same_template_name() {
        let logger = VecLogger::leaked();
        log::set_max_level(log::LevelFilter::Trace);
        let mut dpl = DplFile::default();
        let mut state = StateFile::default();
        dpl.resources.push(ResourceSection {
            resource_name: "A".into(),
            template_name: "template".into(),
            input: json_with_positions::parse_json_value(r#"{"this": "will be echoed"}"#).unwrap(),
        });
        // B has been deployed before, it should cause an update
        // but B depends on A, and so we dont know if it should actually be updated or not:
        // in this case, B should NOT be updated because its current input will be resolved
        // (after A completes) to be the exact same as its last input
        dpl.resources.push(ResourceSection {
            resource_name: "B".into(),
            template_name: "different template".into(),
            input: json_with_positions::parse_json_value(r#"{"resourceA": $.A.output}"#).unwrap(),
        });
        state.resources.insert("B".to_string(), ResourceInState {
            template_name: "template".to_string(),
            resource_name: "B".to_string(),
            last_input: serde_json::json!({ "resourceA": "--this will be echoed\n" }),
            ..Default::default()
        });
        let mut template = TemplateSection::default();
        template.template_name.s = "template".to_string();
        let mut cli_cmd = CliCommand::default();
        cli_cmd.command.s = "echo".to_string();
        cli_cmd.arg_transforms.push(ArgTransform::Destructure(jsonpath_rust::parser::parse_json_path("$").unwrap()));
        template.create.cli_commands.push(CliCommandWithDirectives { cmd: cli_cmd, ..Default::default() });
        dpl.templates.push(template);
        let err = perform_update(logger, dpl, state).await.expect_err("it should error");
        assert_eq!(err, "resource 'B' currently references template 'different template' but it was previously deployed with template 'template'");
    }

    #[tokio::test]
    async fn perform_update_should_error_if_no_template_section_update() {
        let logger = VecLogger::leaked();
        log::set_max_level(log::LevelFilter::Trace);
        let mut dpl = DplFile::default();
        let mut state = StateFile::default();
        dpl.resources.push(ResourceSection {
            resource_name: "A".into(),
            template_name: "template".into(),
            input: json_with_positions::parse_json_value(r#"{"this": "will be echoed"}"#).unwrap(),
        });
        // A was previously in state, therefore becomes an update
        let mut resource_in_state = ResourceInState::default();
        resource_in_state.template_name = "template".to_string();
        state.resources.insert("A".to_string(), resource_in_state);
        let mut template = TemplateSection::default();
        template.template_name.s = "template".to_string();
        // template does not have an update subsection. it should error because
        // A is due to be updated, but template does not support updates
        dpl.templates.push(template);
        let error = perform_update(logger, dpl, state).await.expect_err("it should error");
        assert!(error.starts_with(r#"resource 'A' is to be updated but template 'template' does not define any update commands"#), "{}", error);
    }

    #[tokio::test]
    async fn perform_update_should_error_if_no_template_section_delete() {
        let logger = VecLogger::leaked();
        log::set_max_level(log::LevelFilter::Trace);
        let mut dpl = DplFile::default();
        let mut state = StateFile::default();
        // A was previously in state, but not in dpl file therefore becomes a delete
        let mut resource_in_state = ResourceInState::default();
        resource_in_state.template_name = "template".to_string();
        resource_in_state.resource_name = "A".to_string();
        state.resources.insert("A".to_string(), resource_in_state);
        let mut template = TemplateSection::default();
        template.template_name.s = "template".to_string();
        // template does not have a delete subsection. it should error because
        // A is due to be deleted, but template does not support deletes
        dpl.templates.push(template);
        let error = perform_update(logger, dpl, state).await.expect_err("it should error");
        assert!(error.starts_with(r#"resource 'A' is to be deleted but template 'template' does not define any delete commands"#), "{}", error);
    }

    macro_rules! delete_tr {
        ($name: literal; $depends_on: expr) => {
            TrWithTemplate {
                tr: TransitionableResource::Delete { state_entry: ResourceInState {
                    resource_name: $name.to_string(),
                    depends_on: $depends_on.into_iter().map(|x: &str| x.to_string()).collect(),
                    ..Default::default()
                }},
                template: TemplateSection::default(),
            }
        };
    }

    #[test]
    fn delete_order_map_works() {
        let a = delete_tr!("A"; []);
        let b = delete_tr!("B"; ["A"]);
        let batch = vec![a, b];
        let order_map = get_delete_order_map(&batch).expect("it should not error");
        let order_map_val = serde_json::to_value(order_map).unwrap();
        assert_eq!(order_map_val, serde_json::json!({"A": ["B"], "B": []}));
    }

    #[test]
    fn delete_order_map_works_transient() {
        // a depends on B, B depends on C, C depends on nothing.
        // if all are to be deleted, then the order should be A, B, C
        // and the order map would be:
        // A: []
        // B: [A]
        // C: [B]
        // the delete order map doesnt need to handle transient dependencies
        let a = delete_tr!("A"; ["B"]);
        let b = delete_tr!("B"; ["C"]);
        let c = delete_tr!("C"; []);
        let batch = vec![a, b, c];
        let order_map = get_delete_order_map(&batch).expect("it should not error");
        let order_map_val = serde_json::to_value(order_map).unwrap();
        assert_eq!(order_map_val, serde_json::json!({"A": [], "B": ["A"], "C": ["B"]}));
    }

    #[tokio::test]
    async fn can_delete_simple() {
        let logger = VecLogger::leaked();
        log::set_max_level(log::LevelFilter::Trace);
        let document = r#"
template xyz
  create
    echo hello
  delete
    echo deleted
"#;
        let dpl = deploy_language::parse_and_validate(document).expect("it should be a valid dpl");
        let mut state = StateFile::default();
        // resourceA has been deployed before, it should cause a delete
        state.resources.insert("resourceA".to_string(), ResourceInState {
            template_name: "xyz".to_string(),
            resource_name: "resourceA".to_string(),
            last_input: serde_json::json!({ "a": "a", "b": "b" }),
            ..Default::default()
        });
        let out_state = perform_update(logger, dpl, state).await.expect("it should not error");
        assert_eq!(out_state.resources.len(), 0);
        let logs = logger.get_logs();
        assert_eq!(logs, vec!["deleting 'resourceA'", "resource 'resourceA' OK"]);
    }

    #[tokio::test]
    async fn can_delete_simple_ordered() {
        let logger = VecLogger::leaked();
        log::set_max_level(log::LevelFilter::Trace);
        let document = r#"
template xyz
  create
    echo hello
  delete
    echo deleted
"#;
        let dpl = deploy_language::parse_and_validate(document).expect("it should be a valid dpl");
        let mut state = StateFile::default();
        // resourceA has been deployed before, it should cause a delete
        state.resources.insert("resourceA".to_string(), ResourceInState {
            template_name: "xyz".to_string(),
            resource_name: "resourceA".to_string(),
            last_input: serde_json::json!({ "a": "a", "b": "b" }),
            ..Default::default()
        });
        // resourceB has been deployed before, and it depended on resourceA
        // resourceB should be deleted first
        state.resources.insert("resourceB".to_string(), ResourceInState {
            template_name: "xyz".to_string(),
            resource_name: "resourceB".to_string(),
            last_input: serde_json::json!({ "a": "a", "b": "b" }),
            depends_on: vec!["resourceA".to_string()],
            ..Default::default()
        });
        let out_state = perform_update(logger, dpl, state).await.expect("it should not error");
        assert_eq!(out_state.resources.len(), 0);
        let logs = logger.get_logs();
        assert_eq!(logs, vec!["deleting 'resourceB'", "resource 'resourceB' OK", "deleting 'resourceA'", "resource 'resourceA' OK"]);
    }

    #[tokio::test]
    async fn deletes_happen_after_create_updates() {
        let logger = VecLogger::leaked();
        log::set_max_level(log::LevelFilter::Trace);
        let document = r#"
template xyz
  create
    echo hello
  delete
    echo deleted

resource xyz(new)
  {"a":"a"}
"#;
        let dpl = deploy_language::parse_and_validate(document).expect("it should be a valid dpl");
        let mut state = StateFile::default();
        // resourceA has been deployed before, and previously depended on "new"
        // but new is not being deleted, so resourceA shouldnt wait for "new" to be deleted
        state.resources.insert("resourceA".to_string(), ResourceInState {
            template_name: "xyz".to_string(),
            resource_name: "resourceA".to_string(),
            last_input: serde_json::json!({ "a": "a", "b": "b" }),
            depends_on: vec!["new".to_string()],
            ..Default::default()
        });
        let out_state = perform_update(logger, dpl, state).await.expect("it should not error");
        assert_eq!(out_state.resources.len(), 1);
        assert!(out_state.resources.contains_key("new"));
        let logs = logger.get_logs();
        assert_eq!(logs, vec![
            "creating 'new'",
            "resource 'new' OK",
            "deleting 'resourceA'",
            "resource 'resourceA' OK"
        ]);
    }

    #[tokio::test]
    async fn templates_can_reference_output_and_accum() {
        let logger = VecLogger::leaked();
        log::set_max_level(log::LevelFilter::Trace);
        let document = r#"
template xyz
  create
    echo hello
  update
    echo
      abc $.output.someval
    echo
      xyz $.accum

resource xyz(resourceA)
    {}
"#;
        let dpl = deploy_language::parse_and_validate(document).expect("it should be a valid dpl");
        let mut state = StateFile::default();
        // resourceA has been deployed before, it should cause an update
        state.resources.insert("resourceA".to_string(), ResourceInState {
            template_name: "xyz".to_string(),
            resource_name: "resourceA".to_string(),
            last_input: serde_json::json!({ "a": "a", "b": "b" }),
            output: serde_json::json!({"someval": "somevalue"}),
            ..Default::default()
        });
        let out_state = perform_update(logger, dpl, state).await.expect("it should not error");
        assert_eq!(out_state.resources.len(), 1);
        let resource_a = out_state.resources.get("resourceA").unwrap();
        // the update runs 2 commands
        // the first echoes "--abc somevalue\n"
        // the second echoes "--xyz $.accum"
        // and accumulator should be updated to contain the value of the first output
        assert_eq!(resource_a.output.as_str().unwrap(), "--xyz --abc somevalue\n\n");
        let logs = logger.get_logs();
        assert_eq!(logs, vec!["updating 'resourceA'", "resource 'resourceA' OK"]);
    }

    #[tokio::test]
    async fn accum_overridden_if_non_object() {
        let logger = VecLogger::leaked();
        log::set_max_level(log::LevelFilter::Trace);
        let document = r#"
template xyz
  create
    echo hello
    echo bye

resource xyz(resourceA)
    {}
"#;
        let dpl = deploy_language::parse_and_validate(document).expect("it should be a valid dpl");
        let state = StateFile::default();
        let out_state = perform_update(logger, dpl, state).await.expect("it should not error");
        assert_eq!(out_state.resources.len(), 1);
        let resource_a = out_state.resources.get("resourceA").unwrap();
        // the output of `echo bye` should override the accum, which was previously
        // the output of `echo hello`
        assert_eq!(resource_a.output.as_str().unwrap(), "bye\n");
        let logs = logger.get_logs();
        assert_eq!(logs, vec!["creating 'resourceA'", "resource 'resourceA' OK"]);
    }

    #[tokio::test]
    async fn templates_can_drop_output_of_specific_command() {
        let logger = VecLogger::leaked();
        log::set_max_level(log::LevelFilter::Trace);
        let document = r#"
template xyz
  create
    echo hello
    @dropoutput
    echo bye

resource xyz(resourceA)
    {}
"#;
        let dpl = deploy_language::parse_and_validate(document).expect("it should be a valid dpl");
        let state = StateFile::default();
        let out_state = perform_update(logger, dpl, state).await.expect("it should not error");
        assert_eq!(out_state.resources.len(), 1);
        let resource_a = out_state.resources.get("resourceA").unwrap();
        // the output of the second echo normally would have overridden the first output
        // but because `echo bye` has @dropoutput it should be dropped
        assert_eq!(resource_a.output.as_str().unwrap(), "hello\n");
        let logs = logger.get_logs();
        assert_eq!(logs, vec!["creating 'resourceA'", "resource 'resourceA' OK"]);
    }

    #[tokio::test]
    async fn templates_can_add_fields_to_accumulator() {
        let logger = VecLogger::leaked();
        log::set_max_level(log::LevelFilter::Trace);
        let document = r#"
template xyz
  create
    # adds "1" to the accumulator at PolicyVersion.VersionId
    @accum [$.accum.PolicyVersion.DefaultVersionId, $.PolicyVersion.VersionId]
    echo { "PolicyVersion" : { "DefaultVersionId": "1" } }

resource xyz(resourceA)
  { "thing": "hello" }
"#;
        let dpl = deploy_language::parse_and_validate(document).expect("it should be a valid dpl");
        let state = StateFile::default();
        let out_state = perform_update(logger, dpl, state).await.expect("it should not error");
        assert_eq!(out_state.resources.len(), 1);
        let resource_a = out_state.resources.get("resourceA").unwrap();
        // the output of a should be what the echo outputted
        // but also with an extra field that was added via the @accum directive
        assert_eq!(resource_a.output, serde_json::json!({"PolicyVersion": { "VersionId": "1", "DefaultVersionId": "1"}}));
        let logs = logger.get_logs();
        assert_eq!(logs, vec!["creating 'resourceA'", "resource 'resourceA' OK"]);
    }

    #[tokio::test]
    async fn templates_can_add_fields_to_accumulator_from_input() {
        let logger = VecLogger::leaked();
        log::set_max_level(log::LevelFilter::Trace);
        let document = r#"
template xyz
  create
    # adds "hello" to the accumulator at PolicyVersion.VersionId
    @accum [$.input.thing, $.PolicyVersion.VersionId]
    echo { "PolicyVersion" : { } }

resource xyz(resourceA)
  { "thing": "hello" }
"#;
        let dpl = deploy_language::parse_and_validate(document).expect("it should be a valid dpl");
        let state = StateFile::default();
        let out_state = perform_update(logger, dpl, state).await.expect("it should not error");
        assert_eq!(out_state.resources.len(), 1);
        let resource_a = out_state.resources.get("resourceA").unwrap();
        // the output of a should be what the echo outputted
        // but also with an extra field that was added via the @accum directive
        assert_eq!(resource_a.output, serde_json::json!({"PolicyVersion": { "VersionId": "hello" }}));
        let logs = logger.get_logs();
        assert_eq!(logs, vec!["creating 'resourceA'", "resource 'resourceA' OK"]);
    }

    #[tokio::test]
    async fn templates_can_add_fields_to_accumulator_from_output() {
        let logger = VecLogger::leaked();
        log::set_max_level(log::LevelFilter::Trace);
        let document = r#"
template xyz
  create
    echo hi
  update
    @accum [$.output.someval, $.PolicyVersion.VersionId]
    echo { "PolicyVersion" : { } }

resource xyz(resourceA)
  { "thing": "hello" }
"#;
        let dpl = deploy_language::parse_and_validate(document).expect("it should be a valid dpl");
        let mut state = StateFile::default();
        // resourceA has been deployed before, it should cause an update
        state.resources.insert("resourceA".to_string(), ResourceInState {
            template_name: "xyz".to_string(),
            resource_name: "resourceA".to_string(),
            last_input: serde_json::json!({ "thing": "b" }),
            output: serde_json::json!({ "someval": "somevalue" }),
            ..Default::default()
        });
        let out_state = perform_update(logger, dpl, state).await.expect("it should not error");
        assert_eq!(out_state.resources.len(), 1);
        let resource_a = out_state.resources.get("resourceA").unwrap();
        // the output of a should be what the echo outputted
        // but also with an extra field that was added via the @accum directive
        assert_eq!(resource_a.output, serde_json::json!({ "someval": "somevalue", "PolicyVersion": { "VersionId": "somevalue" }}));
        let logs = logger.get_logs();
        assert_eq!(logs, vec!["updating 'resourceA'", "resource 'resourceA' OK"]);
    }

    #[tokio::test]
    async fn output_during_update_unchanged() {
        let logger = VecLogger::leaked();
        log::set_max_level(log::LevelFilter::Trace);
        let document = r#"
template xyz
  create
    echo hello
  update
    echo
      abc $.output.someval
    echo
      xyz $.output

resource xyz(resourceA)
    {}
"#;
        let dpl = deploy_language::parse_and_validate(document).expect("it should be a valid dpl");
        let mut state = StateFile::default();
        // resourceA has been deployed before, it should cause an update
        state.resources.insert("resourceA".to_string(), ResourceInState {
            template_name: "xyz".to_string(),
            resource_name: "resourceA".to_string(),
            last_input: serde_json::json!({ "a": "a", "b": "b" }),
            output: serde_json::json!({"someval": "somevalue"}),
            ..Default::default()
        });
        let out_state = perform_update(logger, dpl, state).await.expect("it should not error");
        assert_eq!(out_state.resources.len(), 1);
        let resource_a = out_state.resources.get("resourceA").unwrap();
        // the update runs 2 commands
        // the first echoes "--abc somevalue\n"
        // this becomes the accumulator
        // but the object should still be {"someval": "somevalue"}
        // so on the second command it echoes "--xyz {"someval":"somevalue"}"
        assert_eq!(resource_a.output.as_str().unwrap(), "--xyz {\"someval\":\"somevalue\"}\n");
        let logs = logger.get_logs();
        assert_eq!(logs, vec!["updating 'resourceA'", "resource 'resourceA' OK"]);
    }

    #[tokio::test]
    async fn accum_starts_as_last_output() {
        let logger = VecLogger::leaked();
        log::set_max_level(log::LevelFilter::Trace);
        let document = r#"
template xyz
  create
    echo hello
  update
    echo
      abc $.accum

resource xyz(resourceA)
    {}
"#;
        let dpl = deploy_language::parse_and_validate(document).expect("it should be a valid dpl");
        let mut state = StateFile::default();
        // resourceA has been deployed before, it should cause an update
        state.resources.insert("resourceA".to_string(), ResourceInState {
            template_name: "xyz".to_string(),
            resource_name: "resourceA".to_string(),
            last_input: serde_json::json!({ "a": "a", "b": "b" }),
            output: serde_json::json!({"someval": "somevalue"}),
            ..Default::default()
        });
        let out_state = perform_update(logger, dpl, state).await.expect("it should not error");
        assert_eq!(out_state.resources.len(), 1);
        let resource_a = out_state.resources.get("resourceA").unwrap();
        // the update runs a command
        // that references accum. accum's initial state should be the same as last output
        assert_eq!(resource_a.output.as_str().unwrap(), "--abc {\"someval\":\"somevalue\"}\n");
        let logs = logger.get_logs();
        assert_eq!(logs, vec!["updating 'resourceA'", "resource 'resourceA' OK"]);
    }

    #[tokio::test]
    async fn deletes_can_reference_last_output() {
        let logger = VecLogger::leaked();
        log::set_max_level(log::LevelFilter::Trace);
        let document = r#"
template xyz
  create
    echo hello
  delete
    rm
      force $.output.filename
"#;
        let dpl = deploy_language::parse_and_validate(document).expect("it should be a valid dpl");
        let mut state = StateFile::default();
        // resourceA has been deployed before, it should cause a delete
        state.resources.insert("resourceA".to_string(), ResourceInState {
            template_name: "xyz".to_string(),
            resource_name: "resourceA".to_string(),
            last_input: serde_json::json!({ "a": "a", "b": "b" }),
            output: serde_json::json!({"filename": "blah.txt"}),
            ..Default::default()
        });
        std::fs::write("blah.txt", "").unwrap();
        let file = PathBuf::from("blah.txt");
        assert!(file.exists());
        let out_state = perform_update(logger, dpl, state).await.expect("it should not error");
        assert!(!file.exists());
        assert_eq!(out_state.resources.len(), 0);
        let logs = logger.get_logs();
        assert_eq!(logs, vec!["deleting 'resourceA'", "resource 'resourceA' OK"]);
    }

    #[test]
    fn get_transitionable_resources_can_check_for_already_done_updates() {
        // some resources depend on other resources, and we treat them as being updated (if they had prior
        // state entries) since we cannot know if the input has changed ahead of time.
        // this is the general case, but there's a special case which is when resource A depends on resource B
        // but resource B is already done and doesnt need to be updated. in such a case, we should return
        // no transitionable resources

        let document = r#"
template some_template
  create
    echo hello

resource some_template(A)
    {}

resource some_template(B)
    {"x": $.A.output}
"#;
        let mut dpl = deploy_language::parse_and_validate(document).expect("it should be a valid dpl");
        let mut state = StateFile::default();
        // both A and B were already in state:
        for resource_name in ["A", "B"] {
            let mut resource = ResourceInState::default();
            resource.resource_name = resource_name.to_string();
            resource.template_name = "some_template".to_string();
            if resource_name == "B" {
                resource.last_input = serde_json::json!({"x": "hello"});
            } else {
                resource.output = serde_json::json!("hello");
                resource.last_input = serde_json::json!({});
            }
            state.resources.insert(resource_name.to_string(), resource);
        }

        // should be empty because resource B depends on A, but A doesnt need to be updated, and therefore
        // its value is the same
        let trs = get_transitionable_resources(&mut state, &mut dpl);
        assert!(trs.is_empty());
    }

    #[test]
    fn get_transitionable_resources_can_check_for_already_done_updates_and_still_cause_update() {
        // same situation as above test, but the input actually does differ
        // and thus we should consider B an update

        let document = r#"
template some_template
  create
    echo hello

resource some_template(A)
    {}

resource some_template(B)
    {"y": $.A.output}
"#;
        let mut dpl = deploy_language::parse_and_validate(document).expect("it should be a valid dpl");
        let mut state = StateFile::default();
        // both A and B were already in state:
        for resource_name in ["A", "B"] {
            let mut resource = ResourceInState::default();
            resource.resource_name = resource_name.to_string();
            resource.template_name = "some_template".to_string();
            if resource_name == "B" {
                resource.last_input = serde_json::json!({"x": "hello"});
            } else {
                resource.output = serde_json::json!("hello");
                resource.last_input = serde_json::json!({});
            }
            state.resources.insert(resource_name.to_string(), resource);
        }

        // should cause an update because B's current input is {"y": "hello"}
        // and its last input was {"x": "hello"}. we still had to look up resource A
        // to be able to do this comparison.
        // TODO: mini optimization here is before doing the expensive lookup, try to check if
        // B's keys are all the same
        let mut trs = get_transitionable_resources(&mut state, &mut dpl);
        assert_eq!(trs.len(), 1);
        let resource_b_tr = trs.remove(0);
        assert_eq!(resource_b_tr.get_resource_name(), "B");
        assert_eq!(resource_b_tr.is_update(), true);
    }

    #[tokio::test]
    async fn dependency_map_update_works_for_already_done_resources() {
        let document = r#"
template aws_s3_bucket
  create
    echo hi
  update
    echo hi

template aws_iam_policy
  create
    echo bye
  update
    echo bye

resource aws_iam_policy(my_policy)
  {
    "policy": $.my_s3_bucket.input.bucket
  }

resource aws_s3_bucket(my_s3_bucket)
  {
    "bucket": "xyz"
  }
"#;
        let state = StateFile::default();
        let dpl = deploy_language::parse_and_validate(document).expect("it should be a valid dpl");
        let logger = VecLogger::leaked();
        log::set_max_level(log::LevelFilter::Trace);
        // we run deploy once on an empty state and it works fine
        let state = perform_update(logger, dpl, state).await.expect("initial deploy should work");
        assert_eq!(logger.get_logs(), vec!["creating 'my_s3_bucket'", "resource 'my_s3_bucket' OK", "creating 'my_policy'", "resource 'my_policy' OK"]);
        // but now we update the dpl, and resource "my_policy" points to the output, rather than the input of "my_s3_bucket"
        let document = r#"
template aws_s3_bucket
  create
    echo hi
  update
    echo hi

template aws_iam_policy
  create
    echo bye
  update
    echo bye

resource aws_iam_policy(my_policy)
  {
    "policy": $.my_s3_bucket.output
  }

resource aws_s3_bucket(my_s3_bucket)
  {
    "bucket": "xyz"
  }
"#;
        let dpl = deploy_language::parse_and_validate(document).expect("it should be a valid dpl");
        // this should still succeed but should only update my_policy
        let logger = VecLogger::leaked();
        let state = perform_update(logger, dpl, state).await.expect("initial deploy should work");
        assert!(state.resources.contains_key("my_policy"));
        assert!(state.resources.contains_key("my_s3_bucket"));
        assert_eq!(logger.get_logs(), vec!["updating 'my_policy'", "resource 'my_policy' OK"]);
    }

    #[test]
    fn no_fake_resources_if_no_function_calls() {
        let document = r#"
template hello
  create
    echo hi

resource hello(r1)
    {}
"#;
        let mut dpl = deploy_language::parse_and_validate(document).expect("it should be a valid dpl");
        let mut state = StateFile::default();
        let mut transitionable_resources = get_transitionable_resources(&mut state, &mut dpl);
        assert_eq!(transitionable_resources.len(), 1);
        insert_function_resources(&dpl, &state, &mut transitionable_resources).unwrap();
        assert_eq!(transitionable_resources.len(), 1);
    }

    #[test]
    fn can_add_fake_resources_for_fn_calls() {
        let document = r#"
template hello
  create
    echo hi

resource hello(r2)
    {}

function javascript(myfunc)
  console.log(1);

resource hello(r1)
    {"a": $.myfunc['r2']}
"#;
        let mut dpl = deploy_language::parse_and_validate(document).expect("it should be a valid dpl");
        let mut state = StateFile::default();
        let mut transitionable_resources = get_transitionable_resources(&mut state, &mut dpl);
        assert_eq!(transitionable_resources.len(), 2);
        insert_function_resources(&dpl, &state, &mut transitionable_resources).unwrap();
        assert_eq!(transitionable_resources.len(), 3);
        let fake_resource = transitionable_resources.iter().find(|x| x.get_template_name().unwrap() == FUNCTION_RESOURCE_PREFIX).unwrap();
        assert_matches!(fake_resource, TransitionableResource::Create { current_entry } => {
            assert!(current_entry.resource_name.s.starts_with(FUNCTION_RESOURCE_PREFIX));
            let val = current_entry.input.clone().to_serde_json_value();
            assert_eq!(val, serde_json::json!({
                "depends_on": { "__DPL_PATH_QUERY_PRIVATE_FIELD_DO_NOT_USE__": "$.r2" },
                "function_name": "myfunc",
                "function_type": "javascript",
                "function_body": "console.log(1);"
            }));
        });
    }

    #[test]
    fn can_add_fake_resources_for_fn_calls_with_update() {
        let document = r#"
template hello
  create
    echo hi

resource hello(r2)
    {}

function javascript(myfunc)
  console.log(1);

resource hello(r1)
    {"a": $.myfunc['r2']}
"#;
        let mut dpl = deploy_language::parse_and_validate(document).expect("it should be a valid dpl");
        let mut state = StateFile::default();
        // r2 already exists in state and has not changed:
        state.resources.insert("r2".to_string(), ResourceInState {
            resource_name: "r2".to_string(),
            template_name: "hello".to_string(),
            last_input: serde_json::json!({}),
            ..Default::default()
        });
        let mut transitionable_resources = get_transitionable_resources(&mut state, &mut dpl);
        assert_eq!(transitionable_resources.len(), 1);
        insert_function_resources(&dpl, &state, &mut transitionable_resources).unwrap();
        assert_eq!(transitionable_resources.len(), 2);
        let fake_resource = transitionable_resources.iter().find(|x| x.get_template_name().unwrap() == FUNCTION_RESOURCE_PREFIX).unwrap();
        assert_matches!(fake_resource, TransitionableResource::Create { current_entry } => {
            assert!(current_entry.resource_name.s.starts_with(FUNCTION_RESOURCE_PREFIX));
            let val = current_entry.input.clone().to_serde_json_value();
            assert_eq!(val, serde_json::json!({
                "depends_on": { "__DPL_PATH_QUERY_PRIVATE_FIELD_DO_NOT_USE__": "$.r2" },
                "function_name": "myfunc",
                "function_type": "javascript",
                "function_body": "console.log(1);"
            }));
        });
    }

    #[test]
    fn duplicate_function_calls_treated_as_one_resource() {
        let document = r#"
template hello
  create
    echo hi

resource hello(r3)
    {}

function javascript(myfunc)
  console.log(1);

resource hello(r1)
    {"a": $.myfunc['r3']}

resource hello(r2)
    {"a": $.myfunc['r3']}
"#;
        // same as the above test, but this tests that if multiple resources call the same function
        // with the same input resource, then that only creates 1 extra resource
        let mut dpl = deploy_language::parse_and_validate(document).expect("it should be a valid dpl");
        let mut state = StateFile::default();
        let mut transitionable_resources = get_transitionable_resources(&mut state, &mut dpl);
        assert_eq!(transitionable_resources.len(), 3);
        insert_function_resources(&dpl, &state,&mut transitionable_resources).unwrap();
        assert_eq!(transitionable_resources.len(), 4);
        let fake_resource = transitionable_resources.iter().find(|x| x.get_template_name().unwrap() == FUNCTION_RESOURCE_PREFIX).unwrap();
        assert_matches!(fake_resource, TransitionableResource::Create { current_entry } => {
            assert!(current_entry.resource_name.s.starts_with(FUNCTION_RESOURCE_PREFIX));
            let val = current_entry.input.clone().to_serde_json_value();
            assert_eq!(val, serde_json::json!({
                "depends_on": { "__DPL_PATH_QUERY_PRIVATE_FIELD_DO_NOT_USE__": "$.r3" },
                "function_name": "myfunc",
                "function_type": "javascript",
                "function_body": "console.log(1);"
            }));
        });
    }

    #[tokio::test]
    async fn can_run_resources_with_function_reference() {
        let document = r#"
template some_template
  create
    echo hi

function javascript(myfunc)
  return "eee"

resource some_template(foo)
  {
    "foo": "foo"
  }

resource some_template(bar)
  {
    "bar": $.myfunc['foo']
  }
"#;
        let state = StateFile::default();
        let dpl = deploy_language::parse_and_validate(document).expect("it should be a valid dpl");
        let logger = VecLogger::leaked();
        log::set_max_level(log::LevelFilter::Trace);
        // we run deploy once on an empty state and it works fine
        let mut state = perform_update(logger, dpl, state).await.expect("initial deploy should work");
        assert_eq!(state.resources.len(), 2);
        let foo = state.resources.remove("foo").unwrap();
        assert_eq!(foo.last_input, serde_json::json!({"foo": "foo"}));
        assert!(foo.depends_on.is_empty());
        let bar = state.resources.remove("bar").unwrap();
        // the bar field calls function 'myfunc' with the entire shape of resource 'foo'.
        // the function 'myfunc' simply returns the string 'eee' so the value of
        // the bar field should be 'eee'
        assert_eq!(bar.last_input, serde_json::json!({"bar": "eee"}));
        // it should depend on foo since it calls a function and passes foo in
        assert_eq!(bar.depends_on, vec!["foo"]);
        assert_eq!(logger.get_logs(), vec!["creating 'foo'", "resource 'foo' OK", "calling function 'myfunc(foo)'", "function call 'myfunc(foo)' OK", "creating 'bar'", "resource 'bar' OK"]);
    }

    #[tokio::test]
    async fn can_run_resources_with_function_reference_manipulate_input() {
        let document = r#"
template some_template
  create
    echo hi

function javascript(myfunc)
  return input.foo;

resource some_template(foo)
  {
    "foo": "foo2"
  }

resource some_template(bar)
  {
    "bar": $.myfunc['foo']
  }
"#;
        let state = StateFile::default();
        let dpl = deploy_language::parse_and_validate(document).expect("it should be a valid dpl");
        let logger = VecLogger::leaked();
        log::set_max_level(log::LevelFilter::Trace);
        // we run deploy once on an empty state and it works fine
        let mut state = perform_update(logger, dpl, state).await.expect("initial deploy should work");
        assert_eq!(state.resources.len(), 2);
        let foo = state.resources.remove("foo").unwrap();
        assert_eq!(foo.last_input, serde_json::json!({"foo": "foo2"}));
        assert!(foo.depends_on.is_empty());
        let bar = state.resources.remove("bar").unwrap();
        // the bar field calls function 'myfunc' with the entire shape of resource 'foo'.
        // the function 'myfunc' accesses the value of foo.input.foo
        // which should resolve to "foo2"
        assert_eq!(bar.last_input, serde_json::json!({"bar": "foo2"}));
        // it should depend on foo since it calls a function and passes foo in
        assert_eq!(bar.depends_on, vec!["foo"]);
        assert_eq!(logger.get_logs(), vec!["creating 'foo'", "resource 'foo' OK", "calling function 'myfunc(foo)'", "function call 'myfunc(foo)' OK", "creating 'bar'", "resource 'bar' OK"]);
    }

    #[tokio::test]
    async fn can_run_resources_with_function_reference_manipulate_advanced() {
        let document = r#"
template some_template
  create
    echo {}

function javascript(myfunc)
  return `${JSON.stringify(output)}${name}`

resource some_template(foo)
  {
    "foo": "foo2"
  }

resource some_template(bar)
  {
    "bar": $.myfunc['foo']
  }
"#;
        let state = StateFile::default();
        let dpl = deploy_language::parse_and_validate(document).expect("it should be a valid dpl");
        let logger = VecLogger::leaked();
        log::set_max_level(log::LevelFilter::Trace);
        // we run deploy once on an empty state and it works fine
        let mut state = perform_update(logger, dpl, state).await.expect("initial deploy should work");
        assert_eq!(state.resources.len(), 2);
        let foo = state.resources.remove("foo").unwrap();
        assert_eq!(foo.last_input, serde_json::json!({"foo": "foo2"}));
        assert!(foo.depends_on.is_empty());
        let bar = state.resources.remove("bar").unwrap();
        // the bar field calls function 'myfunc' with the entire shape of resource 'foo'.
        // the function 'myfunc' simply concatenates the output of foo with the name 'foo'
        assert_eq!(bar.last_input, serde_json::json!({"bar": "{}foo"}));
        // it should depend on foo since it calls a function and passes foo in
        assert_eq!(bar.depends_on, vec!["foo"]);
        assert_eq!(logger.get_logs(), vec!["creating 'foo'", "resource 'foo' OK", "calling function 'myfunc(foo)'", "function call 'myfunc(foo)' OK", "creating 'bar'", "resource 'bar' OK"]);
    }
}

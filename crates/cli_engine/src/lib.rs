use std::collections::HashMap;

use dcl_language::{parse::{resource::ResourceSection, template::TemplateSection, SpannedDiagnostic}, DclFile};
use jsonpath_rust::{parser::model::JpQuery, query::{js_path_process, state::State, Query}};
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Default)]
pub struct StateFile {
    /// reserved root field for later metadata, currently unused and values will show up as null
    #[serde(default)]
    pub dcl_metadata: serde_json::Value,
    /// map of resources and their state representation.
    /// note the key is the resource name, thus must be unique across all resources.
    /// also note the ResourceInState also contains the resource name for convenience
    pub resources: HashMap<String, ResourceInState>,
}

// TODO: need tp update DclFile definitions to preserve section line positions.
// current reporting all errors on line 0 :/
/// loads the state file and parses as a json object. if the state file doesnt exist, it will be
/// created, and treated as an empty {} object
pub fn load_state(dcl: &DclFile) -> Result<StateFile, SpannedDiagnostic> {
    let state = dcl.state.as_ref().ok_or("no state file provided")
        .map_err(|e| SpannedDiagnostic::new(e, 0, 999))?;
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
                return Ok(default_statefile);
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
    Ok(out)
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
    /// a resource is Createable if it exists in the current .dcl file
    /// but has no prior state entry
    Create { current_entry: ResourceSection },
    /// a resource is Updaeable if it exists in the current .dcl file
    /// and it also has a prior state entry. Note: the existance of an Update TransitionableResource
    /// implies that it's current input and last input differ. if a resource's current input is exactly the same
    /// as its input from the state entry, it should not be represented here. There is a caveat which is
    /// resources whose inputs have a json path, which implies we cannot yet compare their input, as it has not been resolved yet.
    /// these resources may be unchanged, but they will be represented as an update temporarily until their dependencies finish
    /// and then this resource's input can be represented as a serde_json::Value, at which point
    /// we can then compare the current input against the last state input
    Update { state_entry: ResourceInState, current_entry: ResourceSection },
    /// a resource is Deleteable if it does not exist in the current .dcl file
    /// but it has a prior state entry. for this reason the state entry must preserve the template name
    /// that this resource applies to. we can then look up the template and run its deletion lifecycle command(s).
    /// it is required that the template exists, otherwise it should be a runtime error to indicate to the user
    /// that they must add back the template, or otherwise accept to have a detached resource no longer managed by dcl
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
                        "Resource '{}' currently references template '{}' but it was previously deployed with template '{}'",
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
/// or deleted (any state entries that remain that dont have a corresponding resource in the dcl file. these are resources
/// that were previously created and now must be deleted).
/// after calling this function, the state's resources will only contain no-op resources whose input has not changed
pub fn get_transitionable_resources(
    state: &mut StateFile,
    dcl: &mut DclFile,
) -> Vec<TransitionableResource> {
    // first collect the known resources that are to be created or updated:
    let mut out = Vec::with_capacity(state.resources.len());
    let mut done_resources = HashMap::new();
    for resource in dcl.resources.drain(..) {
        match state.resources.remove(&resource.resource_name) {
            Some(state_entry) => {
                // if we can determine this resource has not changed, then we can omit it
                // from the transitionable resources:
                if resource_input_has_been_changed(&resource, &state_entry) {
                    out.push(TransitionableResource::Update { state_entry, current_entry: resource });
                } else {
                    // otherwise its input has not changed, dont add it to the
                    // list of transitionable resources. instead, treat it as
                    // done so other resources can read its output
                    done_resources.insert(state_entry.resource_name.clone(), state_entry);
                }
            }
            None => {
                // this resource is to be created since there's no corresponding state entry
                out.push(TransitionableResource::Create { current_entry: resource });
            }
        }
    }
    // now, check all of the resources in the state file that do not have corresponding
    // entry in the current dcl file, these resources are to be deleted:
    for (_, state_entry) in state.resources.drain() {
        out.push(TransitionableResource::Delete { state_entry })
    }
    // afterwards, put back all of the resources that had no-op updates into the state
    // such that other resources can look up their outputs:
    state.resources = done_resources;
    out
}

/// returns true if:
/// - the current resource has a json path
/// - or if the current resoure can be represented as serde_json::Value and it differs from the previous state entry
pub fn resource_input_has_been_changed(current: &ResourceSection, previous: &ResourceInState) -> bool {
    let current_input = match current.input.to_serde_json_value_pure() {
        Some(v) => v,
        None => {
            // current input has json paths, return true
            // since we cant know if it has changed or not yet
            return true
        }
    };
    return current_input != previous.last_input
}

pub fn match_resources_with_template(mut transitionable: Vec<TransitionableResource>, dcl: &DclFile) -> Result<Vec<TrWithTemplate>, String> {
    let mut out = Vec::with_capacity(transitionable.len());
    for tr in transitionable.drain(..) {
        let template_name = tr.get_template_name()?;
        let resource_name = tr.get_resource_name();
        let is_delete = tr.is_delete();
        let template = dcl.templates.iter()
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
resource '{}' is to be deleted, but its template '{}' does not exist in the current DCL file.
the following are your options, in order from most recommended to least recommended:
1. if you edited the DCL file and removed template '{}', simply add that template back, and run deploy again.
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

/// returns a list of dependencies that are immediate: this current resource depends on resource A, B, C, ...
/// dependencies are other resource names from json paths that are referenced by this current resource in its input json
pub fn get_immediate_dependencies(current: &TrWithTemplate) -> Result<Vec<String>, String> {
    let mut immediate_deps = match &current.tr {
        TransitionableResource::Create { current_entry } |
        TransitionableResource::Update { current_entry, .. } => {
            // TODO: parse and extract just the first segment
            let all_json_paths = current_entry.input.get_all_json_paths();
            let num_json_paths = all_json_paths.len();
            let json_paths = all_json_paths
                .iter()
                // we ignore errors here, only returning the successfully parsed json paths
                // because that validation should have happened already by the dcl_language crate
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
            out
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

pub fn get_all_transient_dependencies_map(trs: &[TrWithTemplate]) -> Result<HashMap<String, Vec<String>>, String> {
    // first, collect a map of all immediate dependencies:
    let mut immediate_dep_map = HashMap::with_capacity(trs.len());
    for tr in trs {
        let immediate_deps = get_immediate_dependencies(tr)?;
        immediate_dep_map.insert(tr.tr.get_resource_name().to_string(), immediate_deps);
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

pub fn get_all_transient_dependencies(current: &TrWithTemplate, trs: &[TrWithTemplate]) -> Result<Vec<String>, String> {
    let mut map = get_all_transient_dependencies_map(trs)?;
    map.remove(current.tr.get_resource_name()).ok_or_else(|| format!("resource '{}' not found in map of all resources", current.tr.get_resource_name()))
}

pub async fn perform_update(
    logger: &'static dyn log::Log,
    mut dcl: DclFile,
    mut state: StateFile,
) -> Result<StateFile, String> {
    // first, collect resources into create/update/or delete, discarding
    // any resources that dont need to be updated
    let transitionable_resources = get_transitionable_resources(&mut state, &mut dcl);
    // next, ensure every resource can be matched with a template. error otherwise:
    let mut transitionable_resources = match_resources_with_template(transitionable_resources, &dcl)?;
    // get map of names of resources to a flat list of all transient dependencies of that resource
    let dependency_map = get_all_transient_dependencies_map(&transitionable_resources)?;
    // split out deletes
    let deletes: Vec<TrWithTemplate> = transitionable_resources.extract_if(.., |tr| tr.tr.is_delete()).collect();
    let create_or_updates = transitionable_resources;
    // process creates/updates first then deletes
    perform_update_batch(logger, &mut state, create_or_updates, &dependency_map).await?;
    perform_update_batch(logger, &mut state, deletes, &dependency_map).await?;
    Ok(state)
}

pub async fn perform_update_batch(
    logger: &'static dyn log::Log,
    state: &mut StateFile,
    mut batch: Vec<TrWithTemplate>,
    dep_map: &HashMap<String, Vec<String>>,
) -> Result<(), String> {
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
        let res = res?;
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
    task_set: &mut tokio::task::JoinSet<Result<ResourceInState, String>>,
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
    let current_input = current_json.to_serde_json_value_with_replace_func(&mut |s| {
        let mut jpq = jsonpath_rust::parser::parse_json_path(s)
            .map_err(|e| format!(
                "cannot create/update resource '{}'. invalid json path string '{}' error: {:?}",
                tr.tr.get_resource_name(), s, e
            ))?;
        // the first segment in the query should correspond to an existing resource in the state file
        let first_seg = if jpq.segments.len() == 0 {
            return Err(format!("cannot create/update resource '{}'. json path string '{}' does not reference anything", tr.tr.get_resource_name(), s));
        } else {
            jpq.segments.remove(0)
        };
        let resource_name = get_resource_name_from_segment(tr.tr.get_resource_name(), &first_seg)?;
        // look it up in state
        let resource = state.resources.get(&resource_name).ok_or_else(|| format!("cannot create/update resource '{}'. json path references '{}' but this resource does not exist in state", tr.tr.get_resource_name(), resource_name))?;
        let next_seg = if jpq.segments.len() == 0 {
            return Err(format!("cannot create/update resource '{}'. json path string '{}' does not reference input or output of resource '{}'", tr.tr.get_resource_name(), s, resource_name))
        } else {
            jpq.segments.remove(0)
        };
        let input_or_output = get_resource_name_from_segment(tr.tr.get_resource_name(), &next_seg)?;
        let value_to_lookup = match input_or_output.as_str() {
            "input" => &resource.last_input,
            "output" => &resource.output,
            "name" => return Ok(serde_json::Value::String(resource_name)),
            x => return Err(format!("cannot create/update resource '{}'. json path string '{}' after resource name '{}' must be 'input', 'output', or 'name'. instead found '{}'", tr.tr.get_resource_name(), s, resource_name, x))
        };
        // now that we have removed the first few segments of the json path, we can use jsonpath_rust to
        // query the serde value:
        let mut result_vals = process_json_path_query(&jpq, value_to_lookup).map_err(|e| {
            format!("cannot create/update resource '{}'. json path string '{}' error: {}", tr.tr.get_resource_name(), s, e)
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
    })?;

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
    dependencies: Vec<String>
) -> Result<ResourceInState, String> {
    // // first check if this is an update:
    // if let TransitionableResource::Update { state_entry, .. } = &tr.tr {
    //     // if the input is the same (now that it's been able to be resolved)
    //     // we can exit early. we still want to return its state such that our caller
    //     // adds it to the statefile and this can be considered done.
    //     if state_entry.last_input == current_input {
    //         let mut state_entry = state_entry.clone();
    //         state_entry.depends_on = dependencies;
    //         return Ok(state_entry)
    //     }
    // }

    todo!()
}

#[cfg(test)]
mod test {
    use std::sync::Mutex;

    use super::*;
    use assert_matches::assert_matches;

    #[test]
    fn can_determine_createable_resources() {
        let mut dcl = DclFile::default();
        let mut state = StateFile::default();
        dcl.resources.push(ResourceSection {
            resource_name: "a".to_string(),
            template_name: "t".to_string(),
            input: json_with_positions::parse_json_value(r#"{
                "a": "a"
            }"#).unwrap(),
        });
        // no prior state for resource 'a' so it should be created:
        let mut transitionable = get_transitionable_resources(&mut state, &mut dcl);
        assert_eq!(transitionable.len(), 1);
        let resource = transitionable.pop().unwrap();
        assert_matches!(resource, TransitionableResource::Create { .. });
        assert_eq!(state.resources.len(), 0);
    }

    #[test]
    fn can_determine_deleteable_resources() {
        let mut dcl = DclFile::default();
        let mut state = StateFile::default();
        state.resources.insert("a".to_string(), ResourceInState {
            resource_name: "a".to_string(),
            template_name: "t".to_string(),
            last_input: serde_json::json!({"a": "a"}),
            ..Default::default()
        });
        // no current entry for resource 'a', but state does have a prior entry for resource 'a'
        // so it should be deleted
        let mut transitionable = get_transitionable_resources(&mut state, &mut dcl);
        assert_eq!(transitionable.len(), 1);
        let resource = transitionable.pop().unwrap();
        assert_matches!(resource, TransitionableResource::Delete { .. });
        assert_eq!(state.resources.len(), 0);
    }

    #[test]
    fn can_determine_updateable_resources() {
        let mut dcl = DclFile::default();
        let mut state = StateFile::default();
        dcl.resources.push(ResourceSection {
            resource_name: "a".to_string(),
            template_name: "t".to_string(),
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
        let mut transitionable = get_transitionable_resources(&mut state, &mut dcl);
        assert_eq!(transitionable.len(), 1);
        let resource = transitionable.pop().unwrap();
        assert_matches!(resource, TransitionableResource::Update { .. });
        assert_eq!(state.resources.len(), 0);
    }

    #[test]
    fn can_determine_updateable_resources_due_to_json_path() {
        let mut dcl = DclFile::default();
        let mut state = StateFile::default();
        dcl.resources.push(ResourceSection {
            resource_name: "a".to_string(),
            template_name: "t".to_string(),
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
        let mut transitionable = get_transitionable_resources(&mut state, &mut dcl);
        assert_eq!(transitionable.len(), 1);
        let resource = transitionable.pop().unwrap();
        assert_matches!(resource, TransitionableResource::Update { .. });
        assert_eq!(state.resources.len(), 0);
    }

    #[test]
    fn can_determine_noop_updates_if_input_unchanged() {
        let mut dcl = DclFile::default();
        let mut state = StateFile::default();
        dcl.resources.push(ResourceSection {
            resource_name: "a".to_string(),
            template_name: "t".to_string(),
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
        let transitionable = get_transitionable_resources(&mut state, &mut dcl);
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
        let mut dcl = DclFile::default();
        let mut state = StateFile::default();
        // dcl file has 1 template 't'
        let mut template = TemplateSection::default();
        template.template_name.s = "t".to_string();
        dcl.templates.push(template);
        // and 3 resources. one to be created, one to be updated, and one to be deleted.
        // all point to template 't':

        // resources to be created because it doesnt have corresponding state entry:
        dcl.resources.push(ResourceSection {
            resource_name: "a".to_string(),
            template_name: "t".to_string(),
            input: json_with_positions::parse_json_value(r#"{
                "a": "a"
            }"#).unwrap(),
        });
        // resource to be updated because its state entry differs from current:
        dcl.resources.push(ResourceSection {
            resource_name: "b".to_string(),
            template_name: "t".to_string(),
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
        let transitionable = get_transitionable_resources(&mut state, &mut dcl);
        let matched = match_resources_with_template(transitionable, &dcl).expect("should not error");
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
        let dcl = DclFile::default();
        // resources to be created because it doesnt have corresponding state entry:
        let r = ResourceSection {
            resource_name: "a".to_string(),
            template_name: "t".to_string(),
            input: json_with_positions::parse_json_value(r#"{
                "a": "a"
            }"#).unwrap(),
        };
        // template 't' does not exist. matching to a template should fail:
        let transitionable = vec![TransitionableResource::Create { current_entry: r }];
        let err = match_resources_with_template(transitionable, &dcl).expect_err("it should error");
        assert_eq!(err, "unable to find template 't' referenced by resource 'a'");
    }

    #[test]
    fn match_resources_errors_with_big_scary_error_if_deleted_resources_template_removed() {
        let dcl = DclFile::default();
        // resources to be deleted because it doesnt have corresponding state entry:
        let r = ResourceInState {
            resource_name: "a".to_string(),
            template_name: "t".to_string(),
            ..Default::default()
        };
        // template 't' does not exist. matching to a template should fail:
        let transitionable = vec![TransitionableResource::Delete { state_entry: r }];
        let err = match_resources_with_template(transitionable, &dcl).expect_err("it should error");
        assert!(err.starts_with("\nresource 'a' is to be deleted, but its template 't' does not exist"));
        assert!(err.contains("your options, in order from most recommended to least recommended"));
    }

    #[test]
    fn can_get_immediate_dependencies_for_create() {
        let current = TrWithTemplate {
            tr: TransitionableResource::Create {
                current_entry: ResourceSection {
                    resource_name: "a".to_string(),
                    template_name: "template".to_string(),
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
                    resource_name: "a".to_string(),
                    template_name: "template".to_string(),
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
                    resource_name: "a".to_string(),
                    template_name: "template".to_string(),
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
                    resource_name: "a".to_string(),
                    template_name: "template".to_string(),
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
                    resource_name: "a".to_string(),
                    template_name: "template".to_string(),
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
                    resource_name: $name.to_string(),
                    template_name: "t".to_string(),
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
        let a = create_tr!("a"; r#"{"1": $.b, "2": $.c}"#);
        let b = create_tr!("b"; r#"{"1": $.c}"#);
        let c = create_tr!("c"; r#"{}"#);
        let trs = [a, b, c];
        let current = &trs[0];
        // A depends on B and C
        // B depends on C
        // C depends on nothing.
        // this should be valid, and all transient deps of A should be B and C
        let mut deps = get_all_transient_dependencies(current, &trs).expect("it shouldnt error");
        deps.sort();
        assert_eq!(deps, ["b", "c"]);
    }

    #[test]
    fn can_get_all_transient_deps_simple_ok() {
        let a = create_tr!("a"; r#"{"1": $.b}"#);
        let b = create_tr!("b"; r#"{"1": $.c}"#);
        let c = create_tr!("c"; r#"{}"#);
        let trs = [a, b, c];
        let current = &trs[0];
        // A depends on B
        // B depends on C
        // C depends on nothing.
        // this should be valid, and all transient deps of A should be B and C
        let mut deps = get_all_transient_dependencies(current, &trs).expect("it shouldnt error");
        deps.sort();
        assert_eq!(deps, ["b", "c"]);
    }

    #[test]
    fn transient_deps_checks_for_self_dep_err() {
        let a = create_tr!("a"; r#"{"1": $.a}"#);
        let trs = [a];
        let current = &trs[0];
        // A depends on A which should be an error
        let err = get_all_transient_dependencies(current, &trs).expect_err("it should error");
        assert_eq!(err, "resource 'a' cannot depend on itself");
    }

    #[test]
    fn transient_deps_checks_for_self_dep_err_deep() {
        let a = create_tr!("a"; r#"{"1": $.b}"#);
        let b = create_tr!("b"; r#"{"1": $.c}"#);
        let c = create_tr!("c"; r#"{"1": $.c}"#);
        let trs = [a, b, c];
        let current = &trs[0];
        // C depends on C which should be an error
        let err = get_all_transient_dependencies(current, &trs).expect_err("it should error");
        assert_eq!(err, "resource 'c' cannot depend on itself");
    }

    #[test]
    fn can_get_all_transient_deps_deep_circle() {
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
        let err = get_all_transient_dependencies(current, &trs).expect_err("it should error");
        assert!(err.starts_with("circular dependency detected"));
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
}


/*


delete cases:
1. you delete a resource that does not depend on anything, nor anything depends on it. can be handled easily
2. you delete a resource that depends on another resource 'X'. X still exists. your resource can be deleted easily.
3. you delete a resource that depends on another resource 'X'. X is to be deleted in this update. must delete your resource first, before deleting X.


*/
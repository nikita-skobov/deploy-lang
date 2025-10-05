use std::collections::HashMap;

use dcl_language::{parse::{resource::ResourceSection, SpannedDiagnostic}, DclFile};
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
#[derive(Deserialize, Serialize, Debug)]
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
}

#[derive(Debug)]
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

/// iterate the currently known resources, and return a list of TransitionableResources
/// where each resource is either to be created (no state entry for that resource)
/// updated (the resource has a prior entry in state and the resource either has a json path [which means
/// we dont know yet if it has changed or not] or the resource's input has changed from its prior state entry)
/// or deleted (any state entries that remain that dont have a corresponding resource in the dcl file. these are resources
/// that were previously created and now must be deleted)
pub fn get_transitionable_resources(
    state: &mut StateFile,
    dcl: &mut DclFile,
) -> Vec<TransitionableResource> {
    // first collect the known resources that are to be created or updated:
    let mut out = Vec::with_capacity(state.resources.len());
    for resource in dcl.resources.drain(..) {
        match state.resources.remove(&resource.resource_name) {
            Some(state_entry) => {
                // if we can determine this resource has not changed, then we can omit it
                // from the transitionable resources:
                if resource_input_has_been_changed(&resource, &state_entry) {
                    out.push(TransitionableResource::Update { state_entry, current_entry: resource });
                }
                // otherwise its input has not changed, dont add it to the
                // list of transitionable resources
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

#[cfg(test)]
mod test {
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
    }

    #[test]
    fn can_determine_deleteable_resources() {
        let mut dcl = DclFile::default();
        let mut state = StateFile::default();
        state.resources.insert("a".to_string(), ResourceInState {
            resource_name: "a".to_string(),
            template_name: "t".to_string(),
            last_input: serde_json::json!({"a": "a"}),
            output: serde_json::Value::Null,
        });
        // no current entry for resource 'a', but state does have a prior entry for resource 'a'
        // so it should be deleted
        let mut transitionable = get_transitionable_resources(&mut state, &mut dcl);
        assert_eq!(transitionable.len(), 1);
        let resource = transitionable.pop().unwrap();
        assert_matches!(resource, TransitionableResource::Delete { .. });
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
            output: serde_json::Value::Null,
        });
        // a has a prior state, but its input has since changed. it should be updateable
        let mut transitionable = get_transitionable_resources(&mut state, &mut dcl);
        assert_eq!(transitionable.len(), 1);
        let resource = transitionable.pop().unwrap();
        assert_matches!(resource, TransitionableResource::Update { .. });
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
            output: serde_json::Value::Null,
        });
        // a has a prior state, but its current input is dynamic (has a json path)
        // so it should be considered updateable until its current input can be resolved
        // to an explicit value
        let mut transitionable = get_transitionable_resources(&mut state, &mut dcl);
        assert_eq!(transitionable.len(), 1);
        let resource = transitionable.pop().unwrap();
        assert_matches!(resource, TransitionableResource::Update { .. });
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
            output: serde_json::Value::Null,
        });
        // a has a prior state, but its last input is the same as its current input
        // therefore it should not be transitionable
        let transitionable = get_transitionable_resources(&mut state, &mut dcl);
        assert_eq!(transitionable.len(), 0);
    }
}

use std::collections::HashMap;

use dcl_language::{parse::{resource::ResourceSection, template::TemplateSection, SpannedDiagnostic}, DclFile};
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
#[derive(Deserialize, Serialize, Debug, Default)]
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
#[derive(Debug)]
pub struct TrWithTemplate {
    pub tr: TransitionableResource,
    pub template: TemplateSection,
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

pub async fn perform_update(mut dcl: DclFile, mut state: StateFile) -> Result<StateFile, String> {
    // first, collect resources into create/update/or delete, discarding
    // any resources that dont need to be updated
    let transitionable_resources = get_transitionable_resources(&mut state, &mut dcl);
    // next, ensure every resource can be matched with a template. error otherwise:
    let _transitionable_resources = match_resources_with_template(transitionable_resources, &dcl)?;
    todo!()
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
            ..Default::default()
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
            ..Default::default()
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
            ..Default::default()
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
            ..Default::default()
        });
        // a has a prior state, but its last input is the same as its current input
        // therefore it should not be transitionable
        let transitionable = get_transitionable_resources(&mut state, &mut dcl);
        assert_eq!(transitionable.len(), 0);
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
}


/*


delete cases:
1. you delete a resource that does not depend on anything, nor anything depends on it. can be handled easily
2. you delete a resource that depends on another resource 'X'. X still exists. your resource can be deleted easily.
3. you delete a resource that depends on another resource 'X'. X is to be deleted in this update. must delete your resource first, before deleting X.


*/
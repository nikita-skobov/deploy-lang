//! module for the various validations that should happen to a DclFile once it has been successfully parsed

use crate::{parse::{resource::ResourceSection, SpannedDiagnostic}, DclFile};

pub fn validate_dcl_file(dcl: &DclFile) -> Vec<SpannedDiagnostic> {
    let mut diagnostics = vec![];
    validate_resources(dcl, &mut diagnostics);
    diagnostics
}

pub fn validate_resources(dcl: &DclFile, diagnostics: &mut Vec<SpannedDiagnostic>) {
    for resource in dcl.resources.iter() {
        resource_has_corresponding_template(resource, dcl, diagnostics);
    }
}

pub fn resource_has_corresponding_template(resource: &ResourceSection, dcl: &DclFile, diagnostics: &mut Vec<SpannedDiagnostic>) {
    if !dcl.templates.iter().any(|x| resource.template_name == x.template_name) {
        diagnostics.push(SpannedDiagnostic::from_str_at_line(
            &resource.template_name,
            format!(
                    "template '{}' not found (referenced by resource '{}')",
                    resource.template_name,
                    resource.resource_name
                ),
            ));
    }
}

// pub fn every_json_path_query_references_valid_path(dcl: &DclFile) -> Result<(), SpannedDiagnostic> {
//     for resource in dcl.resources.iter() {
//         // resource.input
//     }
//     Ok(())
// }

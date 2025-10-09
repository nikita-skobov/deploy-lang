//! crate for parsing a .dcl file

use crate::parse::{resource::ResourceSection, state::StateSection, template::TemplateSection, SpannedDiagnostic};

pub mod parse;
pub mod validate;

#[derive(Default, Debug)]
pub struct DclFile {
    pub templates: Vec<TemplateSection>,
    /// a dcl file can only define state once
    pub state: Option<StateSection>,
    pub resources: Vec<ResourceSection>,
}

pub fn parse_and_validate<S: AsRef<str>>(dcl_file_contents: S) -> Result<DclFile, Vec<SpannedDiagnostic>> {
    let dcl_file_contents = dcl_file_contents.as_ref().to_string();
    let sections = parse::parse_document_to_sections(&dcl_file_contents);
    let mut filtered_sections = Vec::with_capacity(sections.len());
    let mut err_diagnostics = Vec::with_capacity(sections.len());
    for section in sections {
        match section {
            Ok(s) => filtered_sections.push(s),
            Err(e) => err_diagnostics.push(e),
        }
    }
    // error early if parsing failures:
    if !err_diagnostics.is_empty() {
        return Err(err_diagnostics);
    }

    let dcl = parse::sections_to_dcl_file(&filtered_sections)
        .map_err(|e| vec![e])?;
    let validations = validate::validate_dcl_file(&dcl);
    if !validations.is_empty() {
        return Err(validations);
    }
    Ok(dcl)
}

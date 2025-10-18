//! crate for parsing a .dpl file

use crate::parse::{function::FunctionSection, resource::ResourceSection, state::StateSection, template::TemplateSection, SpannedDiagnostic};

pub mod parse;
pub mod validate;

#[derive(Default, Debug)]
pub struct DplFile {
    pub templates: Vec<TemplateSection>,
    /// a dpl file can only define state once
    pub state: Option<StateSection>,
    pub resources: Vec<ResourceSection>,
    pub functions: Vec<FunctionSection>,
}

pub fn parse_and_validate<S: AsRef<str>>(dpl_file_contents: S) -> Result<DplFile, Vec<SpannedDiagnostic>> {
    let dpl_file_contents = dpl_file_contents.as_ref().to_string();
    let sections = parse::parse_document_to_sections(&dpl_file_contents);
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

    let dpl = parse::sections_to_dpl_file(&filtered_sections)
        .map_err(|e| vec![e])?;
    let validations = validate::validate_dpl_file(&dpl);
    if !validations.is_empty() {
        return Err(validations);
    }
    Ok(dpl)
}

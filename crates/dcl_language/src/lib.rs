//! crate for parsing a .dcl file

use crate::parse::{state::StateSection, template::TemplateSection};

pub mod dynamic_json;
pub mod parse;

#[derive(Default, Debug)]
pub struct DclFile {
    pub templates: Vec<TemplateSection>,
    /// a dcl file can only define state once
    pub state: Option<StateSection>,
}

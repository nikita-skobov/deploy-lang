//! state in declarative-cli is just a json object.
//! this module defines the shape of the state section
//! which defines how/where state is loaded

pub struct StateSection {
    /// state section requires loading state from a json file
    /// for now this is the only way to load state.
    pub file: String,
}

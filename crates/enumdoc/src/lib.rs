pub use enumdoc_derive;

pub trait Enumdoc {
    fn self_doc() -> &'static str;
    fn variant_doc(variant_name: &str) -> Option<&'static str>;
}

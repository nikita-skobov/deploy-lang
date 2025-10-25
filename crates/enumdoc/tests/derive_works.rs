use enumdoc::enumdoc_derive::Enumdoc;
use enumdoc::Enumdoc as _;

#[allow(unused)]
#[test]
fn derive_works() {
    #[derive(Enumdoc)]
    /// this is my enum docs
    pub enum MyEnum {
        /// documentation
        /// for
        /// A
        A,
        #[doc = "B doc here"]
        B,
    }
    assert_eq!(MyEnum::self_doc(), "this is my enum docs");
    assert_eq!(MyEnum::variant_doc("A").unwrap(), "documentation\nfor\nA");
    assert_eq!(MyEnum::variant_doc("B").unwrap(), "B doc here");
    assert_eq!(MyEnum::variant_doc("Nonexistantvariant"), None);
}

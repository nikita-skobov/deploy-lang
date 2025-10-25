use quote::{quote, ToTokens};
use syn::{Attribute, DeriveInput};

#[proc_macro_derive(Enumdoc)]
pub fn enumdoc_derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = proc_macro2::TokenStream::from(input);
    let out = enumdoc_derive_impl(input);
    proc_macro::TokenStream::from(out)
}

fn enumdoc_derive_impl(input: proc_macro2::TokenStream) -> proc_macro2::TokenStream {
    let input: DeriveInput = syn::parse2(input).unwrap();
    let data_enum = match input.data {
        syn::Data::Struct(_) => panic!("enumdoc can only be used on enums"),
        syn::Data::Union(_) => panic!("enumdoc can only be used on enums"),
        syn::Data::Enum(data_enum) => data_enum,
    };
    let name = input.ident;
    let s = extract_doc_str_from_attrs(&input.attrs);

    let variant_docs_tokens = data_enum
        .variants
        .iter()
        .map(|variant| {
            let variant_name = variant.ident.to_string().to_lowercase();
            let doc_str = extract_doc_str_from_attrs(&variant.attrs);  
            quote! {
                (#variant_name, #doc_str)
            }
        });
    let expanded = quote! {
        impl enumdoc::Enumdoc for #name {
            fn self_doc() -> &'static str { #s }
            fn variant_doc(variant_name: &str) -> Option<&'static str> {
                let variant_name = variant_name.to_lowercase();
                static VARIANTS: &[( &str, &str )] = &[
                    #(
                        (#variant_docs_tokens),
                    )*
                ];
                VARIANTS.iter()
                    .find(|(name, _)| *name == variant_name)
                    .map(|(_, doc)| *doc)
            }
        }
    };
    proc_macro2::TokenStream::from(expanded)
}

fn extract_doc_str_from_attrs(attrs: &[Attribute]) -> String {
    let mut out = "".to_string();
    for attr in attrs {
        if let Some(ident) = attr.path().get_ident() {
            if ident.to_string() != "doc" { continue; }
        }
        // its a doc ident, get the doc string:
        let mut s = match &attr.meta {
            syn::Meta::NameValue(meta_name_value) => {
                meta_name_value.value.to_token_stream().to_string()
            }
            _ => continue,
        };
        while s.len() > 1 && s.starts_with('"') && s.ends_with('"') {
            s.pop();
            s.remove(0);
        }
        // remove leading whitespace
        let s = s.trim_start().to_string();
        out.push_str(&s);
        out.push('\n');
    }
    if !out.is_empty() {
        out.pop();
    }
    out
}

#[cfg(test)]
mod test {
    use std::str::FromStr;
    use super::*;

    #[test]
    fn can_parse_doc_attrs() {
        let stream = r#"
        /// hello world
        #[doc = "abc"]
        pub enum Blah {}
        "#;
        let tokenstream = proc_macro2::TokenStream::from_str(stream).unwrap();
        let input: DeriveInput = syn::parse2(tokenstream).unwrap();
        let doc_str = extract_doc_str_from_attrs(&input.attrs);
        assert_eq!(doc_str, "hello world\nabc");
    }
}

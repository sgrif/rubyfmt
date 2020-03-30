extern crate proc_macro;

use proc_macro::TokenStream;
use quote::quote;

#[proc_macro_derive(
    RipperDeserialize,
    attributes(tag)
)]
pub fn derive_deserialize(input: TokenStream) -> TokenStream {
    let item: syn::ItemEnum = syn::parse(input)
        .expect("`#[derive(RipperDeserialize)]` only works on enums. Use \
                `#[derive(Serialize)]` for structs");
    let variants = item.variants.into_iter()
        .filter_map(TaggedVariant::from_variant);
    let enum_name = item.ident;

    let variant_arms = variants.map(|TaggedVariant { tag, ident }| {
        quote! {
            #tag => serde::Deserialize::deserialize(rest).map(#enum_name::#ident),
        }
    });

    let tokens = quote! {
        impl<'de> serde::Deserialize<'de> for #enum_name {
            fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
            where
                D: serde::Deserializer<'de>,
            {
                struct Visitor;

                impl<'de> serde::de::Visitor<'de> for Visitor {
                    type Value = #enum_name;

                    fn expecting(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                        write!(f, concat!("a variant of ", stringify!(#enum_name)))
                    }

                    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
                    where
                        A: serde::de::SeqAccess<'de>,
                    {
                        let tag = seq.next_element()?
                            .ok_or_else(|| serde::de::Error::custom("No tag found"))?;
                        let rest = serde::de::value::SeqAccessDeserializer::new(seq);
                        match tag {
                            #(#variant_arms)*
                            _ => Err(serde::de::Error::custom(format!(
                                concat!(
                                    "Invalid tag for ",
                                    stringify!(#enum_name),
                                    " {}"
                                ),
                            tag))),
                        }
                    }
                }

                deserializer.deserialize_seq(Visitor)
            }
        }
    };

    tokens.into()
}

struct TaggedVariant {
    tag: String,
    ident: syn::Ident,
}

impl TaggedVariant {
    fn from_variant(variant: syn::Variant) -> Option<Self> {
        let tag = variant.attrs
            .into_iter()
            .find(|attr| attr.path.is_ident("tag"))?
            // .expect("All enum variants must have `#[tag=\"...\"]`")
            .parse_meta()
            .ok()?;
            // .expect("#[tag] must be in the form `#[tag=\"...\"]`");
        let tag = match tag {
            syn::Meta::NameValue(syn::MetaNameValue { lit: syn::Lit::Str(s), .. }) => s.value(),
            _ => panic!("#[tag] must be in the form `#[tag=\"...\"]`"),
        };

        Some(Self {
            tag,
            ident: variant.ident,
        })
    }
}

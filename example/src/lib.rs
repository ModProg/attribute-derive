#![allow(unused)]
use std::fmt::Debug;

use attribute_derive::{Attribute, AttributeIdent};
use proc_macro::TokenStream;
use syn::{Block, DeriveInput, Result};

// #[derive(Attribute)]
// #[attribute(ident = "positional")]
// struct PositionalAttr {
//     #[attribute(positional)]
//     a: u8,
//     #[attribute(positional)]
//     b: String,
//     #[attribute(positional)]
//     c: bool,
// }
//
// #[proc_macro_derive(Positional, attributes(attribute))]
// pub fn positional_derive(input: TokenStream) -> proc_macro::TokenStream {
//     all_attrs::<PositionalAttr>(input).unwrap_or_else(|e|
// e.to_compile_error().into()) }
#[derive(Attribute)]
#[attribute(ident = empty)]
struct Empty {}

#[derive(Attribute)]
#[attribute(ident = single)]
struct Single {
    field: bool,
}

#[derive(Attribute, Debug)]
#[attribute(ident = ident, aliases = [a, b])]
struct Normal {
    optional_implicit: Option<u8>,
    #[attribute(optional)]
    optional_explicit: u8,
    #[attribute(optional, default = 2 * 5)]
    optional_default: u8,
    #[attribute(default = 33)]
    default: u8,
    #[attribute(conflicts = [conflict_b])]
    conflict_a: Option<String>,
    conflict_b: Option<String>,
    #[attribute(example = "2.5")]
    example: f32,
    flag: bool,
    #[attribute(optional = false)]
    mandatory_flag: bool,
}
#[proc_macro_derive(Normal, attributes(ident, a, b, empty, single))]
pub fn normal_derive(input: TokenStream) -> proc_macro::TokenStream {
    let mut tokens =
        all_attrs::<Normal>(input.clone()).unwrap_or_else(|e| e.to_compile_error().into());
    tokens
        .extend(all_attrs::<Empty>(input.clone()).unwrap_or_else(|e| e.to_compile_error().into()));
    tokens.extend(all_attrs::<Single>(input).unwrap_or_else(|e| e.to_compile_error().into()));
    tokens
}

#[derive(Attribute, Debug)]
#[attribute(ident = ident, aliases = [a, b])]
#[attribute(error(
    unknown_field = "found `{found_field}` but expected one of {expected_fields:i(`{}`)(, )}",
    duplicate_field = "duplicate `{field}`",
    missing_field = "missing field `{field}`",
    field_help = "try {attribute}: {field}={example}",
    missing_flag = "missing flag `{flag}`",
    flag_help = "try {attribute}: {flag}",
    conflict = "{first} !!! {second}"
))]
struct Custom {
    optional_implicit: Option<Block>,
    #[attribute(optional)]
    optional_explicit: u8,
    #[attribute(optional, default = 2 * 5)]
    optional_default: u8,
    #[attribute(default = 33)]
    default: u8,
    #[attribute(conflicts = [conflict_b])]
    conflict_a: Option<String>,
    conflict_b: Option<String>,
    #[attribute(example = "2.5")]
    example: f32,
    flag: bool,
    #[attribute(optional = false)]
    mandatory_flag: bool,
}
#[derive(Attribute)]
#[attribute(ident = empty, error(unknown_field_empty = "found {found_field}, but expected none"))]
struct EmptyCustom {}

#[derive(Attribute)]
#[attribute(ident = single, error(unknown_field_single = "found {found_field}, but expected {expected_field}"))]
struct SingleCustom {
    field: bool,
}

#[proc_macro_derive(Custom, attributes(ident, a, b, empty, single))]
pub fn custom_derive(input: TokenStream) -> proc_macro::TokenStream {
    let mut tokens =
        all_attrs::<Custom>(input.clone()).unwrap_or_else(|e| e.to_compile_error().into());
    tokens.extend(
        all_attrs::<EmptyCustom>(input.clone()).unwrap_or_else(|e| e.to_compile_error().into()),
    );
    tokens.extend(all_attrs::<SingleCustom>(input).unwrap_or_else(|e| e.to_compile_error().into()));
    tokens
}

fn all_attrs<T: Attribute + AttributeIdent>(input: TokenStream) -> Result<TokenStream> {
    let DeriveInput { attrs, data, .. } = syn::parse(input)?;
    T::from_attributes(&attrs)?;
    match data {
        syn::Data::Struct(data) => {
            for field in data.fields {
                T::from_attributes(&field.attrs)?;
            }
        }
        syn::Data::Enum(data) => {
            for variant in data.variants {
                T::from_attributes(&variant.attrs)?;
                for field in variant.fields {
                    T::from_attributes(&field.attrs)?;
                }
            }
        }
        syn::Data::Union(data) => {
            for field in data.fields.named {
                T::from_attributes(&field.attrs)?;
            }
        }
    }
    Ok(TokenStream::new())
}

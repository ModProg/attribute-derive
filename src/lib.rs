#![warn(missing_docs)]
//! Basically clap for attribute macros:
//! ```
//! use attribute_derive::FromAttr;
//! #[derive(FromAttr)]
//! #[attribute(ident = attr_name)]
//! // overriding the builtin error messages
//! #[attribute(error(missing_field = "`{field}` was not specified"))]
//! struct MyAttribute {
//!     // Positional values need to be specified before any named ones
//!     #[attribute(positional)]
//!     positional: u8,
//!     // Options are optional by default (will be set to None if not specified)
//!     optional: Option<String>,
//!     required: String,
//!     // Any type implementing default can be flagged as default
//!     // This will be set to Vec::default() when not specified
//!     #[attribute(optional)]
//!     list: Vec<syn::Type>,
//!     // Booleans can be used without assigning a value, i.e., as a flag.
//!     // If omitted they are set to false
//!     some_flag: bool,
//! }
//! ```
//!
//! Will be able to parse an attribute like this:
//! ```rust
//! # #[cfg(no)]
//! #[attr_name(5, optional="some", required = r#"string"#, some_flag, list = [Option, ()])]
//! // or
//! #[attr_name(5, required = "string", list(Option, ()))]
//! # struct Placeholder;
//! ```
//!
//! Any type that for [`AttributeNamed`] or [`AttributePositional`] are
//! implemented respectively are supported. These should be the general types
//! that [`syn`] supports like [`LitStr`](struct@LitStr) or [`Type`] or that
//! have a direct equivalent in those like [`String`], [`char`] or [`f32`]. A
//! special treatment have [`Vecs`](Vec) which are parsed as either `name = [a,
//! b, c]` or `name(a, b, c)` and [`Options`](Option) that will be [`None`] if
//! not specified and [`Some`] when the value is specified via the attribute. It
//! is not specified via `Some(value)` but as just `value`. [`Bools`](bool) are
//! used for flags, i.e., without a value. Most should just behave as expected,
//! see [`parsing`] for details.
//!
//! Tuple structs can derive [`FromAttr`] as well, but all fields will be
//! positional. Tuples with a single field
//! ([new types](https://rust-unofficial.github.io/patterns/patterns/behavioural/newtype.html))
//! will copy the behavior of the contained field, e.g. for [`bool`]:
//!
//! ```
//! use syn::{Attribute, parse_quote};
//! use attribute_derive::FromAttr;
//!
//! #[derive(FromAttr, PartialEq, Debug)]    
//! #[attribute(ident = flag)]
//! struct Flag(bool);
//!
//! let attr: Attribute = parse_quote!(#[flag]);
//! assert_eq!(Flag::from_attribute(attr).unwrap(), Flag(true));
//!
//! let attr: Attribute = parse_quote!(#[flag = true]);
//! assert_eq!(Flag::from_attribute(attr).unwrap(), Flag(true));
//!
//! let attr: Attribute = parse_quote!(#[flag(false)]);
//! assert_eq!(Flag::from_attribute(attr).unwrap(), Flag(false));
//! ```
//!
//! # Attributes
//!
//! The parsing of attributes can be modified with the following parameters via
//! the `#[attribute(<params>)]` attribute. All of them are optional. Error
//! messages are formatted using [interpolator], and only support display and
//! lists `i` formatting. See [interpolator] docs for details.
//!
//! ### Struct
//!
//! - `ident = <ident>` The attribute ident. Improves error messages and enables
//!   the [`from_attributes`](FromAttr::from_attributes) and
//!   [`remove_attributes`](FromAttr::remove_attributes) functions.
//! - `aliases = [<alias>, ...]` Aliases for the attribute ident.
//! - `error = "<error message>"` Overrides default error message.
//! - `error(`
//!     - ``unknown_field = "supported fields are {expected_fields:i..-1(`{}`)(,
//!       )} and `{expected_fields:i-1}`",`` Custom error message printed if an
//!       unknown property is specified and attribute has more than one field.
//!       Placeholders: `{expected_fields:i}`.
//!     - ``unknown_field_single = "expected supported field
//!       `{expected_field}`",`` Custom error message printed if an unknown
//!       property is specified, and attribute only has a single field.
//!       Placeholders: `{expected_field}`.
//!     - ``unknown_field_empty = "expected empty attribute",`` Custom error
//!       message printed if a property is specified, and attribute has no
//!       fields.
//!     - ``duplicate_field = "`{field}` is specified multiple times",`` Custom
//!       error message printed if a property is specified multiple times.
//!       Placeholders: `{field}`.
//!     - ``missing_field = "required `{field}` is not specified",`` Custom
//!       error message printed if a required property is not specified.
//!       Placeholders: `{field}`.
//!     - ``field_help = "try `#[{attribute}({field}={example})]`",`` Additional
//!       help message printed if a required property is not specified or has an
//!       error. Placeholders: `{attribute}`, `{field}` and `{example}`.
//!     - ``conflict = "`{first}` conflicts with mutually exclusive
//!       `{second}`"`` Custom error message printed when conflicting properties
//!       are specified. Placeholders: `{first}` and `{second}`.
//!
//!   `)`
// //! - `duplicate = AggregateOrError` Change the behavior for duplicate arguments
// //!   (also across multiple attributes).
// //!   - `AggregateOrError` Aggregate multiple [`Vec`], error on everything else.
// //!   - `Error` Disables aggregation, errors on all duplicates.
// //!   - `AggregateOrOverride`  Aggregate multiple [`Vec`], take the last
// //!     specified for everything else.
// //!   - `Override` Disables aggregation, always take the last value.
//! ### Fields
//!
//! - `optional` If field is not specified, the default value is used instead.
//! - `default = <default expr>` provides a default to be used instead of
//!   [`Default::default()`]. Enables `optional`.
//! - `conflicts(<field>, ...)` Conflicting fields
//! - `example = "<example>"`
//!
//! # Parse methods
//!
//! There are multiple ways of parsing a struct deriving [`FromAttr`].
//!
//! For helper attributes there is:
//! - [`FromAttr::from_attributes`] which takes in an [`IntoIterator<Item = &'a
//!   syn::Attribute`](syn::Attribute) (e.g. a
//!   [`&Vec<syn::Attribute>`](syn::Attribute)). Most useful for derive macros.
//! - [`FromAttr::remove_attributes`] which takes a [`&mut
//!   Vec<syn::Attribute>`](syn::Attribute) and does not only parse the
//!   attributes, but also removes those matching. Useful for helper attributes
//!   for attribute macros, where the helper attributes need to be removed.
//!
//! For parsing a single [`TokenStream`] e.g. for parsing the proc macro input
//! there are two ways:
//!
//! - [`FromAttr::from_args`] taking in a [`TokenStream`]
//! - As `derive(FromAttr)` also derives [`Parse`] so you can use the
//!   [parse](mod@syn::parse) API, e.g. with [`parse_macro_input!(tokens as
//!   Attribute)`](syn::parse_macro_input!).
//!
//! [interpolator]: https://docs.rs/interpolator/latest/interpolator/
use std::borrow::Borrow;
use std::fmt::Debug;
use std::iter;

#[doc(hidden)]
pub use attribute_derive_macro::Attribute;
pub use attribute_derive_macro::FromAttr;
use manyhow::SpanRanged;
#[cfg(doc)]
use parsing::*;
use parsing::{AttributeBase, SpannedValue};
use proc_macro2::{Span, TokenStream};
use quote::ToTokens;
use syn::parse::{ParseStream, Parser, Result};
#[cfg(doc)]
use syn::{parse::Parse, LitStr, Type};
use syn::{Error, Meta, Path};

extern crate self as attribute_derive;

#[doc(hidden)]
pub mod __private {
    pub use {proc_macro2, quote, syn};
}

mod std_impls;

mod syn_impls;

pub mod utils;
pub use utils::FlagOrValue;

pub mod parsing;

pub mod from_partial;
pub use from_partial::FromPartial;

/// The trait you actually derive on your attribute struct.
///
/// Basic gist is a struct like this:
/// ```
/// # use attribute_derive::FromAttr;
/// # use syn::Type;
/// #[derive(FromAttr)]
/// #[attribute(ident = collection)]
/// #[attribute(error(missing_field = "`{field}` was not specified"))]
/// struct CollectionAttribute {
///     // Options are optional by default (will be set to None if not specified)
///     authority: Option<String>,
///     name: String,
///     // Any type implementing default can be flagged as optional
///     // This will be set to Vec::default() when not specified
///     #[attribute(optional)]
///     views: Vec<Type>,
///     // Booleans can be used without assiging a value. as a flag.
///     // If omitted they are set to false
///     some_flag: bool,
/// }
/// ```
///
/// Will be able to parse an attribute like this:
/// ```text
/// #[collection(authority="Some String", name = r#"Another string"#, views = [Option, ()], some_flag)]
/// ```
pub trait FromAttr: Sized + AttributeBase {
    /// Parses an [`IntoIterator`] of [`syn::Attributes`](syn::Attribute)
    /// e.g. [`Vec<Attribute>`](Vec). Only available if you specify
    /// the attribute ident: `#[attribute(ident="<ident>")]` when
    /// using the derive macro.
    ///
    /// It can therefore parse fields set over multiple attributes like:
    /// ```text
    /// #[collection(authority = "Authority", name = "Name")]
    /// #[collection(views = [A, B])]
    /// ```
    /// And also catch duplicate/conflicting settings over those.
    ///
    /// This is best used for derive macros, where you don't need to remove
    /// your attributes.
    ///
    /// # Errors
    /// Fails with a [`syn::Error`] so you can conveniently return that as a
    /// compiler error in a proc macro in the following cases
    ///
    /// - A required parameter is omitted
    /// - Invalid input is given for a parameter
    /// - A non aggregating parameter is specified multiple times
    /// - An attribute called [`IDENTS`](const@AttributeIdent::IDENTS) has
    ///   invalid syntax (e.g. `#attr(a: "a")`)
    fn from_attributes<A: Borrow<syn::Attribute>>(
        attrs: impl IntoIterator<Item = A>,
    ) -> Result<Self>
    where
        Self: AttributeIdent,
    {
        attrs
            .into_iter()
            .filter(|attr| Self::is_ident(attr.borrow().path()))
            .map(Self::from_attribute_partial)
            .try_fold(None, |acc, item| {
                Self::join(
                    acc,
                    SpannedValue::call_site(item?),
                    &format!("`{}` was specified twice", Self::ident()),
                )
            })
            .and_then(|o| {
                Self::from_option(
                    o.map(SpannedValue::value),
                    &format!("`{}` is not set", Self::ident()),
                )
            })
    }

    /// Parses a [`&mut Vec<syn::Attributes>`](syn::Attribute). Removing
    /// matching attributes. Only available if you specify an ident:
    /// `#[attribute(ident="<ident>")]` when using the derive macro.
    ///
    /// It can therefore parse fields set over multiple attributes like:
    /// ```text
    /// #[collection(authority = "Authority", name = "Name")]
    /// #[collection(views = [A, B])]
    /// ```
    /// And also catch duplicate/conflicting settings over those.
    ///
    /// Use this if you are implementing an attribute macro, and need to
    /// remove your helper attributes.
    ///
    /// ```
    /// use syn::parse_quote;
    /// use attribute_derive::FromAttr;
    /// let mut attrs = vec![
    ///     parse_quote!(#[ignored]), parse_quote!(#[test]),
    ///     parse_quote!(#[also_ignored]), parse_quote!(#[test])
    /// ];
    /// #[derive(FromAttr)]
    /// #[attribute(ident = test)]
    /// struct Test {}
    /// assert!(Test::remove_attributes(&mut attrs).is_ok());
    ///
    /// assert_eq!(attrs, vec![parse_quote!(#[ignored]), parse_quote!(#[also_ignored])]);
    /// ```
    ///
    /// # Errors
    /// Fails with a [`syn::Error`], so you can conveniently return that as
    /// a compiler error in a proc macro in the following cases
    ///
    /// - A necessary parameter is omitted
    /// - Invalid input is given for a parameter
    /// - A non aggregating parameter is specified multiple times
    /// - An attribute called [`IDENTS`](const@AttributeIdent::IDENTS) has
    ///   invalid syntax (e.g. `#attr(a: "a")`)
    fn remove_attributes(attrs: &mut Vec<syn::Attribute>) -> Result<Self>
    where
        Self: AttributeIdent,
    {
        let mut i = 0;
        Self::from_attributes(iter::from_fn(|| {
            while i < attrs.len() {
                if Self::is_ident(attrs[i].path()) {
                    return Some(attrs.remove(i));
                }
                i += 1;
            }
            None
        }))
    }

    /// Parses from a single attribute. Ignoring the name.
    ///  
    /// This is available even without `#[attribute(ident = ...)]`, because
    /// it ignores the attribute's path, allowing to use it to parse e.g.
    /// literals:
    /// ```
    /// use attribute_derive::FromAttr;
    ///
    /// let attr: syn::Attribute = syn::parse_quote!(#[test = "hello"]);
    /// assert_eq!(String::from_attribute(attr).unwrap(), "hello");
    ///
    /// let attr: syn::Attribute = syn::parse_quote!(#[test]);
    /// assert_eq!(bool::from_attribute(attr).unwrap(), true);
    /// ```
    fn from_attribute(attr: impl Borrow<syn::Attribute>) -> Result<Self> {
        Self::from_attribute_partial(attr).and_then(Self::from)
    }

    #[doc(hidden)]
    #[deprecated = "use `from_input` instead"]
    fn from_args(tokens: TokenStream) -> Result<Self> {
        Self::from_input(tokens)
    }

    /// Parses a [`TokenStream`](proc_macro2::TokenStream).
    ///
    /// Useful for implementing general proc macros to parse the input of
    /// your macro.
    ///
    /// This is a convenience over [`parse_input`](Self::parse_input). More
    /// details are documented there.
    fn from_input(input: impl Into<TokenStream>) -> Result<Self> {
        Self::parse_input.parse2(input.into())
    }

    /// Parses input as the complete attribute.
    ///
    /// Due to this only parsing the input for a single attribute it is not
    /// able to aggregate input spread over multiple attributes.
    ///
    /// # Errors
    /// Fails with a [`syn::Error`], so you can conveniently return that as
    /// a compiler error in a proc macro in the following cases
    ///
    /// - A necessary parameter is omitted
    /// - Invalid input is given for a parameter
    /// - A non aggregating parameter is specified multiple times
    fn parse_input(input: ParseStream) -> Result<Self> {
        Self::parse_partial(input).and_then(Self::from)
    }

    /// Like [`parse_partial`](Self::parse_partial) but instead takes an
    /// [`Attribute`](syn::Attribute).
    ///
    /// This allows it to support all three, `#[flag]`, `#[function(like)]`
    /// and `#[name = value]` attributes.
    fn from_attribute_partial(attr: impl Borrow<syn::Attribute>) -> Result<Self::Partial> {
        let tokens = match attr.borrow().meta {
            Meta::Path(_) => TokenStream::new(),
            Meta::List(ref list) => list.tokens.clone(),
            Meta::NameValue(ref nv) => nv.value.to_token_stream(),
        };
        Self::parse_partial.parse2(tokens)
    }

    /// Actual implementation for parsing the attribute. This is the only
    /// function required to implement in this trait and derived by the
    /// [`FromAttr`](macro@FromAttr) derive macro.
    fn parse_partial(input: ParseStream) -> Result<Self::Partial>;
}

/// Helper trait providing the path for an attribute.
///
/// Automatically derived with [`FromAttr`], if `#[attribute(ident =
/// "some_ident")]` is provided.
pub trait AttributeIdent {
    /// List of idents, must contain at least one ident.
    const IDENTS: &'static [&'static str];

    /// Tests if Attribute matches one of the idents.
    fn is_ident(path: &Path) -> bool {
        Self::IDENTS.iter().any(|ident| path.is_ident(ident))
    }

    /// Returns default ident.
    ///
    /// # Panics
    /// The default implementation panics if `IDENTS` is empty. Implementors
    /// should ensure this is not the case.
    fn ident() -> &'static str {
        Self::IDENTS
            .first()
            .expect("`AttributeIdent::IDENTS` should not be empty")
    }
}

//! Basicly clap for attribute macros:
//! ```
//! # use attribute_derive::Attribute;
//! # use syn::Type;
//! #[derive(Attribute)]
//! #[attribute(ident = collection)]
//! #[attribute(error(missing_field = "`{field}` was not specified"))]
//! struct CollectionAttribute {
//!     // Options are optional by default (will be set to None if not specified)
//!     authority: Option<String>,
//!     name: String,
//!     // Any type implementing default can be flagged as default
//!     // This will be set to Vec::default() when not specified
//!     #[attribute(optional)]
//!     views: Vec<Type>,
//!     // Booleans can be used without assiging a value. as a flag.
//!     // If omitted they are set to false
//!     some_flag: bool,
//! }
//! ```
//!
//! Will be able to parse an attribute like this:
//! ```text
//! #[collection(authority="Some String", name = r#"Another string"#, views = [Option, ()])]
//! ```
//!
//! Any type that [`ConvertParsed`] is implemented for is supported. These
//! should be the general types that syn supports like [`LitStr`](struct@LitStr)
//! or [`Type`] or that have a direct equivalent in those like [`String`],
//! [`char`] or [`f32`]. A special treatment have [`Vecs`](Vec) which are parsed
//! using [`Array`] with the syntax `[a, b, c]` and [`Options`](Option) that
//! will be [`None`] if not specified and [`Some`] when the value is specified
//! via the attribute. It is not specified via `Some(value)` but as just
//! `value`.
//!
//! # Attributes
//!
//! The parsing of attributes can be modified with the following parameters via
//! the `#[attribute(<params>)]` attribute. All of them are optional. Error
//! messages are formatted using [interpolator], and only support display and on
//! lists `i` formatting. See [interpolator] docs for details.
//!
//! ### Struct
//!
//! - `ident = <ident>` The attribute ident. Improves error messages and enables
//!   the [`from_attributes`](Attribute::from_attributes) and
//!   [`remove_attributes`](Attribute::remove_attributes) functions.
//! - `aliases = [<alias>, ...]` Aliases for the attribute ident.
//! - `error = "<error message>"` Overrides default error message.
//! - `error(`
//!     - ``unknown_field = "supported fields are {expected_fields:i..-1(`{}`)(,
//!       )} and `{expected_fields:i-1}`",`` Custom error message printed if an
//!       unknown property is specified and attribute has more than one field.
//!       Supports `{found_field}` and `{expected_fields:i}` placeholders.
//!     - ``unknown_field_single = "expected supported field
//!       `{expected_field}`",`` Custom error message printed if an unknown
//!       property is specified, and attribute only has a single field. Supports
//!       `{found_field}` and `{expected_field}` placeholders.
//!     - ``unknown_field_empty = "expected empty attribute",`` Custom error
//!       message printed if a property is specified, and attribute has no
//!       fields. Supports `{found_field}` placeholder.
//!     - ``duplicate_field = "`{field}` is specified multiple times",`` Custom
//!       error message printed if a property is specified multiple times.
//!       Supports `{field}` placeholder.
//!     - ``missing_field = "required `{field}` is not specified",`` Custom
//!       error message printed if a required property is not specified.
//!       Supports `{field}` placeholder.
//!     - ``field_help = "try `#[{attribute}({field}={example})]`",`` Additional
//!       help message printed if a required property is not specified or has an
//!       error. Supports `{attribute}`, `{field}` and `{example}` placeholder.
//!     - ``missing_flag = "required `{flag}` is not specified",`` Custom error
//!       message printed if a required flag is not specified. Supports `{flag}`
//!       placeholder.
//!     - ``flag_help = "try `#[{attribute}({flag})]`",`` Additional help
//!       message printed if a required flag is not specified. Supports
//!       `{attribute}` and `{flag}` placeholder.
//!     - ``conflict = "`{first}` conflicts with mutually exclusive
//!       `{second}`"`` Custom error message printed if conflicting properties
//!       are specified. Supports `{first}` and `{second}` placeholder.
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
//! - `optional` / `optional = true` If field is not specified, the default
//!   value is used instead.
//! - `optional = false` Disables implicit optionality of [`Option`], [`Vec`]
//!   and [`bool`]. Note that this makes `Option<T>` behave the same as `T` and
//!   makes a bool a mandatory flag.
//! - `default = <default expr>` provides a default to be used instead of
//!   [`Default`]. Enables `optional`.
// //! - `aggregate = false` Disables aggregation for [`Vec`].
// //! - `flag = false` Disables flag mode for bool.
//! - `conflicts = [<field>, ...]` Conflicting fields
//! - `example = "<example>"`
//!
//! # Limitations
//!
//! There are some limitations in syntax parsing that will be lifted in future
//! releases.
//!
//! - literals in top level (meaning something like `#[attr(42, 3.14, "hi")]`
//! - function like arguments (something like `#[attr(view(a = "test"))]`
//! - other syntaxes, maybe something like `key: value`
//!
//! # Parse methods
//!
//! There are multiple ways of parsing a struct deriving [`Attribute`].
//!
//! For helper attributes there is:
//! - [`Attribute::from_attributes`] which takes in an [`IntoIterator<Item = &'a
//! syn::Attribute`](syn::Attribute)
//! (e.g. a [`&Vec<syn::Attribute>`](syn::Attribute)). Most useful for derive
//! macros.
//! - [`Attribute::remove_attributes`] which takes an [`&mut
//!   Vec<syn::Attribute>`](syn::Attribute)
//! and does not only parse the [`Attribute`] but also removes those matching.
//! Useful for helper attributes for proc macros, where the helper attributes
//! need to be removed.
//!
//! For parsing a single [`TokenStream`] e.g. for parsing the proc macro input
//! there a two ways:
//!
//! - [`Attribute::from_args`] taking in a [`TokenStream`]
//! - As `derive(Attribute)` also derives [`Parse`] so you can use the
//!   [parse](mod@syn::parse) API,
//! e.g. with [`parse_macro_input!(tokens as
//! Attribute)`](syn::parse_macro_input!).
//!
//! [interpolator]: https://docs.rs/interpolator/latest/interpolator/
#![deny(missing_docs)]
use std::fmt::Display;

#[doc(hidden)]
pub use attribute_derive_macro::Attribute;
use proc_macro2::{Group, Literal, Punct, Span, TokenStream, TokenTree};
use quote::ToTokens;
use syn::parse::{Parse, Result};
use syn::punctuated::Punctuated;
use syn::token::{
    Abstract, And, AndAnd, AndEq, As, Async, At, Auto, Await, Become, Break, Caret, CaretEq, Colon,
    Comma, Const, Continue, Crate, Do, Dollar, Dot, DotDot, DotDotDot, DotDotEq, Dyn, Else, Enum,
    Eq, EqEq, Extern, FatArrow, Final, Fn, For, Ge, Gt, If, Impl, In, LArrow, Le, Let, Loop, Lt,
    Match, Minus, MinusEq, Mod, Move, Mut, Ne, Not, Or, OrEq, OrOr, Override, PathSep, Percent,
    PercentEq, Plus, PlusEq, Pound, Priv, Pub, Question, RArrow, Ref, Return, SelfType, SelfValue,
    Semi, Shl, ShlEq, Shr, ShrEq, Slash, SlashEq, Star, StarEq, Static, Struct, Super, Tilde,
    Trait, Try, Typeof, Underscore, Union, Unsafe, Unsized, Use, Virtual, Where, While, Yield,
};
use syn::{
    bracketed, parse2, parse_quote, Abi, AngleBracketedGenericArguments, BareFnArg, BinOp,
    BoundLifetimes, ConstParam, Constraint, DeriveInput, Expr, ExprArray, ExprAssign, ExprAsync,
    ExprBinary, ExprBlock, ExprBreak, ExprCall, ExprCast, ExprClosure, ExprContinue, ExprField,
    ExprForLoop, ExprIf, ExprIndex, ExprLet, ExprLit, ExprLoop, ExprMacro, ExprMatch,
    ExprMethodCall, ExprParen, ExprPath, ExprRange, ExprReference, ExprRepeat, ExprReturn,
    ExprStruct, ExprTry, ExprTryBlock, ExprTuple, ExprUnary, ExprUnsafe, ExprWhile, ExprYield,
    FieldsNamed, FieldsUnnamed, GenericArgument, GenericParam, Generics, Ident, Index, Lifetime,
    Lit, LitBool, LitByteStr, LitChar, LitFloat, LitInt, LitStr, Member, Meta, MetaList,
    MetaNameValue, ParenthesizedGenericArguments, Path, PathSegment, ReturnType, Token, TraitBound,
    TraitBoundModifier, Type, TypeArray, TypeBareFn, TypeGroup, TypeImplTrait, TypeInfer,
    TypeMacro, TypeNever, TypeParam, TypeParamBound, TypeParen, TypePath, TypePtr, TypeReference,
    TypeSlice, TypeTraitObject, TypeTuple, UnOp, Variant, Visibility, WhereClause, WherePredicate,
};

#[doc(hidden)]
pub mod __private {
    pub use {proc_macro2, quote, syn};
}

/// Helper trait providing the path for an attribute.
///
/// Automatically derived with [`Attribute`], if `#[attribute(ident =
/// "some_ident")]` is provided.
pub trait AttributeIdent: Sized {
    /// Type used for Self::ALIASES, e.g. `[&'static str; 5]`
    type IDENTS: AsRef<[&'static str]>;
    /// List of idents
    const IDENTS: Self::IDENTS;

    /// Tests if Attribute matches one of the idents
    fn is_ident(path: &Path) -> bool {
        Self::IDENTS
            .as_ref()
            .iter()
            .any(|ident| path.is_ident(ident))
    }
}

/// The trait you actually derive on your attribute struct.
///
/// Basic gist is a struct like this:
/// ```
/// # use attribute_derive::Attribute;
/// # use syn::Type;
/// #[derive(Attribute)]
/// #[attribute(ident = collection)]
/// #[attribute(error(missing_field = "`{field}` was not specified"))]
/// struct CollectionAttribute {
///     // Options are optional by default (will be set to None if not specified)
///     authority: Option<String>,
///     name: String,
///     // Any type implementing default can be flagged as default
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
pub trait Attribute: Sized {
    /// Helper struct for storing and parsing attributes
    type Parser: TryExtendOne + Parse + Default;

    /// Handles conversion from `Parser` used by the other functions internally
    fn from_parser(parser: Self::Parser) -> Result<Self>;

    /// Parses an [`IntoIterator`] of [`syn::Attributes`](syn::Attribute) e.g.
    /// [`Vec<Attribute>`](Vec). Only availible if you specify the attribute
    /// ident: `#[attribute(ident="<ident>")]` when using the derive macro.
    ///
    /// It can therefore parse fields set over multiple attributes like:
    /// ```text
    /// #[collection(authority = "Authority", name = "Name")]
    /// #[collection(views = [A, B])]
    /// ```
    /// and also catch duplicate/conflicting settings over those.
    ///
    /// This is best used for derive macros, where you don't need to remove your
    /// attributes.
    ///
    /// # Errors
    /// Fails with a [`syn::Error`] so you can conveniently return that as a
    /// compiler error in a proc macro in the following cases
    ///
    /// - A necessary parameter is omitted
    /// - Invalid input is given for a parameter
    /// - A non aggregating parameter is specified multiple times
    /// - An attribute called [`IDENTS`](const@AttributeIdent::IDENTS) has
    ///   invalid syntax (e.g. `#attr(a: "a")`)
    fn from_attributes<'a>(attrs: impl IntoIterator<Item = &'a syn::Attribute>) -> Result<Self>
    where
        Self: AttributeIdent,
    {
        attrs
            .into_iter()
            .filter_map(|attr| {
                Self::is_ident(&attr.path()).then(|| attr.parse_args::<Self::Parser>())
            })
            .try_fold(Self::Parser::default(), |mut acc, item| {
                acc.try_extend_one(item?)?;
                Ok(acc)
            })
            .and_then(Self::from_parser)
    }

    /// Parses an [`&mut Vec<syn::Attributes>`](syn::Attribute). Removing
    /// matching attributes. Only availible if you specify an ident:
    /// `#[attribute(ident="<ident>")]` when using the derive macro.
    ///
    /// It can therefore parse fields set over multiple attributes like:
    /// ```text
    /// #[collection(authority = "Authority", name = "Name")]
    /// #[collection(views = [A, B])]
    /// ```
    /// and also catch duplicate/conflicting settings over those.
    ///
    /// Use this if you are implementing an attribute macro, and need to remove
    /// your helper attributes.
    ///
    /// # Errors
    /// Fails with a [`syn::Error`] so you can conveniently return that as a
    /// compiler error in a proc macro in the following cases
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
        let mut parser: Self::Parser = Default::default();
        let mut i = 0;
        while i < attrs.len() {
            if Self::is_ident(&attrs[i].path()) {
                parser.try_extend_one(attrs.remove(i).parse_args()?)?;
            } else {
                i += 1;
            }
        }
        Self::from_parser(parser)
    }

    /// Parses a [`TokenStream`](proc_macro2::TokenStream).
    ///
    /// Useful for implementing general proc macros to parse the input of your
    /// macro.
    ///
    /// Due to this only parsing the input for a single attribute it is not able
    /// to aggregate input spread over multiple attributes.
    ///
    /// # Errors
    /// Fails with a [`syn::Error`] so you can conveniently return that as a
    /// compiler error in a proc macro in the following cases
    ///
    /// - A necessary parameter is omitted
    /// - Invalid input is given for a parameter
    /// - A non aggregating parameter is specified multiple times
    fn from_args(tokens: TokenStream) -> Result<Self> {
        parse2(tokens).and_then(Self::from_parser)
    }
}

/// Trait to join two structs of the same type
pub trait TryExtendOne {
    /// Try to extend self with another Self, failing if there are conflicts
    fn try_extend_one(&mut self, other: Self) -> Result<()>;
}

/// Helper trait to convert syn types implementing [`Parse`] like
/// [`LitStr`](struct@LitStr) to rust types like [`String`]
///
/// You probably don't need to implement this trait, as most syn types like
/// [`LitStr`](struct@LitStr) and [`Type`] or that have a direct equivalent in
/// those like [`String`], [`char`] or [`f32`] are already implemented. A
/// special treatment have [`Vecs`](Vec) which are parsed using the helper
/// [`Array`] with the syntax `[a, b, c]` and [`Options`](Option) that will be
/// [`None`] if not specified and [`Some`] when the value is specified via the
/// attribute. It is not specified via `Some(value)` but as just `value`.
pub trait ConvertParsed
where
    Self: Sized,
    Self::Type: Error,
{
    /// The type this can be converted from
    type Type;
    /// This takes [`Self::Type`] and converts it to [`Self`].
    ///
    /// This can return an error, e.g. when parsing an integer too large for a
    /// [`u8`] into an `u8`
    fn convert(value: Self::Type) -> Result<Self>;
    /// Should values of this type return their default when they are not
    /// specified even when the `default` flag is not specified (only
    /// returns `true` for [`Option`], [`Vec`] and [`bool`] currently)
    fn default_by_default() -> bool {
        false
    }
    /// The default value, this is necessary to implement the implicit default
    /// behavior of [`Option`] and [`bool`].
    ///
    /// This is necessary as the [`Default`] trait cannot be used in expanded
    /// code, but normally you can easily implement it using it:
    /// ```
    /// # use attribute_derive::ConvertParsed;
    /// # use syn::{Result, LitBool};
    /// # #[derive(Default)]
    /// # struct bool;
    /// impl ConvertParsed for bool {
    /// #   type Type = LitBool;
    /// #   fn convert(value: Self::Type) -> Result<Self> {
    /// #       unimplemented!()
    /// #   }
    ///     fn default() -> Self {
    ///         Default::default()
    ///     }
    /// }
    /// ```
    fn default() -> Self {
        unreachable!("default_by_default should only return true if this is overridden")
    }
    /// Returns the value when this type is specified as flag i.e. just
    /// `#[attr(default)]` instead of `#[attr(default=true)]`. This relies
    /// on [`Self::default`].
    fn as_flag() -> Option<Self::Type> {
        None
    }
    /// Should values of this type be aggregated instead of conflict if
    /// specified multiple times
    ///
    /// Currently this is only implemented for [`Arrays`](Array)
    #[allow(unused)]
    fn aggregate(
        this: Option<IdentValue<Self::Type>>,
        other: Option<IdentValue<Self::Type>>,
        error_msg: &str,
    ) -> Result<Option<IdentValue<Self::Type>>> {
        match (this, other) {
            (None, value) => Ok(value),
            (value, None) => Ok(value),
            (Some(this), Some(other)) => {
                let mut error = this.ident.error(error_msg);
                syn::Error::combine(&mut error, other.ident.error(error_msg));
                Err(error)
            }
        }
    }
}

/// Helper trait to generate sensible errors
pub trait Error {
    /// This is used to be able to create errors more easily. Mostly used
    /// through the implementation for [`T: ToTokens`](ToTokens).
    fn error(&self, message: impl Display) -> syn::Error;
}

impl<T> Error for T
where
    T: ToTokens,
{
    fn error(&self, message: impl Display) -> syn::Error {
        syn::Error::new_spanned(self, message)
    }
}

/// Helper struct to hold a value and the ident of its property
pub struct IdentValue<T> {
    /// The value
    pub value: T,
    /// The ident
    pub ident: Ident,
}

/// Macro to easily implement [`ConvertParsed`] for syn types
macro_rules! convert_parsed {
    ($(#[$meta:meta])* $type:path) => {
        $(#[$meta])*
        impl ConvertParsed for $type {
            type Type = $type;
            fn convert(s: Self) -> Result<Self> {
                Ok(s)
            }
        }
    };
    [$($type:path),* $(,)?] => {
        $(
        impl ConvertParsed for $type {
            type Type = $type;
                fn convert(s: Self) -> Result<Self> {
                    Ok(s)
                }
            }
        )*
    };
    ($from:path => $to:path) => {
        impl ConvertParsed<$from> for $to {
            fn convert(value: $from) -> Result<$to> {
                Ok(value.into())
            }
        }
    };
    ($from:path => $($to:path),+ : $with:path ) => {
        $(
            impl ConvertParsed for $to {
                type Type = $from;
                fn convert(value: $from) -> Result<$to> {
                    Ok($with(&value))
                }
            }
        )*
    };
    ($from:path => $($to:path),+ :? $with:path ) => {
        $(
            impl ConvertParsed for $to {
                type Type = $from;
                fn convert(value: $from) -> Result<$to> {
                    $with(&value)
                }
            }
        )*
    };
}

impl<Output, Parsed> ConvertParsed for Option<Output>
where
    Output: ConvertParsed<Type = Parsed>,
    Parsed: Error + Clone,
{
    type Type = Parsed;

    fn convert(s: Parsed) -> Result<Self> {
        Ok(Some(ConvertParsed::convert(s)?))
    }

    fn default_by_default() -> bool {
        true
    }

    fn default() -> Self {
        Default::default()
    }
}

impl<Output, Parsed> ConvertParsed for Vec<Output>
where
    Output: ConvertParsed<Type = Parsed>,
    Parsed: Clone,
{
    type Type = Array<Parsed>;

    fn convert(array: Array<Parsed>) -> Result<Self> {
        array.data.into_iter().map(ConvertParsed::convert).collect()
    }

    fn aggregate(
        this: Option<IdentValue<Self::Type>>,
        other: Option<IdentValue<Self::Type>>,
        _: &str,
    ) -> Result<Option<IdentValue<Self::Type>>> {
        Ok(match (this, other) {
            (None, None) => None,
            (None, value) => value,
            (value, None) => value,
            (Some(mut this), Some(other)) => {
                this.value.data.extend_from_slice(&other.value.data);
                this.value.span = this
                    .value
                    .span
                    .join(other.value.span)
                    .unwrap_or(this.value.span);
                // TODO figure out what to do with the idents
                Some(this)
            }
        })
    }
}

impl ConvertParsed for bool {
    type Type = LitBool;

    fn convert(value: Self::Type) -> Result<Self> {
        Ok(value.value)
    }

    fn default_by_default() -> bool {
        true
    }

    fn default() -> Self {
        false
    }

    fn as_flag() -> Option<Self::Type> {
        Some(parse_quote!(true))
    }
}

/// Helper struct to parse array literals:
/// `[a, b, c]`
#[derive(Clone)]
pub struct Array<T> {
    data: Vec<T>,
    span: Span,
}

impl<T> Parse for Array<T>
where
    T: Parse,
{
    fn parse(input: syn::parse::ParseStream) -> Result<Self> {
        let content;
        let b = bracketed!(content in input);
        let i = Punctuated::<T, Token!(,)>::parse_terminated(&content)?;
        Ok(Self {
            data: i.into_iter().collect(),
            span: b.span.join(),
        })
    }
}

impl<T> Error for Array<T> {
    fn error(&self, message: impl Display) -> syn::Error {
        syn::Error::new(self.span, message)
    }
}

convert_parsed!(Type);
convert_parsed!(Path);
convert_parsed!(Lit);
convert_parsed![LitStr, LitByteStr, LitChar, LitInt, LitFloat, LitBool];
convert_parsed!(Expr);
convert_parsed![TokenTree, Group, Punct, Literal];

// TODO make this warning better visable
convert_parsed! {
    /// Try to avoid using this, as it will consume everything behind, so it needs to be defined as the
    /// last parameter.
    ///
    /// In the future there might be something to allow better handling of this (maybe by putting it
    /// into `()`)
    TokenStream
}

convert_parsed!(LitStr => String: LitStr::value);
// TODO convert_parsed!(LitByteStr => Vec<u8>: LitByteStr::value);
convert_parsed!(LitChar => char: LitChar::value);
convert_parsed!(LitInt => u8, i8, u16, i16, u32, i32, u64, i64, u128, i128, usize, isize:? LitInt::base10_parse);
convert_parsed!(LitFloat => f32, f64:? LitFloat::base10_parse);

// Some probably useless stuff
convert_parsed![
    Abi,
    Abstract,
    Plus,
    PlusEq,
    And,
    AndAnd,
    AndEq,
    AngleBracketedGenericArguments,
    As,
    Async,
    At,
    Auto,
    Await,
    Not,
    BareFnArg,
    Become,
    BinOp,
    BoundLifetimes,
    Break,
    Caret,
    CaretEq,
    Colon,
    PathSep,
    Comma,
    Const,
    ConstParam,
    Constraint,
    Continue,
    Crate,
    DeriveInput,
    Slash,
    SlashEq,
    Do,
    Dollar,
    Dot,
    DotDot,
    DotDotDot,
    DotDotEq,
    Dyn,
    Else,
    Enum,
    Eq,
    EqEq,
    ExprArray,
    ExprAssign,
    ExprAsync,
    ExprBinary,
    ExprBlock,
    ExprBreak,
    ExprCall,
    ExprCast,
    ExprClosure,
    ExprContinue,
    ExprField,
    ExprForLoop,
    ExprIf,
    ExprIndex,
    ExprLet,
    ExprLit,
    ExprLoop,
    ExprMacro,
    ExprMatch,
    ExprMethodCall,
    ExprParen,
    ExprPath,
    ExprRange,
    ExprReference,
    ExprRepeat,
    ExprReturn,
    ExprStruct,
    ExprTry,
    ExprTryBlock,
    ExprTuple,
    ExprUnary,
    ExprUnsafe,
    ExprWhile,
    ExprYield,
    Extern,
    FatArrow,
    FieldsNamed,
    FieldsUnnamed,
    Final,
    Fn,
    For,
    Ge,
    GenericArgument,
    GenericParam,
    Generics,
    Gt,
    Ident,
    If,
    Impl,
    In,
    Index,
    LArrow,
    Le,
    Let,
    Lifetime,
    Loop,
    Lt,
    Match,
    Member,
    Meta,
    MetaList,
    MetaNameValue,
    Mod,
    Move,
    StarEq,
    Mut,
    Ne,
    Or,
    OrEq,
    OrOr,
    Override,
    ParenthesizedGenericArguments,
    PathSegment,
    Pound,
    Priv,
    Pub,
    Question,
    RArrow,
    Ref,
    Percent,
    PercentEq,
    Return,
    ReturnType,
    SelfType,
    SelfValue,
    Semi,
    Shl,
    ShlEq,
    Shr,
    ShrEq,
    Star,
    Static,
    Struct,
    Minus,
    MinusEq,
    Super,
    Tilde,
    Trait,
    TraitBound,
    TraitBoundModifier,
    Try,
    TypeArray,
    TypeBareFn,
    TypeGroup,
    TypeImplTrait,
    TypeInfer,
    TypeMacro,
    TypeNever,
    TypeParam,
    TypeParamBound,
    TypeParen,
    TypePath,
    TypePtr,
    TypeReference,
    TypeSlice,
    TypeTraitObject,
    TypeTuple,
    Typeof,
    UnOp,
    Underscore,
    Union,
    Unsafe,
    Unsized,
    Use,
    Variant,
    Virtual,
    Visibility,
    Where,
    WhereClause,
    WherePredicate,
    While,
    Yield,
    syn::Macro,
    syn::token::Box,
    syn::token::Default,
    syn::token::Macro,
    syn::token::Type,
];

#[cfg(feature = "syn-full")]
mod syn_full {
    use syn::{
        Arm, Block, FieldValue, File, FnArg, ForeignItem, ForeignItemFn, ForeignItemMacro,
        ForeignItemStatic, ForeignItemType, ImplItem, ImplItemConst, ImplItemMacro, ImplItemType,
        Item, ItemConst, ItemEnum, ItemExternCrate, ItemFn, ItemForeignMod, ItemImpl, ItemMacro,
        ItemMod, ItemStatic, ItemStruct, ItemTrait, ItemTraitAlias, ItemType, ItemUnion, ItemUse,
        Label, Pat, RangeLimits, Receiver, Signature, Stmt, TraitItem, TraitItemConst,
        TraitItemMacro, TraitItemType, UseTree,
    };

    use super::*;

    convert_parsed![
        Arm,
        Block,
        FieldValue,
        File,
        FnArg,
        ForeignItem,
        ForeignItemFn,
        ForeignItemMacro,
        ForeignItemStatic,
        ForeignItemType,
        ImplItem,
        ImplItemConst,
        ImplItemMacro,
        ImplItemType,
        Item,
        ItemConst,
        ItemEnum,
        ItemExternCrate,
        ItemFn,
        ItemForeignMod,
        ItemImpl,
        ItemMacro,
        ItemMod,
        ItemStatic,
        ItemStruct,
        ItemTrait,
        ItemTraitAlias,
        ItemType,
        ItemUnion,
        ItemUse,
        Label,
        Pat,
        RangeLimits,
        Receiver,
        Signature,
        Stmt,
        TraitItem,
        TraitItemConst,
        TraitItemMacro,
        TraitItemType,
        UseTree,
    ];
}

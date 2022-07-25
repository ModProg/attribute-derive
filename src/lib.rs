//! Basicly clap for attribute macros:
//! ```ignore
//! #[derive(Attribute)]
//! #[attribute(ident = "collection")]
//! #[attribute(invalid_field = "Error when an unsupported value is set (e.g. meaning=42")]
//! struct CollectionAttribute {
//!     // Options are optional by default (will be set to None if not specified)
//!     authority: Option<String>,
//!     #[attribute(missing = "Error when the value is not set")]
//!     name: String,
//!     // Any type implementing default can be flagged as default
//!     // This will be set to Vec::default() when not specified
//!     #[attribute(default)]
//!     #[attribute(expected = "Error when an error occured while parsing")]
//!     views: Vec<Type>,
//!     // Booleans can be used without assiging a value. as a flag.
//!     // If omitted they are set to false
//!     some_flag
//! }
//! ```
//!
//! Will be able to parse an attribute like this:
//! ```ignore
//! #[collection(authority="Some String", name = r#"Another string"#, views = [Option, ()])]
//! ```
//!
//! Any type that [`ConvertParsed`] is implemented for is supported. These should be the general
//! types that syn supports like [`LitStr`](struct@LitStr) or [`Type`] or that have a direct equivalent in those
//! like [`String`], [`char`] or [`f32`]. A special treatment have [`Vecs`](Vec) which are parsed
//! using [`Array`] with the syntax `[a, b, c]` and [`Options`](Option) that will be [`None`] if
//! not specified and [`Some`] when the value is specified via the attribute. It is not
//! specified via `Some(value)` but as just `value`.
//!
//! ## Limitations
//!
//! There are some limitations in syntax parsing that will be lifted future releases.
//!
//! - literals in top level (meaning something like `#[attr(42, 3.14, "hi")]`
//! - function like arguments (something like `#[attr(view(a = "test"))]`
//! - other syntaxes, maybe something like `key: value`
//!
//! ## Parse methods
//!
//! There are multiple ways of parsing a struct deriving [`Attribute`].
//!
//! For helper attributes there is:
//! - [`Attribute::from_attributes`] which takes in an [`IntoIterator<Item = &'a
//! syn::Attribute`](syn::Attribute)
//! (e.g. a [`&Vec<syn::Attribute>`](syn::Attribute)). Most useful for derive macros.
//! - [`Attribute::remove_attributes`] which takes an [`&mut Vec<syn::Attribute>`](syn::Attribute)
//! and does not only parse the [`Attribute`] but also removes those matching. Useful for helper
//! attributes for proc macros, where the helper attributes need to be removed.
//!
//! For parsing a single [`TokenStream`] e.g. for parsing the proc macro input there a two ways:
//!
//! - [`Attribute::from_args`] taking in a [`TokenStream`]
//! - As `derive(Attribute)` also derives [`Parse`] so you can use the [parse](mod@syn::parse) API,
//! e.g. with [`parse_macro_input!(tokens as Attribute)`](syn::parse_macro_input!).
use std::fmt::Display;

#[doc(hidden)]
pub use attribute_derive_macro::Attribute;
use proc_macro2::{Group, Literal, Punct, Span, TokenStream, TokenTree};
use quote::ToTokens;
use syn::{
    bracketed,
    parse::{Parse, Result},
    parse2, parse_quote,
    punctuated::Punctuated,
    token::{
        Abstract, Add, AddEq, And, AndAnd, AndEq, As, Async, At, Auto, Await, Bang, Become, Break,
        Caret, CaretEq, Colon, Colon2, Comma, Const, Continue, Crate, Div, DivEq, Do, Dollar, Dot,
        Dot2, Dot3, DotDotEq, Dyn, Else, Enum, Eq, EqEq, Extern, FatArrow, Final, Fn, For, Ge, Gt,
        If, Impl, In, LArrow, Le, Let, Loop, Lt, Match, Mod, Move, MulEq, Mut, Ne, Or, OrEq, OrOr,
        Override, Pound, Priv, Pub, Question, RArrow, Ref, Rem, RemEq, Return, SelfType, SelfValue,
        Semi, Shl, ShlEq, Shr, ShrEq, Star, Static, Struct, Sub, SubEq, Super, Tilde, Trait, Try,
        Typeof, Underscore, Union, Unsafe, Unsized, Use, Virtual, Where, While, Yield,
    },
    Abi, AngleBracketedGenericArguments, Arm, BareFnArg, BinOp, Binding, Block, BoundLifetimes,
    ConstParam, Constraint, DeriveInput, Expr, ExprArray, ExprAssign, ExprAssignOp, ExprAsync,
    ExprBinary, ExprBlock, ExprBox, ExprBreak, ExprCall, ExprCast, ExprClosure, ExprContinue,
    ExprField, ExprForLoop, ExprIf, ExprIndex, ExprLet, ExprLit, ExprLoop, ExprMacro, ExprMatch,
    ExprMethodCall, ExprParen, ExprPath, ExprRange, ExprReference, ExprRepeat, ExprReturn,
    ExprStruct, ExprTry, ExprTryBlock, ExprTuple, ExprType, ExprUnary, ExprUnsafe, ExprWhile,
    ExprYield, FieldValue, FieldsNamed, FieldsUnnamed, File, FnArg, ForeignItem, ForeignItemFn,
    ForeignItemMacro, ForeignItemStatic, ForeignItemType, GenericArgument, GenericMethodArgument,
    GenericParam, Generics, Ident, ImplItem, ImplItemConst, ImplItemMacro, ImplItemMethod,
    ImplItemType, Index, Item, ItemConst, ItemEnum, ItemExternCrate, ItemFn, ItemForeignMod,
    ItemImpl, ItemMacro, ItemMacro2, ItemMod, ItemStatic, ItemStruct, ItemTrait, ItemTraitAlias,
    ItemType, ItemUnion, ItemUse, Label, Lifetime, LifetimeDef, Lit, LitBool, LitByteStr, LitChar,
    LitFloat, LitInt, LitStr, Member, Meta, MetaList, MetaNameValue, NestedMeta,
    ParenthesizedGenericArguments, Pat, Path, PathSegment, RangeLimits, Receiver, ReturnType,
    Signature, Stmt, Token, TraitBound, TraitBoundModifier, TraitItem, TraitItemConst,
    TraitItemMacro, TraitItemMethod, TraitItemType, Type, TypeArray, TypeBareFn, TypeGroup,
    TypeImplTrait, TypeInfer, TypeMacro, TypeNever, TypeParam, TypeParamBound, TypeParen, TypePath,
    TypePtr, TypeReference, TypeSlice, TypeTraitObject, TypeTuple, UnOp, UseTree, Variant,
    Visibility, WhereClause, WherePredicate,
};

#[deny(missing_docs)]
#[doc(hidden)]
pub mod __private {
    pub use proc_macro2;
    pub use syn;
}

/// Helper trait providing the path for an attribute.
///
/// Automatically derived with [`Attribute`], if `#[attribute(ident =
/// "some_ident")]` is provided.
/// ```
pub trait AttributeIdent: Sized {
    const ATTRIBUTE_IDENT: &'static str;
    fn get_attribute_ident() -> &'static str {
        Self::ATTRIBUTE_IDENT
    }
}

/// The trait you actually derive on your attribute struct.
///
/// Basic gist is a struct like this:
/// ```ignore
/// #[derive(Attribute)]
/// #[attribute(ident = "collection")]
/// #[attribute(invalid_field = "Error when an unsupported value is set (e.g. meaning=42")]
/// struct CollectionAttribute {
///     // Options are optional by default (will be set to None if not specified)
///     authority: Option<String>,
///     #[attribute(missing = "Error when the value is not set")]
///     name: String,
///     // Any type implementing default can be flagged as default
///     // This will be set to Vec::default() when not specified
///     #[attribute(default)]
///     #[attribute(expected = "Error when an error occured while parsing")]
///     views: Vec<Type>,
///     // Booleans can be used without assiging a value. as a flag.
///     // If omitted they are set to false
///     some_flag: bool
/// }
/// ```
///
/// Will be able to parse an attribute like this:
/// ```ignore
/// #[collection(authority="Some String", name = r#"Another string"#, views = [Option, ()], some_flag)]
/// ```
pub trait Attribute: Sized {
    type Parser: TryExtendOne + Parse + Default;

    #[doc(hidden)]
    fn from_parser(parser: Self::Parser) -> Result<Self>;

    /// Parses an [`IntoIterator`] of [`syn::Attributes`](syn::Attribute) e.g.
    /// [`Vec<Attribute>`](Vec).
    ///
    /// It can therefore parse fields set over multiple attributes like:
    /// ```ignore
    /// #[collection(authority = "Authority", name = "Name")]
    /// #[collection(views = [A, B])]
    /// ```
    /// and also catch duplicate/conflicting settings over those.
    ///
    /// This is best used for derive macros, where you don't need to remove your attributes.
    ///
    /// # Errors
    /// Fails with a [`syn::Error`] so you can conveniently return that as a compiler error in a proc
    /// macro in the following cases
    ///
    /// - A necessary parameter is omitted
    /// - Invalid input is given for a parameter
    /// - A non aggregating parameter is specified multiple times
    /// - An attribute called [`IDENT`](Self::IDENT) has invalid syntax (e.g. `#attr(a: "a")`)
    fn from_attributes<'a>(attrs: impl IntoIterator<Item = &'a syn::Attribute>) -> Result<Self>
    where
        Self: AttributeIdent,
    {
        attrs
            .into_iter()
            .filter_map(|attr| {
                attr.path
                    .is_ident(Self::ATTRIBUTE_IDENT)
                    .then(|| attr.parse_args::<Self::Parser>())
            })
            .try_fold(Self::Parser::default(), |mut acc, item| {
                acc.try_extend_one(item?)?;
                Ok(acc)
            })
            .and_then(Self::from_parser)
    }

    /// Parses an [`&mut Vec<syn::Attributes>`](syn::Attribute). Removing matching attributes.
    ///
    /// It can therefore parse fields set over multiple attributes like:
    /// ```ignore
    /// #[collection(authority = "Authority", name = "Name")]
    /// #[collection(views = [A, B])]
    /// ```
    /// and also catch duplicate/conflicting settings over those.
    ///
    /// Use this if you are implementing an attribute macro, and need to remove your helper
    /// attributes.
    ///
    /// # Errors
    /// Fails with a [`syn::Error`] so you can conveniently return that as a compiler error in a proc
    /// macro in the following cases
    ///
    /// - A necessary parameter is omitted
    /// - Invalid input is given for a parameter
    /// - A non aggregating parameter is specified multiple times
    /// - An attribute called [`IDENT`](Self::IDENT) has invalid syntax (e.g. `#attr(a: "a")`)
    fn remove_attributes(attrs: &mut Vec<syn::Attribute>) -> Result<Self>
    where
        Self: AttributeIdent,
    {
        let mut parser: Self::Parser = Default::default();
        let mut i = 0;
        while i < attrs.len() {
            if attrs[i].path.is_ident(Self::ATTRIBUTE_IDENT) {
                parser.try_extend_one(attrs.remove(i).parse_args()?)?;
            } else {
                i += 1;
            }
        }
        Self::from_parser(parser)
    }

    /// Parses a [`TokenStream`](proc_macro2::TokenStream).
    ///
    /// Useful for implementing general proc macros to parse the input of your macro.
    ///
    /// Due to this only parsing the input for a single attribute it is not able to aggregate input
    /// spread over multiple attributes.
    ///
    /// # Errors
    /// Fails with a [`syn::Error`] so you can conveniently return that as a compiler error in a proc
    /// macro in the following cases
    ///
    /// - A necessary parameter is omitted
    /// - Invalid input is given for a parameter
    /// - A non aggregating parameter is specified multiple times
    /// - An attribute called [`IDENT`](Self::IDENT) has invalid syntax (e.g. `#attr(a: "a")`)
    fn from_args(tokens: TokenStream) -> Result<Self> {
        parse2(tokens).and_then(Self::from_parser)
    }
}

pub trait TryExtendOne {
    fn try_extend_one(&mut self, other: Self) -> Result<()>;
}

/// Helper trait to convert syn types implementing [`Parse`] like [`LitStr`](struct@LitStr) to rust
/// types like [`String`]
///
/// You probably don't need to implement this trait, as most syn types like [`LitStr`](struct@LitStr)
/// and [`Type`] or that have a direct equivalent in those like [`String`], [`char`] or [`f32`] are
/// already implemented. A special treatment have [`Vecs`](Vec) which are parsed
/// using the helper [`Array`] with the syntax `[a, b, c]` and [`Options`](Option) that will be
/// [`None`] if not specified and [`Some`] when the value is specified via the attribute. It is not
/// specified via `Some(value)` but as just `value`.
pub trait ConvertParsed
where
    Self: Sized,
    Self::Type: Error,
{
    /// The type this can be converted from
    type Type;
    /// This takes [`Self::Type`] and converts it to [`Self`].
    ///
    /// This can return an error, e.g. when parsing an integer too large for a [`u8`] into an `u8`
    fn convert(value: Self::Type) -> Result<Self>;
    /// Should values of this type return their default when they are not specified even when the
    /// `default` flag is not specified (only returns `true` for [`Option`], [`Vec`] and [`bool`] currently)
    fn default_by_default() -> bool {
        false
    }
    /// The default value, this is necessary to implement the implicit default behavior of
    /// [`Option`] and [`bool`].
    ///
    /// This is necessary as the [`Default`] trait cannot be used in expanded code, but normally you
    /// can easily implement it using it:
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
    /// Returns the value when this type is specified as flag i.e. just `#[attr(default)]`
    /// instead of `#[attr(default=true)]`. This relies on [`Self::default`].
    fn as_flag() -> Option<Self::Type> {
        None
    }
    /// Should values of this type be aggregated instead of conflict if specified multiple times
    ///
    /// Currently this is only implemented for [`Arrays`](Array)
    #[allow(unused)]
    fn aggregate(
        this: Option<Self::Type>,
        other: Option<Self::Type>,
        error1: &str,
        error2: &str,
    ) -> Result<Option<Self::Type>> {
        match (this, other) {
            (None, value) => Ok(value),
            (value, None) => Ok(value),
            (Some(this), Some(other)) => {
                let mut error = this.error(error1);
                syn::Error::combine(&mut error, other.error(error2));
                Err(error)
            }
        }
    }
}

/// Helper trait to generate sensible errors
pub trait Error {
    /// This is used to be able to create errors more easily. Mostly used through the
    /// implementation for [`T: ToTokens`](ToTokens).
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
    [$($type:path),*] => {
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
        this: Option<Self::Type>,
        other: Option<Self::Type>,
        _: &str,
        _: &str,
    ) -> Result<Option<Self::Type>> {
        Ok(match (this, other) {
            (None, None) => None,
            (None, value) => value,
            (value, None) => value,
            (Some(mut this), Some(other)) => {
                this.data.extend_from_slice(&other.data);
                this.span = this.span.join(other.span).unwrap_or(this.span);
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
            span: b.span,
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
    BinOp,
    FnArg,
    ForeignItem,
    GenericArgument,
    GenericMethodArgument,
    GenericParam,
    ImplItem,
    Item,
    Member,
    Meta,
    NestedMeta,
    Pat,
    RangeLimits,
    ReturnType,
    Stmt,
    TraitBoundModifier,
    TraitItem,
    TypeParamBound,
    UnOp,
    UseTree,
    Visibility,
    WherePredicate,
    Abi,
    AngleBracketedGenericArguments,
    Arm,
    BareFnArg,
    Binding,
    Block,
    BoundLifetimes,
    ConstParam,
    Constraint,
    DeriveInput,
    ExprArray,
    ExprAssign,
    ExprAssignOp,
    ExprAsync,
    ExprBinary,
    ExprBlock,
    ExprBox,
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
    ExprType,
    ExprUnary,
    ExprUnsafe,
    ExprWhile,
    ExprYield,
    FieldValue,
    FieldsNamed,
    FieldsUnnamed,
    File,
    ForeignItemFn,
    ForeignItemMacro,
    ForeignItemStatic,
    ForeignItemType,
    Generics,
    Ident,
    ImplItemConst,
    ImplItemMacro,
    ImplItemMethod,
    ImplItemType,
    Index,
    ItemConst,
    ItemEnum,
    ItemExternCrate,
    ItemFn,
    ItemForeignMod,
    ItemImpl,
    ItemMacro2,
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
    Lifetime,
    LifetimeDef,
    syn::Macro,
    MetaList,
    MetaNameValue,
    ParenthesizedGenericArguments,
    PathSegment,
    Receiver,
    Signature,
    TraitBound,
    TraitItemConst,
    TraitItemMacro,
    TraitItemMethod,
    TraitItemType,
    TypeArray,
    TypeBareFn,
    TypeGroup,
    TypeImplTrait,
    TypeInfer,
    TypeMacro,
    TypeNever,
    TypeParam,
    TypeParen,
    TypePath,
    TypePtr,
    TypeReference,
    TypeSlice,
    TypeTraitObject,
    TypeTuple,
    Variant,
    WhereClause,
    Abstract,
    Add,
    AddEq,
    And,
    AndAnd,
    AndEq,
    As,
    Async,
    At,
    Auto,
    Await,
    Bang,
    Become,
    syn::token::Box,
    Break,
    Caret,
    CaretEq,
    Colon2,
    Colon,
    Comma,
    Const,
    Continue,
    Crate,
    syn::token::Default,
    Div,
    DivEq,
    Do,
    Dollar,
    Dot2,
    Dot3,
    Dot,
    DotDotEq,
    Dyn,
    Else,
    Enum,
    Eq,
    EqEq,
    Extern,
    FatArrow,
    Final,
    Fn,
    For,
    Ge,
    Gt,
    If,
    Impl,
    In,
    LArrow,
    Le,
    Let,
    Loop,
    Lt,
    syn::token::Macro,
    Match,
    Mod,
    Move,
    MulEq,
    Mut,
    Ne,
    Or,
    OrEq,
    OrOr,
    Override,
    Pound,
    Priv,
    Pub,
    Question,
    RArrow,
    Ref,
    Rem,
    RemEq,
    Return,
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
    Sub,
    SubEq,
    Super,
    Tilde,
    Trait,
    Try,
    syn::token::Type,
    Typeof,
    Underscore,
    Union,
    Unsafe,
    Unsized,
    Use,
    Virtual,
    Where,
    While,
    Yield
];

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
use std::fmt::Display;

use proc_macro2::{Literal, Span};
#[doc(hidden)]
pub use r#macro::Attribute;
use syn::{
    bracketed, parse::Parse, punctuated::Punctuated, Expr, Lit, LitBool, LitByteStr, LitChar,
    LitFloat, LitInt, LitStr, Path, Result, Token, Type, __private::ToTokens, parse_quote,
};

#[deny(missing_docs)]
#[doc(hidden)]
pub mod __private {
    pub use proc_macro2;
    pub use syn;
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
pub trait Attribute
where
    Self: Sized,
{
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
    /// Fails with a [`syn::Error`] so you can conveniently return that as a compiler error in a proc
    /// macro.
    fn from_attributes(attrs: impl IntoIterator<Item = syn::Attribute>) -> Result<Self>;
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
    /// `default` flag is not specified (only returns `true` for [`Option`] currently)
    fn default_by_default() -> bool {
        false
    }
    /// The default value, this is necessary to implement the implicit default behavior of
    /// [`Option`]
    fn default() -> Self {
        unreachable!("default_by_default should only return true if this is overridden")
    }
    /// Should values of this type be able to be defined as flag i.e. just `#[attr(default)]`
    /// instead of `#[attr(default=true)]`
    fn as_flag() -> Option<Self::Type> {
        None
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
    ($type:path) => {
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
    Parsed: Error,
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
{
    type Type = Array<Parsed>;
    fn convert(array: Array<Parsed>) -> Result<Self> {
        array.data.into_iter().map(ConvertParsed::convert).collect()
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
convert_parsed![LitStr, LitByteStr, LitChar, LitInt, LitFloat, LitBool, Literal];
convert_parsed!(Expr);

convert_parsed!(LitStr => String: LitStr::value);
// TODO convert_parsed!(LitByteStr => Vec<u8>: LitByteStr::value);
convert_parsed!(LitChar => char: LitChar::value);
convert_parsed!(LitInt => u8, i8, u16, i16, u32, i32, u64, i64, u128, i128, usize, isize:? LitInt::base10_parse);
convert_parsed!(LitFloat => f32, f64:? LitFloat::base10_parse);

// TODO convert most of these
// impl Parse for Group
// impl Parse for Punct
// impl Parse for BinOp
// impl Parse for FnArg
// impl Parse for ForeignItem
// impl Parse for GenericArgument
// impl Parse for GenericParam
// impl Parse for ImplItem
// impl Parse for Item
// impl Parse for Member
// impl Parse for Meta
// impl Parse for NestedMeta
// impl Parse for Pat
// impl Parse for RangeLimits
// impl Parse for ReturnType
// impl Parse for Stmt
// impl Parse for TraitBoundModifier
// impl Parse for TraitItem
// impl Parse for TypeParamBound
// impl Parse for UnOp
// impl Parse for UseTree
// impl Parse for Visibility
// impl Parse for WherePredicate
// impl Parse for Abi
// impl Parse for AngleBracketedGenericArguments
// impl Parse for Arm
// impl Parse for BareFnArg
// impl Parse for Binding
// impl Parse for Block
// impl Parse for BoundLifetimes
// impl Parse for ConstParam
// impl Parse for Constraint
// impl Parse for DeriveInput
// impl Parse for ExprArray
// impl Parse for ExprAssign
// impl Parse for ExprAssignOp
// impl Parse for ExprAsync
// impl Parse for ExprBinary
// impl Parse for ExprBlock
// impl Parse for ExprBox
// impl Parse for ExprBreak
// impl Parse for ExprCall
// impl Parse for ExprCast
// impl Parse for ExprClosure
// impl Parse for ExprContinue
// impl Parse for ExprField
// impl Parse for ExprForLoop
// impl Parse for ExprIf
// impl Parse for ExprIndex
// impl Parse for ExprLet
// impl Parse for ExprLit
// impl Parse for ExprLoop
// impl Parse for ExprMacro
// impl Parse for ExprMatch
// impl Parse for ExprMethodCall
// impl Parse for ExprParen
// impl Parse for ExprPath
// impl Parse for ExprRange
// impl Parse for ExprReference
// impl Parse for ExprRepeat
// impl Parse for ExprReturn
// impl Parse for ExprStruct
// impl Parse for ExprTry
// impl Parse for ExprTryBlock
// impl Parse for ExprTuple
// impl Parse for ExprType
// impl Parse for ExprUnary
// impl Parse for ExprUnsafe
// impl Parse for ExprWhile
// impl Parse for ExprYield
// impl Parse for FieldValue
// impl Parse for FieldsNamed
// impl Parse for FieldsUnnamed
// impl Parse for File
// impl Parse for ForeignItemFn
// impl Parse for ForeignItemMacro
// impl Parse for ForeignItemStatic
// impl Parse for ForeignItemType
// impl Parse for Generics
// impl Parse for Ident
// impl Parse for ImplItemConst
// impl Parse for ImplItemMacro
// impl Parse for ImplItemMethod
// impl Parse for ImplItemType
// impl Parse for Index
// impl Parse for ItemConst
// impl Parse for ItemEnum
// impl Parse for ItemExternCrate
// impl Parse for ItemFn
// impl Parse for ItemForeignMod
// impl Parse for ItemImpl
// impl Parse for ItemMacro2
// impl Parse for ItemMacro
// impl Parse for ItemMod
// impl Parse for ItemStatic
// impl Parse for ItemStruct
// impl Parse for ItemTrait
// impl Parse for ItemTraitAlias
// impl Parse for ItemType
// impl Parse for ItemUnion
// impl Parse for ItemUse
// impl Parse for Label
// impl Parse for Lifetime
// impl Parse for LifetimeDef
// impl Parse for syn::Macro
// impl Parse for MetaList
// impl Parse for MetaNameValue
// impl Parse for ParenthesizedGenericArguments
// impl Parse for PathSegment
// impl Parse for Receiver
// impl Parse for Signature
// impl Parse for TraitBound
// impl Parse for TraitItemConst
// impl Parse for TraitItemMacro
// impl Parse for TraitItemMethod
// impl Parse for TraitItemType
// impl Parse for TypeArray
// impl Parse for TypeBareFn
// impl Parse for TypeGroup
// impl Parse for TypeImplTrait
// impl Parse for TypeInfer
// impl Parse for TypeMacro
// impl Parse for TypeNever
// impl Parse for TypeParam
// impl Parse for TypeParen
// impl Parse for TypePath
// impl Parse for TypePtr
// impl Parse for TypeReference
// impl Parse for TypeSlice
// impl Parse for TypeTraitObject
// impl Parse for TypeTuple
// impl Parse for Variant
// impl Parse for WhereClause
// impl Parse for Abstract
// impl Parse for Add
// impl Parse for AddEq
// impl Parse for And
// impl Parse for AndAnd
// impl Parse for AndEq
// impl Parse for As
// impl Parse for Async
// impl Parse for At
// impl Parse for Auto
// impl Parse for Await
// impl Parse for Bang
// impl Parse for Become
// impl Parse for syn::token::Box
// impl Parse for Break
// impl Parse for Caret
// impl Parse for CaretEq
// impl Parse for Colon2
// impl Parse for Colon
// impl Parse for Comma
// impl Parse for Const
// impl Parse for Continue
// impl Parse for Crate
// impl Parse for Default
// impl Parse for Div
// impl Parse for DivEq
// impl Parse for Do
// impl Parse for Dollar
// impl Parse for Dot2
// impl Parse for Dot3
// impl Parse for Dot
// impl Parse for DotDotEq
// impl Parse for Dyn
// impl Parse for Else
// impl Parse for Enum
// impl Parse for Eq
// impl Parse for EqEq
// impl Parse for Extern
// impl Parse for FatArrow
// impl Parse for Final
// impl Parse for Fn
// impl Parse for For
// impl Parse for Ge
// impl Parse for Gt
// impl Parse for If
// impl Parse for Impl
// impl Parse for In
// impl Parse for LArrow
// impl Parse for Le
// impl Parse for Let
// impl Parse for Loop
// impl Parse for Lt
// impl Parse for syn::token::Macro
// impl Parse for Match
// impl Parse for Mod
// impl Parse for Move
// impl Parse for MulEq
// impl Parse for Mut
// impl Parse for Ne
// impl Parse for Or
// impl Parse for OrEq
// impl Parse for OrOr
// impl Parse for Override
// impl Parse for Pound
// impl Parse for Priv
// impl Parse for Pub
// impl Parse for Question
// impl Parse for RArrow
// impl Parse for Ref
// impl Parse for Rem
// impl Parse for RemEq
// impl Parse for Return
// impl Parse for SelfType
// impl Parse for SelfValue
// impl Parse for Semi
// impl Parse for Shl
// impl Parse for ShlEq
// impl Parse for Shr
// impl Parse for ShrEq
// impl Parse for Star
// impl Parse for Static
// impl Parse for Struct
// impl Parse for Sub
// impl Parse for SubEq
// impl Parse for Super
// impl Parse for Tilde
// impl Parse for Trait
// impl Parse for Try
// impl Parse for syn::token::Type
// impl Parse for Typeof
// impl Parse for Underscore
// impl Parse for Union
// impl Parse for Unsafe
// impl Parse for Unsized
// impl Parse for Use
// impl Parse for Virtual
// impl Parse for Where
// impl Parse for While
// impl Parse for Yield
// impl Parse for Nothing

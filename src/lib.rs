//! The Idea is to parse smth like #[path(value, key=value, key(value))] for arbitrary types really.
//! Parsing works the same way as a function call... meaning any syntax valid ther + the additional
//! key=value. One important thing to keep in mind, is the limitation on values being valid
//! expresions so we can rely on syn for the , splitting :D

use std::fmt::Display;

use proc_macro2::{Literal, Span};
pub use r#macro::Attribute;
use syn::{
    bracketed, parse::Parse, punctuated::Punctuated, Expr, Lit, LitBool,
    LitByteStr, LitChar, LitFloat, LitInt, LitStr, Path, Result, Token, Type, __private::ToTokens,
};

pub mod __private {
    pub use proc_macro2;
    pub use syn;
}

pub trait Attribute
where
    Self: Sized,
{
    fn from_attributes(attrs: impl IntoIterator<Item = syn::Attribute>) -> Result<Self>;
}

pub trait ConvertParsed
where
    Self: Sized,
    Self::Type: Error,
{
    type Type;
    fn convert(value: Self::Type) -> Result<Self>;
}

/// Helper trait to generate sensible errors
pub trait Error {
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

/// Helper struct to parse array literals
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
convert_parsed!(LitBool => bool: LitBool::value);

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

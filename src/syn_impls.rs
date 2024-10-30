use proc_macro2::{Group, Literal, Punct, TokenStream, TokenTree};
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
    Abi, AngleBracketedGenericArguments, BareFnArg, BinOp, BoundLifetimes, ConstParam, DeriveInput,
    Expr, FieldsNamed, FieldsUnnamed, GenericArgument, GenericParam, Generics, Ident, Index,
    Lifetime, Lit, LitBool, LitByteStr, LitChar, LitFloat, LitInt, LitStr, Member, Meta, MetaList,
    MetaNameValue, ParenthesizedGenericArguments, Path, PathSegment, ReturnType, TraitBound,
    TraitBoundModifier, Type, TypeArray, TypeBareFn, TypeGroup, TypeImplTrait, TypeInfer,
    TypeMacro, TypeNever, TypeParam, TypeParamBound, TypeParen, TypePath, TypePtr, TypeReference,
    TypeSlice, TypeTraitObject, TypeTuple, UnOp, Variant, Visibility, WhereClause, WherePredicate,
};

use crate::parsing::*;
use crate::*;

/// Macro to easily implement [`AttributeValue`] for types implementing
/// [`Parse`] and [`ToTokens`].
#[macro_export]
macro_rules! impl_Attribute_for_Parse_and_ToTokens {
    ($($type:ty),+ $(,)?) => {$(
        impl $crate::parsing::AttributeBase for $type {
            type Partial = Self;
        }

        impl $crate::parsing::AttributeValue for $type {
            fn parse_value(input: $crate::__private::syn::parse::ParseStream) -> $crate::__private::syn::Result<$crate::parsing::SpannedValue<Self::Partial>> {
                input.parse().map($crate::parsing::SpannedValue::from_to_tokens)
            }
        }

        impl $crate::parsing::PositionalValue for $type {}

    )*}
}

impl AttributeBase for TokenStream {
    type Partial = Self;
}

impl AttributeMeta for TokenStream {
    fn parse_inner(input: ParseStream) -> Result<Self::Partial> {
        input.parse()
    }
}

// /// Macro to easily implement [`ConvertParsed`] for syn types.
// macro_rules! ParseToTokensAttribute {
//     ($(#[$meta:meta])* $type:path) => {
//         $(#[$meta])*
//         impl ConvertParsed for $type {
//             type Type = $type;
//             fn convert(s: Self) -> Result<Self> {
//                 Ok(s)
//             }
//         }
//     };
//     [$($type:path),* $(,)?] => {
//         $(
//         impl ConvertParsed for $type {
//             type Type = $type;
//             fn convert(s: Self) -> Result<Self> {
//                 Ok(s)
//             }
//             }
//         )*
//     };
//     ($from:path => $to:path) => {
//         impl ConvertParsed<$from> for $to {
//             fn convert(value: $from) -> Result<$to> {
//                 Ok(value.into())
//             }
//         }
//     };
//     ($from:path => $($to:path),+ : $with:path ) => {
//         $(
//             impl ConvertParsed for $to {
//                 type Type = $from;
//                 fn convert(value: $from) -> Result<$to> {
//                     Ok($with(&value))
//                 }
//             }
//         )*
//     };
//     ($from:path => $($to:path),+ :? $with:path ) => {
//         $(
//             impl ConvertParsed for $to {
//                 type Type = $from;
//                 fn convert(value: $from) -> Result<$to> {
//                     $with(&value)
//                 }
//             }
//         )*
//     };
// }

impl_Attribute_for_Parse_and_ToTokens!(Type);
impl_Attribute_for_Parse_and_ToTokens!(Path);
impl_Attribute_for_Parse_and_ToTokens!(Lit);
impl_Attribute_for_Parse_and_ToTokens![LitStr, LitByteStr, LitChar, LitInt, LitFloat, LitBool];
impl_Attribute_for_Parse_and_ToTokens!(Expr);
impl_Attribute_for_Parse_and_ToTokens![TokenTree, Group, Punct, Literal];

// // TODO make this warning better visable
// ParseToTokensAttribute! {
//     /// Try to avoid using this, as it will consume everything behind, so it
// needs to be defined as the     /// last parameter.
//     ///
//     /// In the future there might be something to allow better handling of
// this (maybe by putting it     /// into `()`)
//     TokenStream
// }

// Some probably useless stuff
impl_Attribute_for_Parse_and_ToTokens![
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
        Arm, Block, ExprArray, ExprAssign, ExprAsync, ExprBinary, ExprBlock, ExprBreak, ExprCall,
        ExprCast, ExprClosure, ExprContinue, ExprField, ExprForLoop, ExprIf, ExprIndex, ExprLet,
        ExprLit, ExprLoop, ExprMacro, ExprMatch, ExprMethodCall, ExprParen, ExprPath, ExprRange,
        ExprReference, ExprRepeat, ExprReturn, ExprStruct, ExprTry, ExprTryBlock, ExprTuple,
        ExprUnary, ExprUnsafe, ExprWhile, ExprYield, FieldValue, File, FnArg, ForeignItem,
        ForeignItemFn, ForeignItemMacro, ForeignItemStatic, ForeignItemType, ImplItem,
        ImplItemConst, ImplItemMacro, ImplItemType, Item, ItemConst, ItemEnum, ItemExternCrate,
        ItemFn, ItemForeignMod, ItemImpl, ItemMacro, ItemMod, ItemStatic, ItemStruct, ItemTrait,
        ItemTraitAlias, ItemType, ItemUnion, ItemUse, Label, RangeLimits, Receiver, Signature,
        Stmt, TraitItem, TraitItemConst, TraitItemMacro, TraitItemType, UseTree,
    };

    use super::*;

    impl_Attribute_for_Parse_and_ToTokens![
        Arm,
        Block,
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

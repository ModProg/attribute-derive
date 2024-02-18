use std::ops::Range;

use manyhow::{error_message, span_range, SpanRanged};
use proc_macro2::{Ident, Span, TokenStream};
use quote::{quote, ToTokens};
use syn::ext::IdentExt;
use syn::parse::discouraged::Speculative;
use syn::parse::{Parse, ParseStream, Parser};
use syn::spanned::Spanned;
use syn::token::{Bracket, Paren};
use syn::{bracketed, parenthesized, Expr, LitBool, LitInt, LitStr, Result, Token};

/// ```
/// _: Partial![usize];
/// ```
#[macro_export]
macro_rules! Partial {
    [$ty:ty] => {
        ::core::option::Option<Argument<<$ty as $crate::FromAttr>::Partial>>
    };
}
// Findings:
// - Positional arguments should disable merging of multiple attributes
// - Don't use Autoref, probably implementing for all syn types is more
//   reasonable...

// Move more parsing into the trait

// Split trait:
// PositionalAttribute
// NamedAttribute
// RootAttibute

struct Positional;
struct Named(&'static str);
trait Positionality {}
impl Positionality for Positional {}
impl Positionality for Named {}

// struct Flag;
// struct Value;
// trait Flagity {}
// impl Flagity for Flag {}
// impl Flagity for Value {}

// TODO: move into manyhow
pub struct Spanning<T>(pub T);

impl<T: ToTokens> SpanRanged for Spanning<T> {
    fn span_range(&self) -> Range<Span> {
        span_range!(self.0)
    }
}

#[derive(Debug)]
pub enum ArgName {
    Ident(Ident),
    Position(Range<Span>),
}

impl SpanRanged for ArgName {
    fn span_range(&self) -> Range<Span> {
        match self {
            ArgName::Ident(ident) => span_range!(ident),
            ArgName::Position(span) => span.clone(),
        }
    }
}

impl ArgName {
    pub fn join_spans(&self, other: &ArgName) -> Option<Span> {
        self.span_joined()
            .and_then(|this| other.span_joined().and_then(|other| this.join(other)))
    }

    pub fn ident(&self) -> Option<&Ident> {
        match self {
            ArgName::Ident(ident) => Some(ident),
            ArgName::Position(_) => None,
        }
    }
}

#[derive(Debug)]
/// Helper struct to hold a value and the ident of its property.
pub struct Argument<T> {
    /// The value.
    pub value: T,
    /// The value's span.
    ///
    /// If the value was a flag, this should be the span of the name.
    pub value_span: Range<Span>,
    /// The argument name or span.
    pub name: Option<Ident>,
}

impl<P> Argument<P> {
    pub fn into_value<T>(self) -> Result<T>
    where
        T: FromPartial<P>,
    {
        into(self.value)
    }
}

trait FromAttr: Sized + FromPartial<Self::Partial> {
    type Partial;
    type Span: SpanRanged;
    fn parse_value(input: ParseStream) -> Result<(Self::Partial, Self::Span)>;

    fn support_grouped() -> bool {
        true
    }

    fn support_key_value() -> bool {
        true
    }

    /// Used to decide whether to parse optional positional values.
    ///
    /// Panics when `Self` cannot be peeked.
    ///
    /// # Implementation notes
    /// This should not mutate `input`, if you cannot decide if `input` matches
    /// using [`ParseStream::peek`] ([peek2](ParseStream::peek2),
    /// [peek3](ParseStream::peek3)), fallback to [`ParseStream::fork`], but
    /// also consider not implementing `peek` at all for such cases.
    fn peek(#[allow(unused)] input: ParseStream) -> bool {
        panic!("used unpeekable AttributeValue as optional Positional");
    }

    fn parse_in_paren(
        input: ParseStream,
        _paren_span: Span,
    ) -> Result<(Self::Partial, Self::Span)> {
        Self::parse_value(input)
    }

    fn from_option(value: Option<Self>, error: &str) -> Result<Self> {
        value.ok_or_else(|| error_message!("{error}").into())
    }

    fn from_argument(value: Option<Argument<Self::Partial>>, error: &str) -> Result<Self> {
        Self::from_option(value.map(Argument::into_value).transpose()?, error)
    }

    fn parse_attr(
        name: Option<&'static str>,
        input: ParseStream,
    ) -> Result<Option<Argument<Self::Partial>>> {
        if let Some(name) = name {
            let Some(ident) = parse_name(input, name) else {
                return Ok(None);
            };
            let (value, span) = if Self::support_key_value() && input.parse::<Token![=]>().is_ok() {
                Self::parse_value(input)?
            } else if Self::support_grouped() && input.peek(Paren) {
                let content;
                let paren = parenthesized!(content in input);
                Self::parse_in_paren(&content, paren.span.join())?
            } else if Self::support_key_value() && Self::support_key_value() {
                return Err(input.error("expected `=` or `(`"));
            } else if Self::support_grouped() {
                return Err(input.error("expected `=`"));
            } else {
                return Err(input.error("expected `(`"));
            };
            Ok(Some(Argument {
                name: Some(ident),
                value,
                value_span: span.span_range(),
            }))
        } else {
            let (value, span) = Self::parse_value(input)?;
            Ok(Some(Argument {
                value,
                value_span: span.span_range(),
                name: None,
            }))
        }
    }

    /// Parses the macro input of, e.g. an attribute proc macro.
    fn from_input(input: impl Into<TokenStream>) -> Result<Self> {
        (Self::parse_value)
            .parse2(input.into())
            .and_then(|(v, _)| Self::from(v))
    }
}

/// Parses the name, if it matches returns `Some(name)` and removes the `name`
/// from input, if `None` it does not modify the input.
pub fn parse_name(input: ParseStream, name: &str) -> Option<Ident> {
    let fork = &input.fork();
    let ident: Ident = Ident::parse_any(fork).ok()?;
    if ident == name {
        input.advance_to(fork);
        Some(ident)
    } else {
        None
    }
}

pub trait FromPartial<T>: Sized {
    fn from(partial: T) -> Result<Self>;
}

impl<T> FromPartial<T> for T {
    fn from(partial: T) -> Result<Self> {
        Ok(partial)
    }
}

pub fn into<P, T: FromPartial<P>>(partial: P) -> Result<T> {
    T::from(partial)
}

impl FromAttr for bool {
    type Partial = bool;
    type Span = Span;

    fn from_option(value: Option<Self>, _error: &str) -> Result<Self> {
        Ok(value.unwrap_or_default())
    }

    fn parse_value(input: ParseStream) -> Result<(bool, Span)> {
        LitBool::parse(input).map(|v| (v.value, v.span))
    }

    fn parse_attr(
        name: Option<&'static str>,
        input: ParseStream,
    ) -> Result<Option<Argument<bool>>> {
        if let Some(name) = name {
            let Some(ident) = parse_name(input, name) else {
                return Ok(None);
            };
            let (value, span) = if input.parse::<Token![=]>().is_ok() {
                Self::parse_value(input)?
            } else if input.peek(Paren) {
                let content;
                parenthesized!(content in input);
                Self::parse_value(&content)?
            } else {
                (true, ident.span())
            };
            Ok(Some(Argument {
                value,
                value_span: span.span_range(),
                name: Some(ident),
            }))
        } else {
            let (value, span) = Self::parse_value(input)?;
            Ok(Some(Argument {
                value,
                value_span: span.span_range(),
                name: None,
            }))
        }
    }
}

impl FromAttr for usize {
    type Partial = Self;
    type Span = Span;

    fn parse_value(input: ParseStream) -> Result<(Self::Partial, Span)> {
        LitInt::parse(input).and_then(|i| Ok((i.base10_parse()?, i.span())))
    }

    fn peek(input: ParseStream) -> bool {
        input.peek(LitInt)
    }
}

impl FromAttr for String {
    type Partial = Self;
    type Span = Span;

    fn parse_value(input: ParseStream) -> Result<(Self::Partial, Self::Span)> {
        <LitStr as Parse>::parse(input).map(|s| (s.value(), s.span()))
    }
}
pub fn with_spanning<T: Clone>(ast: T) -> (T, Spanning<T>) {
    (ast.clone(), Spanning(ast))
}
pub fn with_span<T: Clone + Spanned>(ast: T) -> (T, Span) {
    (ast.clone(), ast.span())
}

impl FromAttr for Expr {
    type Partial = Self;
    type Span = Spanning<Self>;

    fn parse_value(input: ParseStream) -> Result<(Self::Partial, Self::Span)> {
        input.parse().map(with_spanning)
    }
}

impl FromAttr for LitStr {
    type Partial = Self;
    type Span = Span;

    fn parse_value(input: ParseStream) -> Result<(Self::Partial, Self::Span)> {
        input.parse().map(with_span)
    }
}

struct Partial<T>(T);

impl<T> Partial<T> {
    fn with_span<S>((value, span): (T, S)) -> (Partial<T>, S) {
        (Partial(value), span)
    }
}

impl<T: FromPartial<P>, P> FromPartial<Partial<Option<P>>> for Option<T> {
    fn from(partial: Partial<Option<P>>) -> Result<Self> {
        partial.0.map(T::from).transpose()
    }
}

impl<T: FromAttr> FromAttr for Option<T> {
    type Partial = Partial<Option<T::Partial>>;
    type Span = T::Span;

    fn parse_value(input: ParseStream) -> Result<(Self::Partial, Self::Span)> {
        T::parse_value(input)
            .map(|(v, s)| (Some(v), s))
            .map(Partial::with_span)
    }

    fn parse_attr(
        name: Option<&'static str>,
        input: ParseStream,
    ) -> Result<Option<Argument<Self::Partial>>> {
        if let Some(name) = name {
            let Some(ident) = parse_name(input, name) else {
                return Ok(None);
            };
            let (value, span) = if Self::support_key_value() && input.parse::<Token![=]>().is_ok() {
                Self::parse_value(input)?
            } else if Self::support_grouped() && input.peek(Paren) {
                let content;
                parenthesized!(content in input);
                Self::parse_value(&content)?
            } else if Self::support_key_value() && Self::support_key_value() {
                return Err(input.error("expected `=` or `(`"));
            } else if Self::support_grouped() {
                return Err(input.error("expected `=`"));
            } else {
                return Err(input.error("expected `(`"));
            };
            Ok(Some(Argument {
                name: Some(ident),
                value,
                value_span: span.span_range(),
            }))
        } else if T::peek(input) {
            let (value, span) = Self::parse_value(input)?;
            Ok(Some(Argument {
                value,
                value_span: span.span_range(),
                name: None,
            }))
        } else {
            Ok(None)
        }
    }

    fn from_option(value: Option<Self>, _error: &str) -> Result<Self> {
        Ok(value.unwrap_or_default())
    }
}

impl<T: FromPartial<P>, P> FromPartial<Partial<Vec<P>>> for Vec<T> {
    fn from(partial: Partial<Vec<P>>) -> Result<Self> {
        partial.0.into_iter().map(T::from).collect()
    }
}

impl<T: FromAttr> FromAttr for Vec<T> {
    type Partial = Partial<Vec<T::Partial>>;
    type Span = Span;

    fn parse_value(input: ParseStream) -> Result<(Self::Partial, Self::Span)> {
        let content;
        if input.peek(Paren) {
            let p = parenthesized!(content in input);
            Self::parse_in_paren(&content, p.span.join())
        } else if input.peek(Bracket) {
            let b = bracketed!(content in input);
            Self::parse_in_paren(&content, b.span.join())
        } else {
            dbg!(input);
            Err(input.error("expected `[` or `(`"))
        }
    }

    fn parse_in_paren(input: ParseStream, paren_span: Span) -> Result<(Self::Partial, Self::Span)> {
        let i = input.parse_terminated(T::parse_value, Token!(,))?;
        Ok((Partial(i.into_iter().map(|(v, _)| v).collect()), paren_span))
    }
}

struct Attribute {
    // #[attr(positional)]
    positional: Expr,
    // #[attr(positional)]
    optional_positional: Option<usize>,
    flag: bool,
    default: Option<LitStr>,
    non_parse: String,
    list: Vec<String>,
    // #[attr(nested)]
    nested: SubAttribute,
}

macro_rules! partial_field {
    ($partial:ident. $field:ident) => {
        $crate::FromAttr::from_argument(
            $partial.$field,
            ::core::concat!("mandatory `", stringify!($field), "` was not specified"),
        )?
    };
}
macro_rules! partial_fields {
    (Self { $($field:ident),* $(,)? } = $partial:ident) => {
        Ok(Self {
            $($field: partial_field!($partial.$field)),*
        })
    };
}

macro_rules! parse_positional {
    ($partial:ident. $field:ident : $type:ty = $input:expr, $span:expr) => {
        if let Some($field) = <$type as $crate::FromAttr>::parse_attr(None, $input)? {
            $partial.$field = Some($field);
            comma!($input, $partial, $span)
        }
    };
}

macro_rules! parse_named {
    ($partial:ident. $field:ident : $type:ty = $input:expr, $span:expr) => {
        if let Some($field) =
            <$type as $crate::FromAttr>::parse_attr(Some(stringify!($field)), $input)?
        {
            $partial.$field = Some($field);
            comma!($input, $partial, $span);
            continue;
        }
    };
}

macro_rules! comma {
    ($input:expr, $partial:expr, $span:expr) => {
        if $input.is_empty() {
            return Ok(($partial, $span));
        } else {
           <Token![,]>::parse($input)?;
        }
    };
}

#[allow(clippy::field_reassign_with_default)]
const _: () = {
    #[derive(Default)]
    struct PartialAttribute {
        positional: Partial![Expr],
        optional_positional: Partial![Option<usize>],
        flag: Partial![bool],
        default: Partial![Option<LitStr>],
        non_parse: Partial![String],
        list: Partial![Vec<String>],
        nested: Partial![SubAttribute],
    }

    impl FromAttr for Attribute {
        type Partial = PartialAttribute;
        type Span = Span;

        fn parse_value(input: ParseStream) -> Result<(Self::Partial, Self::Span)> {
            let content;
            let paren = parenthesized!(content in input);
            Self::parse_in_paren(&content, paren.span.join())
        }

        fn parse_in_paren(input: ParseStream, span: Span) -> Result<(Self::Partial, Self::Span)> {
            let mut partial = PartialAttribute::default();

            // positionals
            parse_positional!(partial.positional: Expr = input, span);
            parse_positional!(partial.optional_positional: Option<usize> = input, span);
            while !input.is_empty() {
                parse_named!(partial.flag: bool = input, span);
                parse_named!(partial.default: Option<LitStr> = input, span);
                parse_named!(partial.non_parse: String = input, span);
                parse_named!(partial.list: Vec<String> = input, span);
                parse_named!(partial.nested: SubAttribute = input, span);
                return Err(
                    input.error("expected `flag`, `default`, `non_parse`, `list` or `nested`")
                );
            }
            Ok((partial, span))
        }

        fn support_key_value() -> bool {
            false
        }

        fn from_input(input: impl Into<TokenStream>) -> Result<Self> {
            (|input: ParseStream| Self::parse_in_paren(input, Span::call_site()))
                .parse2(input.into())
                .and_then(|(v, _)| FromPartial::from(v))
        }
    }

    impl FromPartial<PartialAttribute> for Attribute {
        fn from(partial: PartialAttribute) -> Result<Self> {
            partial_fields!(
                Self {
                    positional,
                    optional_positional,
                    flag,
                    default,
                    non_parse,
                    list,
                    nested,
                } = partial
            )
        }
    }
};

struct SubAttribute {
    name: String,
}
const _: () = {
    #[derive(Default)]
    struct PartialSubAttribute {
        name: Partial![String],
    }

    impl FromAttr for SubAttribute {
        type Partial = PartialSubAttribute;
        type Span = Span;

        fn parse_value(input: ParseStream) -> Result<(Self::Partial, Self::Span)> {
            let content;
            let paren = parenthesized!(content in input);
            Self::parse_in_paren(&content, paren.span.join())
        }

        fn support_key_value() -> bool {
            false
        }

        fn parse_in_paren(input: ParseStream, span: Span) -> Result<(Self::Partial, Self::Span)> {
            let mut partial = PartialSubAttribute::default();
            while !input.is_empty() {
                parse_named!(partial.name: String = input, span);
                return Err(input.error("expected `name`"));
            }
            Ok((partial, span))
        }

        fn from_input(input: impl Into<TokenStream>) -> Result<Self> {
            (|input: ParseStream| Self::parse_in_paren(input, Span::call_site()))
                .parse2(input.into())
                .and_then(|(v, _)| FromPartial::from(v))
        }
    }

    impl FromPartial<PartialSubAttribute> for SubAttribute {
        fn from(partial: PartialSubAttribute) -> Result<Self> {
            partial_fields!(Self { name } = partial)
        }
    }
};

#[test]
fn redesign() {
    let input = quote!(
        1 + 2,
        non_parse = "hello",
        list("hello", "world",),
        nested(name = "!")
    );
    Attribute::from_input(input).unwrap();
}

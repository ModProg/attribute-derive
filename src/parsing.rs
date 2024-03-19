//! This module defines the traits defining how parsing works.
//!
//! `attribute-derive` reuses the same traits for nested values and the root
//! attribute, this is why all traits in this module are named `Attribute*`.
#![doc = include_str!("../docs/traits.html")]

use std::ops::Range;

use manyhow::{span_range, SpanRanged};
use proc_macro2::{Ident, Span};
use quote::ToTokens;
use syn::ext::IdentExt;
use syn::parse::discouraged::Speculative;
#[cfg(doc)]
use syn::parse::ParseBuffer;
use syn::parse::{Parse, ParseStream};
use syn::token::Paren;
use syn::{parenthesized, Result, Token};

use crate::from_partial::FromPartial;
use crate::FromAttr;

/// Values that can be parsed named, e.g. `<name>(<value>)`, `<name> = <value>`,
/// `<name>` (as flag).
///
/// This is the default parsing mode used for fields in derived [`FromAttr`]
/// implementations.
pub trait AttributeNamed: AttributeBase {
    /// What open delimiter to use when providing error messages.
    ///
    /// For `<name> = <value>`, this is `" = "`, for `<function>(<like>)`, it is
    /// `"("`.
    ///
    /// As named attributes can allow both `<name> = <value>` and
    /// `name(<value>)`, this might not be the only way this attribute can be
    /// used.
    const PREFERRED_OPEN_DELIMITER: &'static str = " = ";
    /// What close delimiter to use when providing error messages.
    ///
    /// For `<name> = <value>`, this is `""`, for `<function>(<like>)`, it is
    /// `")"`.
    ///
    /// As named attributes can allow both `<name> = <value>` and
    /// `<name>(<value>)`, this might not be the only way this attribute can be
    /// used.
    const PREFERRED_CLOSE_DELIMITER: &'static str = "";

    /// Parses an attribute containing `Self` called `name`.
    ///
    /// While this function can be implemented freely, the provided
    /// implementations support `<name> = <value>`, `<function>(<like>)` and
    /// `<flag>`.
    ///
    /// **Note:** This needs to stop parsing at the end of the value, before a
    /// possible following `,` and further arguments.
    fn parse_named(
        name: &'static str,
        input: ParseStream,
    ) -> Result<Option<Named<SpannedValue<Self::Partial>>>>;
}

/// Values that can be parsed positionally, i.e., without a name, e.g.
/// `"literal"`, `a + b`, `true`.
///
/// When deriving [`FromAttr`] this behavior is enabled via putting
/// `#[attr(positional)]` on the field.
///
/// The trait is implemented for each [`AttributeValue`] that implements the
/// marker trait [`PositionalValue`].
pub trait AttributePositional: AttributeBase {
    /// Parses `Self`, positionally.
    ///
    /// **Note:** This needs to stop parsing at the end of the value, before a
    /// possible following `,` and further arguments.
    fn parse_positional(input: ParseStream) -> Result<Option<SpannedValue<Self::Partial>>>;
}

/// Any values that can be parsed in an attribute input.
///
/// This is probably the trait you want to implement when you created a custom
/// type for field inside [`#[derive(FromAttr)]`](FromAttr), as it will provide
/// implementations for [`FromAttr`], [`AttributeNamed`] and, if you implement
/// the marker trait [`PositionalValue`], [`AttributePositional`] as well.
///
/// For named attributes by default it will support both `<name> = <value>` and
/// `<function>(<like>)`, though this can be tweaked in the implementation.
pub trait AttributeValue: AttributeBase {
    /// Printed when not encountering a `(` or `=` respectively while trying to
    /// parse a [`AttributeNamed`].
    const EXPECTED: &'static str = "expected `=` or `(`";
    /// What open delimiter to use when providing error messages.
    ///
    /// For `<name> = <value>`, this is `" = "`, for `<function>(<like>)`, it is
    /// `"("`.
    ///
    /// As named attributes can allow both `<name> = <value>` and
    /// `name(<value>)`, this might not be the only way this attribute can be
    /// used.
    const PREFERRED_OPEN_DELIMITER: &'static str = " = ";
    /// What close delimiter to use when providing error messages.
    ///
    /// For `<name> = <value>`, this is `""`, for `<function>(<like>)`, it is
    /// `")"`.
    ///
    /// As named attributes can allow both `<name> = <value>` and
    /// `<name>(<value>)`, this might not be the only way this attribute can be
    /// used.
    const PREFERRED_CLOSE_DELIMITER: &'static str = "";
    /// Parses the attribute value when parentheses (`(`) were peeked.
    ///
    /// Note: this is the input with the parentheses, and potentially following
    /// arguments.
    ///
    /// ```text
    /// attribute(value), ...
    ///          ^^^^^^^^^^^^
    /// ```
    ///
    /// In the default implementation this calls through to
    /// [`parse_value`](Self::parse_value) after removing the parentheses.
    fn parse_value_meta(input: ParseStream) -> Result<SpannedValue<Self::Partial>> {
        let content;
        let paren = parenthesized!(content in input);
        Self::parse_value(&content)
            .map(SpannedValue::value)
            .map(SpannedValue::with(paren.span.join()))
    }

    /// Parses the attribute value when an equals (`=`) was peeked.
    ///
    /// Note: this is the input with the equals, and potentially following
    /// arguments.
    ///
    /// ```text
    /// attribute = value, ...
    ///           ^^^^^^^^^^^^
    /// ```
    ///
    /// In the default implementation this calls through to
    /// [`parse_value`](Self::parse_value) after removing the `=`.
    fn parse_value_eq(input: ParseStream) -> Result<SpannedValue<Self::Partial>> {
        <Token![=]>::parse(input)?;
        Self::parse_value(input)
    }

    /// Parses the plain attribute value without leading `=` or enclosing
    /// parenthesis.
    ///
    /// **Note:** this input includes potentially a trailing `,` and following
    /// arguments.
    ///
    /// ```text
    /// attribute = value, ...
    ///             ^^^^^^^^^^
    /// ```
    ///
    /// For simple syntax this is the only function needed to implement, as the
    /// default implementations for [`parse_value_meta`](Self::parse_value_meta)
    /// and [`parse_value_eq`](Self::parse_value_eq).
    fn parse_value(input: ParseStream) -> Result<SpannedValue<Self::Partial>>;
}

impl<T: AttributeValue> FromAttr for T {
    fn parse_partial(input: ParseStream) -> Result<Self::Partial> {
        Self::parse_value(input).map(SpannedValue::value)
    }
}

impl<T: AttributeValue> AttributeNamed for T {
    const PREFERRED_CLOSE_DELIMITER: &'static str = Self::PREFERRED_CLOSE_DELIMITER;
    const PREFERRED_OPEN_DELIMITER: &'static str = Self::PREFERRED_OPEN_DELIMITER;

    fn parse_named(
        name: &'static str,
        input: ParseStream,
    ) -> Result<Option<Named<SpannedValue<Self::Partial>>>> {
        let Some(name) = parse_name(input, name) else {
            return Ok(None);
        };
        let value = if input.peek(Token![=]) {
            Self::parse_value_eq(input)?
        } else if input.peek(Paren) {
            Self::parse_value_meta(input)?
        } else {
            return Err(input.error(Self::EXPECTED));
        };
        Ok(Some(Named { name, value }))
    }
}

/// Marker trait that enables the blanket implementation of
/// [`AttributePositional`] for [`AttributeValue`].
pub trait PositionalValue {}

impl<T: AttributeValue + PositionalValue> AttributePositional for T {
    fn parse_positional(input: ParseStream) -> Result<Option<SpannedValue<Self::Partial>>> {
        Self::parse_value(input).map(Some)
    }
}

/// Trait implementing parsing for `<function>(<like>)` attributes.
///
/// This is the trait defining the parsing of both top level attributes deriving
/// [`FromAttr`] and sub attributes.
/// ```
/// # quote::quote!(
/// #[attribute(sub_attribute("hello", "world"))]
/// # );  
/// ```
pub trait AttributeMeta: AttributeBase {
    /// Parses the content of the parenthesis:
    ///
    /// ```text
    /// attribute(value)       
    ///           ^^^^^
    /// ```
    fn parse_inner(input: ParseStream) -> Result<Self::Partial>;
}

impl<T: AttributeMeta> AttributeValue for T {
    const EXPECTED: &'static str = "expected `(`";
    const PREFERRED_CLOSE_DELIMITER: &'static str = ")";
    const PREFERRED_OPEN_DELIMITER: &'static str = "(";

    fn parse_value_eq(input: ParseStream) -> Result<SpannedValue<Self::Partial>> {
        Err(input.error(Self::EXPECTED))
    }

    fn parse_value(input: ParseStream) -> Result<SpannedValue<Self::Partial>> {
        Self::parse_inner(input).map(SpannedValue::call_site)
    }
}

/// Trait implemented for attributes that can be parsed optionally as a
/// positional argument, and the requirement for `Option<T>` to implement
/// [`AttributePositional`].
pub trait AttributePeekable {
    /// Used to decide whether to parse optional positional values.
    ///
    /// While most implementations should not mutate `input`, it might be good
    /// to call this on a [`fork`](ParseBuffer::fork) of the original
    /// [`ParseStream`] to ensure no mutation is persisted.
    ///
    /// # Implementation notes
    /// This should not try to parse `input`, if you cannot decide if `input`
    /// matches using [`ParseBuffer::peek`] ([peek2](ParseBuffer::peek2),
    /// [peek3](ParseBuffer::peek3)), consider not implementing
    /// [`AttributePeekable`].
    ///
    /// `attribute-derive` will always [`fork`](ParseBuffer::fork) before
    /// calling this function to allow `peek` to modify `input` without
    /// effecting further parsing.
    fn peek(input: ParseStream) -> bool;
}

#[derive(Debug)]
/// Helper struct to hold a value and the ident of its property.
pub struct Named<T> {
    /// The value.
    pub value: T,
    /// The argument name.
    pub name: Ident,
}

impl<T> Named<T> {
    #[doc(hidden)]
    pub fn error_span(&self) -> Span {
        self.name.span()
    }
}

impl<T> Named<SpannedValue<T>> {
    /// The value.
    pub fn value(self) -> T {
        self.value.value
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

/// Utility crate holding `Self::Partial` used in most attribute traits, i.e.,
/// [`FromAttr`], [`AttributeValue`], [`AttributePositional`], ...
pub trait AttributeBase: FromPartial<Self::Partial> {
    /// Partial type for this attribute. In most cases this can be `Self`,
    /// unless the attribute can be parsed in multiple on-its-own-incomplete
    /// parts or needs special handling on the conversion.
    type Partial;
}

/// Helper struct to hold a value and the corresponding range.
#[derive(Debug)]
pub struct SpannedValue<T> {
    /// The value.
    pub value: T,
    /// The value's span.
    pub span: Range<Span>,
}

impl<T: Default> Default for SpannedValue<T> {
    fn default() -> Self {
        Self::call_site(Default::default())
    }
}

impl<T> SpannedValue<T> {
    /// The value.
    pub fn value(self) -> T {
        self.value
    }

    /// The value's span.
    pub fn span(&self) -> Range<Span> {
        self.span.clone()
    }

    /// Map the value to a new type, keeping the span.
    pub fn map_value<I>(self, map: impl FnOnce(T) -> I) -> SpannedValue<I> {
        SpannedValue {
            span: self.span(),
            value: map(self.value()),
        }
    }

    pub(crate) fn with(span: impl SpanRanged) -> impl Fn(T) -> Self {
        move |value| Self::new(value, span.span_range())
    }

    /// Creates a new `SpannedValue` from a `value` implementing [`ToTokens`].
    pub fn from_to_tokens(value: T) -> Self
    where
        T: ToTokens,
    {
        Self {
            span: span_range!(value),
            value,
        }
    }

    /// Creates a new `SpannedValue` from a `value` and a [`span`](SpanRanged).
    pub fn new(value: T, span: impl SpanRanged) -> SpannedValue<T> {
        Self {
            value,
            span: span.span_range(),
        }
    }

    /// Creates a new `SpannedValue` with the span [`Span::call_site()`].
    pub fn call_site(value: T) -> SpannedValue<T> {
        Self::new(value, Span::call_site())
    }

    #[doc(hidden)]
    pub fn error_span(&self) -> Span {
        self.span()
            .span_joined()
            .unwrap_or_else(|| self.span().start)
    }
}

impl<T> SpanRanged for SpannedValue<T> {
    fn span_range(&self) -> Range<Span> {
        self.span.clone()
    }
}

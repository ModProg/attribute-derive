//! Utilities implementing useful patterns for fields inside an attribute.
use from_partial::{FromPartial, Partial};
use manyhow::SpanRanged;
use parsing::{AttributeNamed, AttributeValue};
use syn::token::Paren;
use syn::Token;

use self::parsing::parse_name;
use crate::parsing::Named;
use crate::{SpannedValue, *};

/// [`FromAttr`] value that can be used both as a flag and with a value.
///
/// When parameter is specified both as flag and as value, the value will
/// dominate.
///
/// ```
/// # use attribute_derive::{FromAttr, utils::FlagOrValue};
/// # use quote::quote;
/// #[derive(FromAttr)]
/// struct Test {
///     param: FlagOrValue<String>,
/// }
///
/// assert_eq!(
///     Test::from_args(quote!(param)).unwrap().param,
///     FlagOrValue::Flag
/// );
/// assert_eq!(
///     Test::from_args(quote!(param = "value")).unwrap().param,
///     FlagOrValue::Value("value".into())
/// );
/// assert_eq!(
///     Test::from_args(quote!(param, param = "value", param))
///         .unwrap()
///         .param,
///     FlagOrValue::Value("value".into())
/// );
/// assert_eq!(Test::from_args(quote!()).unwrap().param, FlagOrValue::None);
/// ```
#[derive(Debug, Default, PartialEq, Eq, PartialOrd, Ord)]
pub enum FlagOrValue<T> {
    /// Was not specified.
    #[default]
    None,
    /// Was specified as a flag, i.e., without a value.
    Flag,
    /// Was specified with a value.
    Value(T),
}

impl<T> FlagOrValue<T> {
    /// Was not specified.
    pub fn is_none(&self) -> bool {
        matches!(self, Self::None)
    }

    /// Was specified as a flag, i.e., without a value.
    pub fn is_flag(&self) -> bool {
        matches!(self, Self::Flag,)
    }

    /// Was specified with a value.
    pub fn is_value(&self) -> bool {
        matches!(self, Self::Value(_),)
    }

    /// Returns value if set.
    pub fn into_value(self) -> Option<T> {
        match self {
            FlagOrValue::Value(value) => Some(value),
            _ => None,
        }
    }

    /// Returns value if set.
    pub fn as_value(&self) -> Option<&T> {
        match self {
            FlagOrValue::Value(value) => Some(value),
            _ => None,
        }
    }

    /// Maps the `value` if present.
    pub fn map_value<I>(self, map: impl FnOnce(T) -> I) -> FlagOrValue<I> {
        match self {
            FlagOrValue::None => FlagOrValue::None,
            FlagOrValue::Flag => FlagOrValue::Flag,
            FlagOrValue::Value(value) => FlagOrValue::Value(map(value)),
        }
    }
}

/// Enables the `transpose` function on [`FlagOrValue`] containing or being
/// contained in [`Option`] or [`Result`](std::result::Result).
pub trait Transpose<T> {
    /// Should behave equivalent to the built-in `transpose` functions available
    /// on [`Result<Option>`](Result::transpose) and
    /// [`Option<Result>`](Option::transpose).
    fn transpose(self) -> T;
}

impl<T> Transpose<Option<FlagOrValue<T>>> for FlagOrValue<Option<T>> {
    fn transpose(self) -> Option<FlagOrValue<T>> {
        match self {
            FlagOrValue::None => Some(FlagOrValue::None),
            FlagOrValue::Flag => Some(FlagOrValue::Flag),
            FlagOrValue::Value(option) => option.map(FlagOrValue::Value),
        }
    }
}

impl<T, E> Transpose<std::result::Result<FlagOrValue<T>, E>>
    for FlagOrValue<std::result::Result<T, E>>
{
    fn transpose(self) -> std::result::Result<FlagOrValue<T>, E> {
        match self {
            FlagOrValue::None => Ok(FlagOrValue::None),
            FlagOrValue::Flag => Ok(FlagOrValue::Flag),
            FlagOrValue::Value(result) => result.map(FlagOrValue::Value),
        }
    }
}

impl<T: FromPartial<P>, P> FromPartial<Partial<FlagOrValue<P>>> for FlagOrValue<T> {
    fn from(partial: Partial<FlagOrValue<P>>) -> syn::Result<Self> {
        partial.0.map_value(T::from).transpose()
    }

    fn from_option(partial: Option<Partial<FlagOrValue<P>>>, _: &str) -> syn::Result<Self> {
        // Pass in surrounding span
        partial
            .map(FromPartial::from)
            .transpose()
            .map(Option::unwrap_or_default)
    }

    fn join(
        first: Option<SpannedValue<Partial<FlagOrValue<P>>>>,
        second: SpannedValue<Partial<FlagOrValue<P>>>,
        specified_twice_error: &str,
    ) -> syn::Result<Option<SpannedValue<Partial<FlagOrValue<P>>>>> {
        match (first, second) {
            (None, this) => Ok(Some(this)),
            (
                Some(value),
                SpannedValue {
                    value: Partial(FlagOrValue::None | FlagOrValue::Flag),
                    ..
                },
            )
            | (
                Some(SpannedValue {
                    value: Partial(FlagOrValue::None | FlagOrValue::Flag),
                    ..
                }),
                value,
            ) => Ok(Some(value)),
            (Some(first), second) => P::join(
                Some(first.map_value(|v| {
                    v.0.into_value()
                        .expect("flag and none are checked in earlier branch")
                })),
                second.map_value(|Partial(v)| {
                    v.into_value()
                        .expect("flag and none are checked in earlier branch")
                }),
                specified_twice_error,
            )
            .map(|o| o.map(|a| a.map_value(FlagOrValue::Value).map_value(Partial))),
        }
    }
}

impl<T: AttributeBase> AttributeBase for FlagOrValue<T> {
    type Partial = Partial<FlagOrValue<T::Partial>>;
}

impl<T: AttributeValue> AttributeNamed for FlagOrValue<T> {
    fn parse_named(
        name: &'static str,
        input: syn::parse::ParseStream,
    ) -> syn::Result<Option<Named<SpannedValue<Self::Partial>>>> {
        let Some(ident) = parse_name(input, name) else {
            return Ok(None);
        };
        let value = if input.peek(Token![=]) {
            T::parse_value_eq(input)?.map_value(FlagOrValue::Value)
        } else if input.peek(Paren) {
            T::parse_value_meta(input)?.map_value(FlagOrValue::Value)
        } else {
            SpannedValue {
                value: FlagOrValue::Flag,
                span: ident.span().span_range(),
            }
        }
        .map_value(Partial);
        Ok(Some(Named { value, name: ident }))
    }
}

impl<T: FromAttr> FromAttr for FlagOrValue<T> {
    fn parse_partial(input: ParseStream) -> Result<Self::Partial> {
        if input.is_empty() {
            Ok(Partial(FlagOrValue::Flag))
        } else {
            T::parse_partial(input).map(FlagOrValue::Value).map(Partial)
        }
    }
}

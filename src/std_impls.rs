use manyhow::{span_range, SpanRanged};
use syn::parse::discouraged::{AnyDelimiter, Speculative};
use syn::parse::{Parse, ParseStream};
use syn::token::{Bracket, Paren};
use syn::{bracketed, parenthesized, LitBool, LitChar, LitFloat, LitInt, LitStr, Result, Token};

use super::FromAttr;
use crate::from_partial::{Defaulting, FromPartial, Partial};
use crate::parsing::{
    parse_name, AttributeBase, AttributeNamed, AttributePeekable, AttributePositional,
    AttributeValue, Named, PositionalValue, SpannedValue,
};

macro_rules! ParseAs {
    ($parse:ty => $($type:ty),+; $value:ident $($tt:tt)*) => {
        ParseAs! {$parse => $($type),+; |$value| $value $($tt)*}
    };
    ($parse:ty => $($type:ty),+; |$value:ident| $conv:expr) => {
        $(impl AttributeBase for $type {
            type Partial = Self;
        }

        impl AttributeValue for $type {
            fn parse_value(input: ParseStream) -> Result<SpannedValue<Self>> {
                let $value: $parse = input.parse()?;
                let span = span_range!($value);
                Ok(SpannedValue::new($conv, span))
            }
        }

        impl PositionalValue for $type {}
    )+
    };
}

ParseAs!(LitStr => String; v.value());
ParseAs!(LitChar => char; v.value());
// TODO: This could be achieved with auto-deref or real specilization
//       ParseAs!(LitByteStr => Vec<u8>; v.value());
ParseAs!(LitInt => u8, i8, u16, i16, u32, i32, u64, i64, u128, i128, usize, isize; v.base10_parse()?);
ParseAs!(LitFloat => f32, f64; v.base10_parse()?);

impl<T: FromPartial<P>, P> FromPartial<Partial<Option<P>>> for Option<T> {
    fn from(partial: Partial<Option<P>>) -> Result<Self> {
        partial.0.map(T::from).transpose()
    }
}

impl<T: AttributeBase> AttributeBase for Option<T> {
    type Partial = Defaulting<Partial<Option<T::Partial>>>;
}

impl<T: AttributePositional + AttributePeekable> AttributePositional for Option<T> {
    fn parse_positional(input: ParseStream) -> Result<Option<SpannedValue<Self::Partial>>> {
        Ok(if T::peek(input) {
            T::parse_positional(input)?.map(|SpannedValue { value, span }| SpannedValue {
                value: Defaulting(Partial(Some(value))),
                span,
            })
        } else {
            None
        })
    }
}

impl<T: AttributeNamed> AttributeNamed for Option<T> {
    fn parse_named(
        name: &'static str,
        input: ParseStream,
    ) -> Result<Option<Named<SpannedValue<Self::Partial>>>> {
        T::parse_named(name, input).map(|value| {
            value.map(|value| Named {
                value: SpannedValue {
                    value: Defaulting(Partial(Some(value.value.value))),
                    span: value.value.span,
                },
                name: value.name,
            })
        })
    }
}

impl<T: FromAttr> FromAttr for Option<T> {
    fn parse_partial(input: ParseStream) -> Result<Self::Partial> {
        T::parse_partial(input)
            .map(Some)
            .map(Partial)
            .map(Defaulting)
    }
}

impl<T: FromPartial<P>, P> FromPartial<Partial<Vec<P>>> for Vec<T> {
    fn from(partial: Partial<Vec<P>>) -> Result<Self> {
        partial.0.into_iter().map(T::from).collect()
    }

    fn join(
        first: Option<SpannedValue<Partial<Vec<P>>>>,
        second: SpannedValue<Partial<Vec<P>>>,
        _: &str,
    ) -> Result<Option<SpannedValue<Partial<Vec<P>>>>> {
        if let Some(mut first) = first {
            first.value.0.extend(second.value.0);
            Ok(Some(first))
        } else {
            Ok(Some(second))
        }
    }
}

impl<T: AttributeBase> AttributeBase for Vec<T> {
    type Partial = Partial<Vec<T::Partial>>;
}

impl<T> AttributePeekable for Vec<T> {
    fn peek(input: ParseStream) -> bool {
        input.peek(Paren) || input.peek(Bracket)
    }
}

impl<T> PositionalValue for Vec<T> {}

fn parse_vec_inner<T: AttributePositional>(
    input: ParseStream,
) -> Result<<Vec<T> as AttributeBase>::Partial> {
    // TODO parse_positional that return Ok(None) will behave wierd
    let i = input.parse_terminated(T::parse_positional, Token!(,))?;
    Ok(Partial(i.into_iter().flatten().map(|v| v.value).collect()))
}

impl<T: AttributePositional> AttributeValue for Vec<T> {
    fn parse_value_meta(input: ParseStream) -> Result<SpannedValue<Self::Partial>> {
        let content;
        let paren = parenthesized!(content in input);
        let fork = &content.fork();
        match parse_vec_inner::<T>(fork) {
            Ok(value) => {
                content.advance_to(fork);
                Ok(SpannedValue {
                    value,
                    span: paren.span.join().span_range(),
                })
            }
            Err(err) => {
                if let Ok((.., span, content)) = content.parse_any_delimiter() {
                    if let Ok(value) = parse_vec_inner::<T>(&content) {
                        return Ok(SpannedValue {
                            value,
                            span: span.join().span_range(),
                        });
                    }
                }
                Err(err)
            }
        }
    }

    fn parse_value(input: ParseStream) -> Result<SpannedValue<Self::Partial>> {
        if input.peek(Paren) {
            Self::parse_value_meta(input)
        } else if input.peek(Bracket) {
            let content;
            let b = bracketed!(content in input);
            let value = parse_vec_inner::<T>(&content)?;
            Ok(SpannedValue {
                value,
                span: b.span.join().span_range(),
            })
        } else {
            dbg!(input);
            Err(input.error("expected `[` or `(`"))
        }
    }
}

impl AttributeBase for bool {
    type Partial = Defaulting<bool>;
}

impl AttributePositional for bool {
    fn parse_positional(input: ParseStream) -> Result<Option<SpannedValue<Self::Partial>>> {
        let lit = LitBool::parse_value(input)?;
        Ok(Some(SpannedValue {
            value: Defaulting(lit.value.value),
            span: lit.span.span_range(),
        }))
    }
}

impl AttributePeekable for bool {
    fn peek(input: ParseStream) -> bool {
        input.peek(LitBool)
    }
}

impl AttributeNamed for bool {
    fn parse_named(
        name: &'static str,
        input: ParseStream,
    ) -> Result<Option<Named<SpannedValue<Self::Partial>>>> {
        let Some(ident) = parse_name(input, name) else {
            return Ok(None);
        };
        let value = if input.parse::<Token![=]>().is_ok() {
            Self::parse_positional(input)?.unwrap()
        } else if input.peek(Paren) {
            let content;
            parenthesized!(content in input);
            Self::parse_positional(&content)?.unwrap()
        } else {
            SpannedValue {
                value: Defaulting(true),
                span: ident.span().span_range(),
            }
        };
        Ok(Some(Named { value, name: ident }))
    }
}

impl crate::FromAttr for bool {
    fn parse_partial(input: ParseStream) -> Result<Self::Partial> {
        if input.is_empty() {
            Ok(Defaulting(true))
        } else {
            Ok(Defaulting(LitBool::parse(input)?.value()))
        }
    }
}

//! Contains utilities for implementing [`FromPartial`].
use crate::*;

/// Converts from a [`Partial`](AttributeBase::Partial) value.
pub trait FromPartial<T>: Sized {
    /// Creates `Self` from `T`.
    ///
    /// # Errors
    /// Returns a [`syn::Error`] when `T` does not represent a valid `Self`,
    /// e.g., due to missing or conflicting fields.
    fn from(partial: T) -> Result<Self>;

    /// Creates `Self` from optional `T`.
    ///
    /// # Errors
    /// The default implementation errors with `error_missing` when `partial` is
    /// [`None`], or when `Self::from` errors.
    ///
    /// Implementors might override this for types with expected default values.
    fn from_option(partial: Option<T>, error_missing: &str) -> Result<Self> {
        // Pass in surrounding span
        partial
            .map(Self::from)
            .transpose()?
            .ok_or_else(|| syn::Error::new(Span::call_site(), error_missing))
    }

    /// Defines how two arguments for the same parameter should be handled.
    ///
    /// # Errors
    /// The default implementation errors if `first` is already present and
    /// `specified_twice_error` is returned with the correct spans.
    fn join(
        first: Option<SpannedValue<T>>,
        second: SpannedValue<T>,
        specified_twice_error: &str,
    ) -> Result<Option<SpannedValue<T>>> {
        if let Some(first) = first {
            if let Some(span) = first
                .span()
                .span_joined()
                .and_then(|s| Some((s, second.span().span_joined()?)))
                .and_then(|(a, b)| a.join(b))
            {
                Err(Error::new(span, specified_twice_error))
            } else {
                let mut error = Error::new(first.span().start, specified_twice_error);
                error.combine(Error::new(second.span().start, specified_twice_error));
                Err(error)
            }
        } else {
            Ok(Some(second))
        }
    }
}

impl<T> FromPartial<T> for T {
    fn from(partial: T) -> Result<Self> {
        Ok(partial)
    }
}

/// [`FromPartial`] wrapper that uses [`Default`] value when not specified.
#[derive(Clone)]
pub struct Defaulting<T>(pub T);

impl<P, T: Default + FromPartial<P>> FromPartial<Defaulting<P>> for T {
    fn from(partial: Defaulting<P>) -> Result<Self> {
        Self::from(partial.0)
    }

    fn from_option(partial: Option<Defaulting<P>>, _error: &str) -> Result<Self> {
        partial
            .map(|d| Self::from(d.0))
            .transpose()
            .map(Option::unwrap_or_default)
    }
}

/// Utility struct to avoid duplicate trait definition when using `Self<A>` for
/// `<Self<B> as BaseAttribute>::Partial`.
pub struct Partial<T>(pub T);

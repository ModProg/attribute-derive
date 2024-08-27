use std::borrow::Cow;
use std::collections::{HashMap, HashSet};
use std::fmt::Display;
use std::iter;
use std::ops::Range;

use collection_literals::hash;
use interpolator::{format, Formattable};
use manyhow::{bail, error_message, manyhow, span_range, ErrorMessage, Result};
use proc_macro2::{Literal, Span, TokenStream};
use proc_macro_utils::{TokenParser, TokenStream2Ext};
use quote::{format_ident, ToTokens};
use quote_use::quote_use as quote;
use syn::spanned::Spanned;
use syn::{DataStruct, DeriveInput, Field, Fields, Generics, Ident, LitStr, Type, Visibility};

const ATTRIBUTE_IDENT: &str = "attribute";

#[allow(clippy::large_enum_variant)]
enum StructError {
    Generic(FormatString),
    Specific(StructErrorSpecific),
}

struct FormatString {
    format: Cow<'static, str>,
    span: Span,
    default: bool,
}

impl<T: Into<Cow<'static, str>>> From<T> for FormatString {
    fn from(value: T) -> Self {
        Self {
            format: value.into(),
            span: Span::call_site(),
            default: true,
        }
    }
}

impl FormatString {
    fn format(&self, values: HashMap<&str, Formattable>) -> Result<LitStr> {
        Ok(LitStr::new(
            &format(&self.format, &values).map_err(|e| error_message!(self.span, "{e}"))?,
            self.span,
        ))
    }

    fn parse(parser: &mut TokenParser) -> Option<Self> {
        Some(Self {
            span: parser.span(),
            format: parser.next_string()?.into(),
            default: false,
        })
    }
}

impl StructError {
    fn duplicate_field(&self) -> &FormatString {
        match self {
            StructError::Generic(g) => g,
            StructError::Specific(s) => &s.duplicate_field,
        }
    }

    fn missing_field(&self) -> &FormatString {
        match self {
            StructError::Generic(g) => g,
            StructError::Specific(s) => &s.missing_field,
        }
    }

    fn field_help(&self) -> Option<&FormatString> {
        if let StructError::Specific(s) = self {
            Some(&s.field_help)
        } else {
            None
        }
    }

    fn conflict(&self) -> &FormatString {
        match self {
            StructError::Generic(g) => g,
            StructError::Specific(s) => &s.conflict,
        }
    }

    fn unknown_field(&self) -> &FormatString {
        match self {
            StructError::Generic(g) => g,
            StructError::Specific(s) => &s.unknown_field,
        }
    }

    fn unknown_field_single(&self) -> &FormatString {
        match self {
            StructError::Generic(g) => g,
            StructError::Specific(s) => &s.unknown_field_single,
        }
    }

    fn unknown_field_empty(&self) -> &FormatString {
        match self {
            StructError::Generic(g) => g,
            StructError::Specific(s) => &s.unknown_field_empty,
        }
    }

    fn unknown_field_error(&self, fields: &[AttrField]) -> Result<LitStr> {
        let fields: Vec<_> = fields
            .iter()
            .filter(|field| !field.positional)
            .map(|field| Formattable::display(&field.ident))
            .collect();
        match fields.len() {
            0 => self.unknown_field_empty().format(HashMap::new()),
            1 => self
                .unknown_field_single()
                .format(hash!("expected_field" => fields[0])),
            _ => self
                .unknown_field()
                .format(hash!("expected_fields" => Formattable::iter(&fields))),
        }
    }
}

struct StructErrorSpecific {
    /// `expected_fields`
    unknown_field: FormatString,
    /// `expected_field`
    unknown_field_single: FormatString,
    unknown_field_empty: FormatString,
    /// `field`
    duplicate_field: FormatString,
    /// `field`
    missing_field: FormatString,
    /// `field`, `attribute`, `example`
    field_help: FormatString,
    /// `first`, `second`
    conflict: FormatString,
}

impl Default for StructErrorSpecific {
    fn default() -> Self {
        Self {
            unknown_field_empty: "expected empty attribute".into(),
            unknown_field_single: "expected supported field `{expected_field}`".into(),
            unknown_field: "supported fields are {expected_fields:i..-1(`{}`)(, )} and \
                            `{expected_fields:i-1}`"
                .into(),
            duplicate_field: "`{field}` is specified multiple times".into(),
            missing_field: "required `{field}` is not specified".into(),
            field_help: "try `#[{attribute}({field}{open_or_eq}{example}{close_or_empty})]`".into(),
            conflict: "`{first}` conflicts with mutually exclusive `{second}`".into(),
        }
    }
}

// enum DuplicateStrategy {
//     AggregateOrError,
//     Error,
//     AggregateOrOverride,
//     Override,
// }

struct StructAttrs {
    ident: Option<Ident>,
    aliases: Vec<String>,
    error: StructError,
    // duplicate: DuplicateStrategy,
}

impl StructAttrs {
    fn from_attrs(
        struct_ident: &Ident,
        attrs: impl IntoIterator<Item = syn::Attribute>,
    ) -> Result<Self> {
        const VALID_FORMAT: &str = r#"expected `#[attribute(ident=attribute_name/!ident, aliases=[alias1, alias2], error="..", error(unknown_field="..", unknown_field_single="..", unknown_field_empty="..", duplicate_field="..", missing_field="..", field_help=".."))]`"#;

        let mut ident_span: Option<Range<Span>> = None;
        let mut ident: Option<Ident> = None;
        let mut aliases: Vec<String> = vec![];
        let mut error = StructError::Specific(Default::default());
        // let mut duplicate: DuplicateStrategy = DuplicateStrategy::AggregateOrError;
        for attr in attrs
            .into_iter()
            .filter(|attr| attr.path().is_ident(ATTRIBUTE_IDENT))
        {
            let parser = &mut attr
                .meta
                .require_list()
                .map_err(|_| ErrorMessage::call_site(VALID_FORMAT))?
                .tokens
                .clone()
                .parser();
            while !parser.is_empty() {
                if let Some(not) = parser.next_tt_not() {
                    if let Some(kw) = parser.next_keyword("ident") {
                        if let Some(ident_span) = ident_span {
                            bail!(
                                error_message!(ident_span, "ident is specified twice")
                                    + error_message!(
                                        not.span()..kw.span(),
                                        "ident was already specified"
                                    )
                            )
                        } else {
                            ident_span = Some(not.span()..kw.span());
                        }
                    }
                } else {
                    let field = parser
                        .next_ident()
                        .ok_or_else(|| ErrorMessage::call_site(VALID_FORMAT))?;
                    match field.to_string().as_str() {
                        "ident" => {
                            parser
                                .next_tt_eq()
                                .ok_or_else(|| ErrorMessage::call_site(VALID_FORMAT))?;
                            ident = Some(
                                parser
                                    .next_ident()
                                    .ok_or_else(|| ErrorMessage::call_site(VALID_FORMAT))?,
                            );
                            if let Some(ident_span) = ident_span {
                                bail!(
                                    error_message!(ident_span, "ident is specified twice")
                                        + error_message!(
                                            field.span()..ident.unwrap().span(),
                                            "ident was already specified"
                                        )
                                )
                            }
                        }
                        "aliases" => {
                            parser
                                .next_tt_eq()
                                .ok_or_else(|| ErrorMessage::call_site(VALID_FORMAT))?;
                            let mut parser = parser
                                .next_bracketed()
                                .ok_or_else(|| ErrorMessage::call_site(VALID_FORMAT))?
                                .stream()
                                .parser();
                            aliases.extend(iter::from_fn(|| {
                                _ = parser.next_tt_comma();
                                parser.next_ident().map(|t| t.to_string())
                            }));
                            if !parser.is_empty() {
                                bail!("{VALID_FORMAT}")
                            }
                        }
                        "error" => {
                            if parser.next_tt_eq().is_some() {
                                error = StructError::Generic(
                                    FormatString::parse(parser)
                                        .ok_or_else(|| ErrorMessage::call_site(VALID_FORMAT))?,
                                );
                            } else {
                                let parser = &mut parser
                                    .next_parenthesized()
                                    .ok_or_else(|| ErrorMessage::call_site(VALID_FORMAT))?
                                    .stream()
                                    .parser();
                                let error = if let StructError::Specific(error) = &mut error {
                                    error
                                } else {
                                    error = StructError::Specific(Default::default());
                                    if let StructError::Specific(error) = &mut error {
                                        error
                                    } else {
                                        unreachable!()
                                    }
                                };
                                while !parser.is_empty() {
                                    let field = parser
                                        .next_ident()
                                        .ok_or_else(|| ErrorMessage::call_site(VALID_FORMAT))?;
                                    let mut string = |f: &mut _| -> Result<()> {
                                        parser
                                            .next_tt_eq()
                                            .ok_or_else(|| ErrorMessage::call_site(VALID_FORMAT))?;
                                        *f = FormatString::parse(parser)
                                            .ok_or_else(|| ErrorMessage::call_site(VALID_FORMAT))?;
                                        Ok(())
                                    };
                                    match field.to_string().as_str() {
                                        "unknown_field" => string(&mut error.unknown_field),
                                        "unknown_field_empty" => {
                                            string(&mut error.unknown_field_empty)
                                        }
                                        "unknown_field_single" => {
                                            string(&mut error.unknown_field_single)
                                        }
                                        "duplicate_field" => string(&mut error.duplicate_field),
                                        "missing_field" => string(&mut error.missing_field),
                                        "field_help" => string(&mut error.field_help),
                                        "conflict" => string(&mut error.conflict),
                                        _ => bail!(field, "{VALID_FORMAT}"),
                                    }?;
                                    _ = parser.next_tt_comma();
                                }
                            }
                        }
                        // "duplicate" => {
                        //     parser.next_eq()                                .ok_or_else(||
                        // ErrorMessage::call_site(VALID_FORMAT))?;     let strategy
                        // = parser.next_ident()                                .ok_or_else(||
                        // ErrorMessage::call_site(VALID_FORMAT))?;     duplicate =
                        // match strategy.to_string().as_str() {
                        //         "AggregateOrError" => DuplicateStrategy::AggregateOrError,
                        //         "Error" => DuplicateStrategy::Error,
                        //         "AggregateOrOverride" => DuplicateStrategy::AggregateOrOverride,
                        //         "Override" => DuplicateStrategy::Override,
                        //         _ => abort!(strategy, VALID_FORMAT),
                        //     }
                        // }
                        _ => bail!(field, "{VALID_FORMAT}"),
                    }
                }
                _ = parser.next_tt_comma();
            }
        }

        if ident_span.is_none() && ident.is_none() {
            let ident_string = struct_ident.to_string();
            let mut out = String::with_capacity(ident_string.len());
            let mut ident_string = ident_string.chars();

            out.extend(ident_string.next().into_iter().flat_map(char::to_lowercase));
            for c in ident_string {
                if c.is_uppercase() {
                    out.push('_');
                    out.extend(c.to_lowercase());
                } else {
                    out.push(c);
                }
            }

            ident = Some(Ident::new(&out, struct_ident.span()));
        }

        Ok(Self {
            ident,
            aliases,
            error,
            // duplicate,
        })
    }
}

struct FieldAttrs {
    optional: bool,
    default: Option<TokenStream>,
    // aggregate: bool,
    conflicts: Vec<Ident>,
    example: Option<String>,
    positional: bool,
}

impl FieldAttrs {
    fn from_attrs(attrs: impl IntoIterator<Item = syn::Attribute>) -> Result<Self> {
        const VALID_FORMAT: &str = r#"expected `#[attribute(optional, positional, default=1+5, conflicts=[a, b], example="22")]`"#;

        let mut optional = false;
        let mut default: Option<TokenStream> = None;
        // let mut aggregate: bool = true;
        let mut conflicts: Vec<Ident> = Vec::new();
        let mut example: Option<String> = None;
        let mut positional = false;

        for attr in attrs
            .into_iter()
            .filter(|attr| attr.path().is_ident(ATTRIBUTE_IDENT))
        {
            let mut parser = attr
                .meta
                .require_list()
                .map_err(|e| error_message!(e.span(), "{VALID_FORMAT}"))?
                .tokens
                .clone()
                .parser();
            while !parser.is_empty() {
                let field = parser
                    .next_ident()
                    .ok_or_else(|| error_message!(parser.next(), "{VALID_FORMAT}"))?;
                match field.to_string().as_str() {
                    "optional" => optional = true,
                    "positional" => positional = true,
                    "default" => {
                        parser
                            .next_tt_eq()
                            .ok_or_else(|| error_message!(parser.next(), "{VALID_FORMAT}"))?;
                        default = Some(
                            parser
                                .next_expression()
                                .ok_or_else(|| error_message!(parser.next(), "{VALID_FORMAT}"))?,
                        );
                        optional = true;
                    }
                    "example" => {
                        parser
                            .next_tt_eq()
                            .ok_or_else(|| error_message!(parser.next(), "{VALID_FORMAT}"))?;
                        // panic!("{:?}", parser.next_string());
                        example = Some(
                            parser
                                .next_string()
                                .ok_or_else(|| error_message!(parser.next(), "{VALID_FORMAT}"))?,
                        );
                    }
                    // "aggregate" => {
                    //     parser.next_eq()                                .ok_or_else(||
                    // error_message!(parser.next(), "{VALID_FORMAT}"))?;     aggregate &=
                    // parser.next_bool()                                .ok_or_else(||
                    // error_message!(parser.next(), "{VALID_FORMAT}"))?; }
                    "conflicts" => {
                        parser
                            .next_tt_eq()
                            .ok_or_else(|| error_message!(parser.next(), "{VALID_FORMAT}"))?;
                        let mut parser = parser
                            .next_bracketed()
                            .ok_or_else(|| error_message!(parser.next(), "{VALID_FORMAT}"))?
                            .stream()
                            .parser();
                        conflicts.extend(iter::from_fn(|| {
                            let ident = parser.next_ident();
                            _ = parser.next_tt_comma();
                            ident
                        }));
                        if !parser.is_empty() {
                            bail!(parser.next(), "{VALID_FORMAT}")
                        }
                    }
                    _ => bail!(field, "{VALID_FORMAT}"),
                }
                _ = parser.next_tt_comma();
            }
        }
        Ok(Self {
            optional,
            default,
            // aggregate,
            conflicts,
            example,
            positional,
        })
    }
}
// TODO generally should use fully qualified names for trait function calls

#[derive(Default)]
struct Conflicts(HashSet<(Ident, Ident)>);
impl Conflicts {
    fn insert(&mut self, a: Ident, b: Ident) {
        if a < b {
            self.0.insert((a, b));
        } else {
            self.0.insert((b, a));
        }
    }

    fn to_tokens(&self, struct_error: &StructError) -> Result<TokenStream> {
        self.0
            .iter()
            .map(|(a, b)| {
                let af = Formattable::display(&a);
                let bf = Formattable::display(&b);

                let error_a_to_b = struct_error
                    .conflict()
                    .format(hash!("first" => af, "second" => bf))?;

                let error_b_to_a = struct_error
                    .conflict()
                    .format(hash!("first" => bf, "second" => af))?;

                Ok(quote! {
                    # use ::attribute_derive::__private::proc_macro2;
                    # use ::attribute_derive::__private::syn::{Error, Spanned};
                    if let (Some(__a), Some(__b)) = (&__partial.#a, &__partial.#b) {
                        if let Some(__joined_span) = __a.error_span().join(__b.error_span()) {
                            return Err(Error::new(__joined_span, #error_a_to_b));
                        } else {
                            let mut __error = Error::new(__a.error_span(), #error_a_to_b);
                            __error.combine(Error::new(__b.error_span(), #error_b_to_a));
                            return Err(__error);
                        }
                    }
                })
            })
            .collect()
    }
}

enum IdentOrIdx {
    Ident(Ident),
    Idx(usize),
}

impl Display for IdentOrIdx {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IdentOrIdx::Ident(i) => write!(f, "{i}"),
            IdentOrIdx::Idx(i) => write!(f, "{i}"),
        }
    }
}

impl ToTokens for IdentOrIdx {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            IdentOrIdx::Ident(i) => i.to_tokens(tokens),
            IdentOrIdx::Idx(i) => Literal::usize_unsuffixed(*i).to_tokens(tokens),
        }
    }
}

struct AttrField {
    ident: IdentOrIdx,
    duplicate: LitStr,
    help: Option<String>,
    missing: String,
    default: Option<TokenStream>,
    ty: Type,
    positional: bool,
}

impl AttrField {
    fn parse_fields(
        input: impl IntoIterator<Item = Field>,
        struct_error: &StructError,
        attribute_ident: Option<Ident>,
    ) -> Result<(Vec<AttrField>, Conflicts)> {
        let mut conflicts = Conflicts::default();
        Ok((
            input
                .into_iter()
                .enumerate()
                .map(
                    |(
                        idx,
                        Field {
                            attrs, ident, ty, ..
                        },
                    )| {
                        let field_name = ident
                            .as_ref()
                            .map_or_else(|| format!("positional_{idx}"), ToString::to_string);

                        let FieldAttrs {
                            optional,
                            default,
                            conflicts: field_conflicts,
                            example,
                            mut positional,
                        } = FieldAttrs::from_attrs(attrs)?;

                        positional |= ident.is_none();

                        for conflict in field_conflicts {
                            if positional {
                                bail!(
                                    ident.map_or_else(
                                        || span_range!(ty),
                                        |ident| ident.span().span_range()
                                    ),
                                    "positional fields do not support conflicts"
                                )
                            }
                            conflicts.insert(
                                ident
                                    .as_ref()
                                    .expect("unnamed fields should be positional")
                                    .clone(),
                                conflict,
                            );
                        }

                        let duplicate = struct_error
                            .duplicate_field()
                            .format(hash!("field" => Formattable::display(&field_name)))?;

                        let help = if let Some(help) = struct_error.field_help() {
                            if attribute_ident.is_none() && help.default {
                                None
                            } else {
                                let example = &example.as_deref().unwrap_or("...");
                                let mut context = hash!(
                                "field" => Formattable::display(&field_name), 
                                "example" => Formattable::display(example), 
                                "open_or_eq" => Formattable::display(&"{open_or_eq}"), 
                                "close_or_empty" => Formattable::display(&"{close_or_empty}"));
                                if let Some(ident) = &attribute_ident {
                                    context.insert("attribute", Formattable::display(ident));
                                }
                                Some(format!("\n\n= help: {}", help.format(context)?.value()))
                            }
                        } else {
                            None
                        };

                        let missing = struct_error
                            .missing_field()
                            .format(hash!("field" => Formattable::display(&field_name)))?
                            .value()
                            + help.as_deref().unwrap_or_default();

                        let default = if let Some(default) = default {
                            Some(default.to_token_stream())
                        } else if optional {
                            Some(quote!(<#ty as Default>::default()))
                        } else {
                            None
                        };

                        Ok(AttrField {
                            positional,
                            ident: ident.map_or(IdentOrIdx::Idx(idx), IdentOrIdx::Ident),
                            duplicate,
                            help,
                            missing,
                            default,
                            ty,
                        })
                    },
                )
                .collect::<Result<_>>()?,
            conflicts,
        ))
    }

    fn map_error(&self, ty: &Type) -> TokenStream {
        self.help
            .as_ref()
            .map(|help| {
                // Precision of `.0` hides the entries but surpresses error when not used in
                // `help` string.
                let fmt = format!("{{__error}}{help}{{open_or_eq:.0}}{{close_or_empty:.0}}");
                let ty = quote!(<#ty as ::attribute_derive::parsing::AttributeNamed>);
                quote! { .map_err(|__error| {
                    ::attribute_derive::__private::syn::Error::new(
                        __error.span(),
                        format!(#fmt,
                            open_or_eq=#ty::PREFERRED_OPEN_DELIMITER,
                            close_or_empty=#ty::PREFERRED_CLOSE_DELIMITER)
                    )
                })}
            })
            .unwrap_or_default()
    }

    fn partial(&self) -> TokenStream {
        let ty = &self.ty;
        if let IdentOrIdx::Ident(ref ident) = self.ident {
            if self.positional {
                quote!(#ident: Option<::attribute_derive::parsing::SpannedValue<
                    <#ty as ::attribute_derive::parsing::AttributeBase>::Partial>>)
            } else {
                quote!(#ident: Option<::attribute_derive::parsing::Named<
                    ::attribute_derive::parsing::SpannedValue<
                    <#ty as ::attribute_derive::parsing::AttributeBase>::Partial>>>)
            }
        } else {
            quote!(Option<::attribute_derive::parsing::SpannedValue<
                    <#ty as ::attribute_derive::parsing::AttributeBase>::Partial>>)
        }
    }

    fn parse_positional(&self) -> Option<TokenStream> {
        let Self {
            ident,
            ty,
            positional,
            ..
        } = self;
        positional.then(|| {
            let parse_comma = parse_comma();
            // TODO let map_error = self.map_error();
            quote! {
                # use attribute_derive::parsing::AttributePositional;
                if let Some(__field) = <#ty as AttributePositional>::parse_positional(__input)? {
                    __partial.#ident = Some(__field);
                    #parse_comma;
                }
            }
        })
    }

    fn parse_named(&self) -> Option<TokenStream> {
        let Self {
            ident,
            ty,
            positional,
            duplicate,
            ..
        } = self;
        (!positional).then(|| {
            let parse_comma = parse_comma();
            let map_error = self.map_error(ty);
            let s_ident = ident.to_string();
            quote! {
                # use attribute_derive::parsing::{AttributeBase, AttributeNamed};
                # use attribute_derive::from_partial::FromPartial;
                if let Some(__field) =
                    <#ty as AttributeNamed>::parse_named(#s_ident, __input) #map_error?
                {
                    let __name = __field.name;
                    __partial.#ident =
                        <#ty as FromPartial<<#ty as AttributeBase>::Partial>>::join(
                            std::mem::take(&mut __partial.#ident).map(|__v| __v.value),
                            __field.value,
                            #duplicate,
                        )?
                        .map(|__v| ::attribute_derive::parsing::Named {
                            name: __name,
                            value: __v,
                        });

                    #parse_comma;
                    continue;
                }
            }
        })
    }

    fn assign_partial(&self) -> TokenStream {
        let Self {
            ident,
            missing,
            default,
            ty,
            ..
        } = self;
        let unwrap = default.as_ref().map_or_else(
            || quote!(?),
            |default| quote!(.unwrap_or_else(|_| #default)),
        );
        let fmt = format!("{missing}{{open_or_eq:.0}}{{close_or_empty:.0}}");
        let attr_named = quote!(<#ty as ::attribute_derive::parsing::AttributeNamed>);
        quote! {
            #ident: <#ty as ::attribute_derive::from_partial::FromPartial<
                <#ty as ::attribute_derive::parsing::AttributeBase>::Partial>>::from_option(
                __partial.#ident.map(|__v| __v.value()),
                &format!(#fmt, open_or_eq=#attr_named::PREFERRED_OPEN_DELIMITER,
                    close_or_empty=#attr_named::PREFERRED_CLOSE_DELIMITER)
            )#unwrap
        }
    }

    fn join_field(&self) -> TokenStream {
        let Self {
            ident,
            ty,
            duplicate,
            ..
        } = self;
        let join = quote! {
                __first.#ident = <#ty as ::attribute_derive::from_partial::FromPartial<
                <#ty as ::attribute_derive::parsing::AttributeBase>::Partial>>::join(
                    __first_value, __second_value, #duplicate)?
        };

        if self.positional {
            quote! {
               if let Some(__second_value) = __second.#ident {
                   let __first_value = ::std::mem::take(&mut __first.#ident);
                   #join;
               }
            }
        } else {
            quote! {
                if let Some(__second) = __second.#ident {
                    let (__first_name, __first_value) = if let Some(__first) =
                        ::std::mem::take(&mut __first.#ident) {
                        (Some(__first.name), Some(__first.value))
                    } else {(None, None)};
                    let __name = __first_name.unwrap_or(__second.name);
                    let __second_value = __second.value;
                    #join.map(|__v| ::attribute_derive::parsing::Named{name: __name, value: __v});
                }

            }
        }
    }
}

fn parse_comma() -> TokenStream {
    quote! {
        if __input.is_empty() {
            return Ok(__partial);
        } else {
            <::attribute_derive::__private::syn::Token![,] as
            ::attribute_derive::__private::syn::parse::Parse>::parse(__input)?;
        }
    }
}

fn partial_attribute(
    partial: &Ident,
    vis: &Visibility,
    fields: &[AttrField],
    generics: &Generics,
) -> Result {
    let Some(first_field) = fields.first() else {
        return Ok(quote!(#[derive(Default)] struct #partial #generics {}));
    };
    let fields = fields.iter().map(AttrField::partial);
    let fields = if matches!(first_field.ident, IdentOrIdx::Ident(_)) {
        quote!({#(#fields),*})
    } else {
        quote!((#(#fields),*);)
    };
    Ok(quote! {
        #[derive(Default)]
        #vis struct #partial #generics #fields
    })
}

#[manyhow(impl_fn)]
#[deprecated = "use `FromAttr` instead"]
#[proc_macro_derive(Attribute, attributes(attribute, attr, from_attr))]
pub fn attribute_derive(input: DeriveInput) -> Result {
    from_attr_derive_impl(input)
}

#[manyhow(impl_fn)]
#[proc_macro_derive(FromAttr, attributes(attribute, attr, from_attr))]
pub fn from_attr_derive(
    DeriveInput {
        attrs,
        vis,
        ident,
        generics,
        data,
        ..
    }: DeriveInput,
) -> Result {
    let partial_ident = &format_ident!("{ident}Partial");

    let StructAttrs {
        ident: attribute_ident,
        mut aliases,
        error: ref struct_error,
        // duplicate,
    } = StructAttrs::from_attrs(&ident, attrs)?;

    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    if let Some(ref attribute_ident) = attribute_ident {
        aliases.insert(0, attribute_ident.to_string());
    }

    let attribute_ident_trait = (!aliases.is_empty())
        .then(|| {
            quote! {
                # use attribute_derive::AttributeIdent;

                impl #impl_generics AttributeIdent for #ident #ty_generics #where_clause {
                    const IDENTS: &'static [&'static str] = &[#(#aliases),*];
                }
            }
        })
        .unwrap_or_default();

    let (fields, conflicts) = match data {
        syn::Data::Struct(DataStruct {
            fields: fields @ (Fields::Named(_) | Fields::Unnamed(_)),
            ..
        }) => AttrField::parse_fields(fields, struct_error, attribute_ident)?,
        _ => bail!("only works on structs with fields"),
    };

    let conflicts = conflicts.to_tokens(struct_error)?;

    let partial_struct = partial_attribute(partial_ident, &vis, &fields, &generics)?;

    let error_invalid_name = struct_error.unknown_field_error(&fields)?;

    let partial_fields = fields.iter().map(AttrField::assign_partial);
    let join_fields = fields.iter().map(AttrField::join_field);

    let from_attr = if fields.len() == 1 && fields[0].positional {
        // newtype struct
        let AttrField { ref ty, .. } = fields[0];
        quote! {
            # use ::attribute_derive::FromAttr;
            # use ::attribute_derive::parsing::SpannedValue;
            # use ::attribute_derive::__private::syn::parse::{ParseStream, Parse};
            # use ::attribute_derive::__private::syn::{Result, Error};
            impl #impl_generics FromAttr for #ident #ty_generics #where_clause {
                fn parse_partial(input: ParseStream) -> Result<Self::Partial> {
                    <#ty as FromAttr>::parse_partial(input)
                        .map(SpannedValue::call_site)
                        .map(Some)
                        .map(#partial_ident)
                }
            }

        }
    } else {
        let parse_positionals: Vec<_> = fields
            .iter()
            .filter_map(AttrField::parse_positional)
            .collect();
        let parse_named: Vec<_> = fields.iter().filter_map(AttrField::parse_named).collect();
        quote! {
            # use ::attribute_derive::parsing::AttributeMeta;
            # use ::attribute_derive::__private::syn::parse::{ParseStream, Parse};
            # use ::attribute_derive::__private::syn::{Result, Error};
            impl #impl_generics AttributeMeta for #ident #ty_generics #where_clause {
               fn parse_inner(__input: ParseStream) -> Result<Self::Partial> {
                   let mut __partial = #partial_ident::default();
                   #(#parse_positionals)*
                   while !__input.is_empty() {
                       #(#parse_named)*
                       return Err(__input.error(#error_invalid_name));
                   }
                   Ok(__partial)
               }
            }
        }
    };
    Ok(quote! {
        # use ::attribute_derive::parsing::{AttributeBase, SpannedValue};
        # use ::attribute_derive::from_partial::FromPartial;
        # use ::attribute_derive::__private::syn::parse::{ParseStream, Parse};
        # use ::attribute_derive::__private::syn::{Result, Error};
        #[allow(clippy::field_reassign_with_default, remove_unnecessary_else)]
        const _: () = {
            #attribute_ident_trait
            #partial_struct

            impl #impl_generics AttributeBase for #ident #ty_generics #where_clause {
                type Partial = #partial_ident #ty_generics;
            }

            #from_attr

            impl #impl_generics Parse for #ident #ty_generics #where_clause {
                fn parse(__input: ParseStream) -> Result<Self> {
                    <Self as ::attribute_derive::FromAttr>::parse_input(__input)
                }
            }

            impl #impl_generics FromPartial<#partial_ident #ty_generics> for #ident #ty_generics
            #where_clause {
                fn from(__partial: #partial_ident #ty_generics) -> Result<Self> {
                    #conflicts
                    Ok(Self {
                        #(#partial_fields),*
                    })
                }

                fn from_option(__partial: Option<#partial_ident #ty_generics>, _: &str)
                -> Result<Self> {
                    <Self as FromPartial<#partial_ident #ty_generics>>::from(
                        __partial.unwrap_or_default())
                }

                fn join(
                    __first: Option<SpannedValue<#partial_ident #ty_generics>>,
                    __second: SpannedValue<#partial_ident #ty_generics>,
                     _: &str)
                  -> Result<Option<SpannedValue<#partial_ident #ty_generics>>> {
                    let mut __first = __first.unwrap_or_default().value;
                    let __span = __second.span;
                    let __second = __second.value;
                    #(#join_fields;)*
                    Ok(Some(SpannedValue { span: __span, value: __first}))
                }
            }
        };
    })
}

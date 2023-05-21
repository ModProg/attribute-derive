use std::borrow::Cow;
use std::collections::HashSet;
use std::iter;

use collection_literals::hash;
use interpolator::{format, Formattable};
use proc_macro2::TokenStream;
use proc_macro_error::{abort, abort_call_site, proc_macro_error, OptionExt};
use proc_macro_utils::TokenStream2Ext;
use quote::{format_ident, ToTokens};
use quote_use::quote_use as quote;
use syn::punctuated::Punctuated;
use syn::{parse_macro_input, DataStruct, DeriveInput, Field, Fields, FieldsNamed, Ident, Token};

const ATTRIBUTE_IDENT: &str = "attribute";

#[allow(clippy::large_enum_variant)]
enum StructError {
    Generic(String),
    Specific(StructErrorSpecific),
}

impl StructError {
    fn duplicate_field(&self) -> &str {
        match self {
            StructError::Generic(g) => g,
            StructError::Specific(s) => &s.duplicate_field,
        }
    }

    fn missing_field(&self) -> &str {
        match self {
            StructError::Generic(g) => g,
            StructError::Specific(s) => &s.missing_field,
        }
    }

    fn field_help(&self) -> Option<&Cow<'static, str>> {
        if let StructError::Specific(s) = self {
            Some(&s.field_help)
        } else {
            None
        }
    }

    fn missing_flag(&self) -> &str {
        match self {
            StructError::Generic(g) => g,
            StructError::Specific(s) => &s.missing_flag,
        }
    }

    fn conflict(&self) -> &str {
        match self {
            StructError::Generic(g) => g,
            StructError::Specific(s) => &s.conflict,
        }
    }

    fn flag_help(&self) -> Option<&Cow<'static, str>> {
        if let StructError::Specific(s) = self {
            Some(&s.flag_help)
        } else {
            None
        }
    }

    fn unknown_field(&self) -> &str {
        match self {
            StructError::Generic(g) => g,
            StructError::Specific(s) => &s.unknown_field,
        }
    }

    fn unknown_field_single(&self) -> &str {
        match self {
            StructError::Generic(g) => g,
            StructError::Specific(s) => &s.unknown_field_single,
        }
    }

    fn unknown_field_empty(&self) -> &str {
        match self {
            StructError::Generic(g) => g,
            StructError::Specific(s) => &s.unknown_field_empty,
        }
    }
}

struct StructErrorSpecific {
    /// `found_field!`, `expected_fields`
    unknown_field: Cow<'static, str>,
    /// `found_field!`, `expected_field`
    unknown_field_single: Cow<'static, str>,
    /// `found_field!`
    unknown_field_empty: Cow<'static, str>,
    /// `field`
    duplicate_field: Cow<'static, str>,
    /// `field`
    missing_field: Cow<'static, str>,
    /// `field`, `attribute`, `example`
    field_help: Cow<'static, str>,
    /// `flag`
    missing_flag: Cow<'static, str>,
    /// `flag`, `attribute`
    flag_help: Cow<'static, str>,
    /// `first`, `second`
    conflict: Cow<'static, str>,
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
            field_help: "try `#[{attribute}({field}={example})]`".into(),
            missing_flag: "required `{flag}` is not specified".into(),
            flag_help: "try `#[{attribute}({flag})]`".into(),
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
    ident: Option<String>,
    aliases: Vec<String>,
    error: StructError,
    // duplicate: DuplicateStrategy,
}

impl StructAttrs {
    fn from_attrs(attrs: impl IntoIterator<Item = syn::Attribute>) -> Self {
        const VALID_FORMAT: &str = r#"expected `#[attribute(ident=attribute_name, aliases=[alias1, alias2], error="..", error(unknown_field="..", unknown_field_single="..", unknown_field_empty="..", duplicate_field="..", missing_field="..", field_help="..", missing_flag"..", flag_help".."))]`"#;
        let mut ident: Option<String> = None;
        let mut aliases: Vec<String> = vec![];
        let mut error = StructError::Specific(Default::default());
        // let mut duplicate: DuplicateStrategy = DuplicateStrategy::AggregateOrError;
        for attr in attrs
            .into_iter()
            .filter(|attr| attr.path().is_ident(ATTRIBUTE_IDENT))
        {
            let mut parser = attr
                .meta
                .require_list()
                .ok()
                .expect_or_abort(VALID_FORMAT)
                .tokens
                .clone()
                .parser();
            while !parser.is_empty() {
                let field = parser.next_ident().expect_or_abort(VALID_FORMAT);
                match field.to_string().as_str() {
                    "ident" => {
                        parser.next_tt_eq().expect_or_abort(VALID_FORMAT);
                        ident = Some(
                            parser
                                .next_ident()
                                .expect_or_abort(VALID_FORMAT)
                                .to_string(),
                        )
                    }
                    "aliases" => {
                        parser.next_tt_eq().expect_or_abort(VALID_FORMAT);
                        let mut parser = parser
                            .next_bracketed()
                            .expect_or_abort(VALID_FORMAT)
                            .stream()
                            .parser();
                        aliases.extend(iter::from_fn(|| {
                            _ = parser.next_tt_comma();
                            parser.next_ident().map(|t| t.to_string())
                        }));
                        if !parser.is_empty() {
                            abort_call_site!(VALID_FORMAT)
                        }
                    }
                    "error" => {
                        if parser.next_tt_eq().is_some() {
                            error = StructError::Generic(
                                parser.next_string().expect_or_abort(VALID_FORMAT),
                            );
                        } else {
                            let mut parser = parser
                                .next_parenthesized()
                                .expect_or_abort(VALID_FORMAT)
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
                                let field = parser.next_ident().expect_or_abort(VALID_FORMAT);
                                let mut string = |f: &mut Cow<str>| {
                                    parser.next_tt_eq().expect_or_abort(VALID_FORMAT);
                                    *f = parser.next_string().expect_or_abort(VALID_FORMAT).into()
                                };
                                match field.to_string().as_str() {
                                    "unknown_field" => string(&mut error.unknown_field),
                                    "unknown_field_empty" => string(&mut error.unknown_field_empty),
                                    "unknown_field_single" => {
                                        string(&mut error.unknown_field_single)
                                    }
                                    "duplicate_field" => string(&mut error.duplicate_field),
                                    "missing_field" => string(&mut error.missing_field),
                                    "field_help" => string(&mut error.field_help),
                                    "missing_flag" => string(&mut error.missing_flag),
                                    "flag_help" => string(&mut error.flag_help),
                                    "conflict" => string(&mut error.conflict),
                                    _ => abort!(field, VALID_FORMAT),
                                }
                                _ = parser.next_tt_comma();
                            }
                        }
                    }
                    // "duplicate" => {
                    //     parser.next_eq().expect_or_abort(VALID_FORMAT);
                    //     let strategy = parser.next_ident().expect_or_abort(VALID_FORMAT);
                    //     duplicate = match strategy.to_string().as_str() {
                    //         "AggregateOrError" => DuplicateStrategy::AggregateOrError,
                    //         "Error" => DuplicateStrategy::Error,
                    //         "AggregateOrOverride" => DuplicateStrategy::AggregateOrOverride,
                    //         "Override" => DuplicateStrategy::Override,
                    //         _ => abort!(strategy, VALID_FORMAT),
                    //     }
                    // }
                    _ => abort!(field, VALID_FORMAT),
                }
                _ = parser.next_tt_comma();
            }
        }
        Self {
            ident,
            aliases,
            error,
            // duplicate,
        }
    }
}

struct FieldAttrs {
    optional: Option<bool>,
    default: Option<TokenStream>,
    // aggregate: bool,
    conflicts: Vec<Ident>,
    example: Option<String>,
}

impl FieldAttrs {
    fn from_attrs(attrs: impl IntoIterator<Item = syn::Attribute>) -> Self {
        const VALID_FORMAT: &str = r#"expected `#[attribute(optional, optional=true, default=1+5, conflicts=[a, b], example="22")]`"#;

        let mut optional: Option<bool> = None;
        let mut default: Option<TokenStream> = None;
        // let mut aggregate: bool = true;
        let mut conflicts: Vec<Ident> = Vec::new();
        let mut example: Option<String> = None;

        for attr in attrs
            .into_iter()
            .filter(|attr| attr.path().is_ident(ATTRIBUTE_IDENT))
        {
            let mut parser = attr
                .meta
                .require_list()
                .ok()
                .expect_or_abort(VALID_FORMAT)
                .tokens
                .clone()
                .parser();
            while !parser.is_empty() {
                let field = parser.next_ident().expect_or_abort(VALID_FORMAT);
                match field.to_string().as_str() {
                    "optional" => {
                        if parser.next_tt_eq().is_some() {
                            optional = Some(parser.next_bool().expect_or_abort(VALID_FORMAT))
                        } else {
                            optional = Some(true)
                        }
                    }
                    "default" => {
                        parser.next_tt_eq().expect_or_abort(VALID_FORMAT);
                        default = Some(parser.next_expression().expect_or_abort(VALID_FORMAT));
                        optional = optional.or(Some(true));
                    }
                    "example" => {
                        parser.next_tt_eq().expect_or_abort(VALID_FORMAT);
                        // panic!("{:?}", parser.next_string());
                        example = Some(parser.next_string().expect_or_abort(VALID_FORMAT));
                    }
                    // "aggregate" => {
                    //     parser.next_eq().expect_or_abort(VALID_FORMAT);
                    //     aggregate &= parser.next_bool().expect_or_abort(VALID_FORMAT);
                    // }
                    "conflicts" => {
                        parser.next_tt_eq().expect_or_abort(VALID_FORMAT);
                        let mut parser = parser
                            .next_bracketed()
                            .expect_or_abort(VALID_FORMAT)
                            .stream()
                            .parser();
                        conflicts.extend(iter::from_fn(|| {
                            _ = parser.next_tt_comma();
                            parser.next_ident()
                        }));
                        if !parser.is_empty() {
                            abort_call_site!(VALID_FORMAT)
                        }
                    }
                    _ => abort!(field, VALID_FORMAT),
                }
                _ = parser.next_tt_comma();
            }
        }
        Self {
            optional,
            default,
            // aggregate,
            conflicts,
            example,
        }
    }
}
// TODO generally should use fully qualified names for trait function calls

#[proc_macro_error]
#[proc_macro_derive(Attribute, attributes(attribute))]
pub fn attribute_derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let DeriveInput {
        attrs,
        ident,
        generics,
        data,
        ..
    } = parse_macro_input!(input as DeriveInput);

    let parser_ident = format_ident!("{ident}__Parser");

    let StructAttrs {
        ident: attribute_ident,
        mut aliases,
        error: struct_error,
        // duplicate,
    } = StructAttrs::from_attrs(attrs);

    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    aliases.extend(attribute_ident.clone());
    let ident_count = aliases.len();

    let attribute_ident_trait = (!aliases.is_empty())
        .then(|| {
            quote! {
                # use attribute_derive::AttributeIdent;

                impl #impl_generics AttributeIdent for #ident #ty_generics #where_clause {
                    type IDENTS = [&'static str; #ident_count];
                    const IDENTS: Self::IDENTS = [#(#aliases),*];
                }
            }
        })
        .unwrap_or_default();

    let mut options_ty: Punctuated<TokenStream, Token!(,)> = Punctuated::new();
    // let mut parsing_positional: Punctuated<TokenStream, Token!(;)> =
    // Punctuated::new();
    let mut parsing: Punctuated<TokenStream, Token!(,)> = Punctuated::new();
    let mut option_assignments: Punctuated<TokenStream, Token!(;)> = Punctuated::new();
    let mut assignments: Punctuated<TokenStream, Token!(,)> = Punctuated::new();
    let mut possible_variables: Vec<Ident> = Vec::new();
    let mut conflicts: HashSet<(Ident, Ident)> = HashSet::new();

    match data {
        syn::Data::Struct(DataStruct {
            fields: Fields::Named(FieldsNamed { named, .. }),
            ..
        }) => {
            for Field {
                attrs, ident, ty, ..
            } in named.into_iter()
            {
                let ident = ident.expect("named struct fields should have idents");
                let ident_str = ident.to_string();

                let FieldAttrs {
                    optional,
                    default,
                    // aggregate,
                    conflicts: field_conflicts,
                    example,
                } = FieldAttrs::from_attrs(attrs);

                for conflict in field_conflicts {
                    if conflict < ident {
                        conflicts.insert((conflict, ident.clone()));
                    } else {
                        conflicts.insert((ident.clone(), conflict));
                    }
                }

                options_ty
                    .push(quote!(#ident: Option<::attribute_derive::IdentValue<<#ty as ::attribute_derive::ConvertParsed>::Type>>));

                let error = format(
                    struct_error.duplicate_field(),
                    &hash!("field" => Formattable::display(&ident)),
                )
                .ok()
                .expect_or_abort("Invalid format string for `duplicate_field`");
                option_assignments.push(quote! {
                    self.#ident = <#ty as ::attribute_derive::ConvertParsed>::aggregate(self.#ident.take(), $other.#ident, #error)?;
                });

                let field_help = if let Some(help) = struct_error.field_help() {
                    if attribute_ident.is_none() && matches!(help, Cow::Borrowed(_)) {
                        None
                    } else {
                        let example = &example.as_deref().unwrap_or("...");
                        let mut context = hash!("field" => Formattable::display(&ident), "example" => Formattable::display(example));
                        if let Some(ident) = &attribute_ident {
                            context.insert("attribute", Formattable::display(ident));
                        }
                        Some(format!(
                            "\n\n= help: {}",
                            format(help, &context)
                                .ok()
                                .expect_or_abort("Invalid format string for `field_help`")
                        ))
                    }
                } else {
                    None
                };

                let error = if let Some(help) = &field_help {
                    quote! {.map_err(|__error| ::attribute_derive::__private::syn::Error::new(__error.span(), ::std::string::ToString::to_string(&__error) + #help))}
                } else {
                    quote!()
                };

                // TODO positional
                // if positional {
                //     parsing_positional.push(quote! {
                //         # use ::attribute_derive::__private::{syn, proc_macro2};
                //         $parser.#ident = Some(syn::parse::Parse::parse($input)#error?);
                //         if $input.is_empty() {
                //             break
                //         }
                //         $input.step(|$cursor| match $cursor.punct() {
                //             Some(($punct, $rest))
                //                 if $punct.as_char() == ',' =>
                //             {
                //                 Ok(((), $rest))
                //             }
                //             _ => Err($cursor.error("expected ,")),
                //         })?;
                //     })
                // } else {
                parsing.push(quote! {
                        # use ::attribute_derive::__private::{syn, proc_macro2};
                        # use ::attribute_derive::IdentValue;
                        #ident_str => {
                            $parser.#ident = Some(IdentValue{
                                value: if let Some(Some(__value)) = $is_flag.then(|| <#ty as ::attribute_derive::ConvertParsed>::as_flag()) {
                                        __value
                                    } else {
                                        $input.step(|__cursor| match __cursor.punct() {
                                            Some((__punct, __rest))
                                                if __punct.as_char() == '=' && __punct.spacing() == proc_macro2::Spacing::Alone =>
                                            {
                                                Ok(((), __rest))
                                            }
                                            _ => Err(__cursor.error("expected assignment `=`")),
                                        })?;
                                        syn::parse::Parse::parse($input)#error?
                                    },
                                ident: $variable
                            });
                        }
                    });
                // }
                let field_error = format(
                    struct_error.missing_field(),
                    &hash!("field" => Formattable::display(&ident)),
                )
                .ok()
                .expect_or_abort("Invalid format string for `missing_field`")
                    + &field_help.unwrap_or_default();

                let flag_help = if let Some(help) = struct_error.flag_help() {
                    if attribute_ident.is_none() && matches!(help, Cow::Borrowed(_)) {
                        None
                    } else {
                        let mut context = hash!("flag" => Formattable::display(&ident));
                        if let Some(ident) = &attribute_ident {
                            context.insert("attribute", Formattable::display(ident));
                        }
                        Some(format!(
                            "\n\n= help: {}",
                            format(help, &context)
                                .ok()
                                .expect_or_abort("Invalid format string for `field_help`")
                        ))
                    }
                } else {
                    None
                };

                let flag_error = format(
                    struct_error.missing_flag(),
                    &hash!("flag" => Formattable::display(&ident)),
                )
                .ok()
                .expect_or_abort("Invalid format string for `flag_error`")
                    + &flag_help.unwrap_or_default();

                let default = if let Some(default) = default {
                    default.to_token_stream()
                } else {
                    quote!(<#ty as Default>::default())
                };

                assignments.push(match optional{
                    Some(true) => {
                        quote! {
                            #ident: $parser.#ident.map(|t| ::attribute_derive::ConvertParsed::convert(t.value)).unwrap_or_else(|| Ok(#default))?
                        }
                    }
                    Some(false) => {
                        quote! {
                            # use ::attribute_derive::__private::{syn, proc_macro2};
                            # use ::attribute_derive::{ConvertParsed};
                            #ident: match $parser.#ident.map(|t| ConvertParsed::convert(t.value)) {
                                    Some(__option) => __option?,
                                    None if <#ty as ConvertParsed>::as_flag().is_some() => Err(syn::Error::new(proc_macro2::Span::call_site(), #flag_error))?,
                                    _ => Err(syn::Error::new(proc_macro2::Span::call_site(), #field_error))?,
                                }
                        }
                    }
                    None => {
                        quote! {
                            # use ::attribute_derive::__private::{syn, proc_macro2};
                            # use ::attribute_derive::{ConvertParsed};
                            #ident: match $parser.#ident.map(|t| ConvertParsed::convert(t.value)) {
                                    Some(__option) => __option?,
                                    None if <#ty as ConvertParsed>::default_by_default() => <#ty as ConvertParsed>::default(),
                                    _ => Err(syn::Error::new(proc_macro2::Span::call_site(), #field_error))?,
                                }
                        }
                    }
                });

                possible_variables.push(ident);
            }
        }
        _ => abort_call_site!("only works on structs with named fields"),
    };

    let verification = conflicts.into_iter().map(|(a, b)| {
        let af = Formattable::display(&a);
        let bf = Formattable::display(&b);
        let error_a_to_b = format(
            struct_error.conflict(),
            &hash!("first" => af, "second" => bf),
        )
        .ok()
        .expect_or_abort("Invalid format string for `conflict`");
        let error_b_to_a = format(
            struct_error.conflict(),
            &hash!("first" => bf, "second" => af),
        )
        .ok()
        .expect_or_abort("Invalid format string for `conflict`");
        quote! {
            # use ::attribute_derive::__private::proc_macro2;
            # use ::attribute_derive::__private::syn::{Error, Spanned};
            if let (Some($a), Some($b)) = (&$parser.#a, &$parser.#b) {
                if let Some($joined_span) = $a.ident.span().join($b.ident.span()) {
                    return Err(Error::new($joined_span, #error_a_to_b));
                } else {
                    let mut $error = Error::new_spanned(&$a.ident, #error_a_to_b);
                    $error.combine(Error::new_spanned(&$b.ident, #error_b_to_a));
                    return Err($error);
                }
            }
        }
    });

    let found_field = Formattable::display(&"{found_field}");
    let error_invalid_name = match possible_variables.len() {
        0 => struct_error.unknown_field_empty().to_owned(),
        1 => format(
            struct_error.unknown_field_single(),
            &hash!("expected_field" => Formattable::display(&possible_variables[0]), "found_field" => found_field),
        )
        .ok()
        .expect_or_abort("Invalid format string for `unknown_field_single"),
        _ => {
            let possible_variables: Vec<_> = possible_variables.iter().map(Formattable::display).collect();
            format(
            struct_error.unknown_field(),
            &hash!("expected_fields" => Formattable::iter(&possible_variables), "found_field" => found_field),
        )
            .ok()
            .expect_or_abort("Invalid format string for `unknown_field_single")},
    };

    quote! {
        # use ::attribute_derive::__private::{syn::{self, Result, Ident, Token, parse::{Parse, ParseStream}}, proc_macro2};
        # use ::attribute_derive::TryExtendOne;

        #[doc(hidden)]
        #[allow(non_camel_case_types)]
        #[derive(Default)]
        struct #parser_ident {
            #options_ty
        }

        #[allow(unreachable_code)]
        impl Parse for #parser_ident {
            fn parse($input: ParseStream<'_>) -> Result<Self> {
                let mut $parser: Self = Default::default();
                // loop {
                //     #parsing_positional
                //     break;
                // }
                loop {
                    if $input.is_empty() {
                        break;
                    }

                    let $variable = Ident::parse($input)?;

                    let $is_flag = !$input.peek(Token!(=));

                    match $variable.to_string().as_str() {
                        #parsing
                        $found_field => {
                            return Err(syn::Error::new(
                                $variable.span(),
                                format!(concat!(#error_invalid_name, "{found_field:.0}"), found_field = $found_field)
                            ))
                        }
                    }

                    if $input.is_empty() {
                        break;
                    }

                    // Parse `,`
                    $input.step(|__cursor| match __cursor.punct() {
                        Some((__punct, __rest)) if __punct.as_char() == ',' => Ok(((), __rest)),
                        _ => Err(__cursor.error("expected end of arg `,`")),
                    })?;
                }
                Ok($parser)
            }
        }

        #[allow(unreachable_code)]
        impl TryExtendOne for #parser_ident {
            fn try_extend_one(&mut self, $other: Self) -> Result<()>{
                #option_assignments
                Ok(())
            }
        }

        #attribute_ident_trait

        #[allow(unreachable_code)]
        impl #impl_generics ::attribute_derive::Attribute for #ident #ty_generics #where_clause {
            type Parser = #parser_ident;

            fn from_parser($parser: Self::Parser) -> Result<Self> {
                #(#verification)*
                Ok(Self{#assignments})
            }
        }

        impl #impl_generics Parse for #ident #ty_generics #where_clause {
            fn parse($input: ParseStream<'_>) -> Result<Self> {
                Parse::parse($input).and_then(Self::from_parser)
            }
        }
    }
    .into()
}

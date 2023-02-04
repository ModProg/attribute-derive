use proc_macro2::TokenStream;
use proc_macro_error::{abort_call_site, proc_macro_error, ResultExt};
use quote::format_ident;
use quote_use::quote_use as quote;
use syn::punctuated::Punctuated;
use syn::{
    parse_macro_input, DataStruct, DeriveInput, Field, Fields, FieldsNamed, Lit, Meta,
    MetaNameValue, NestedMeta, Token,
};

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

    let mut attribute_ident = None;
    let mut invalid_field = None;

    for attribute in attrs
        .into_iter()
        .filter(|attribute| attribute.path.is_ident("attribute"))
    {
        const VALID_FORMAT: &str = r#"Expected `#[attribute(ident="name_of_your_attribute", invalid_field="error message", missing="error messag", conflicts(a, b)")]`"#;
        let meta: Meta = attribute.parse_meta().unwrap_or_abort();
        if let Meta::List(meta) = meta {
            for meta in meta.nested {
                if let NestedMeta::Meta(Meta::NameValue(MetaNameValue { path, lit, .. })) = meta {
                    match (
                        path.get_ident()
                            .unwrap_or_else(|| abort_call_site!(VALID_FORMAT))
                            .to_string()
                            .as_str(),
                        lit,
                    ) {
                        ("ident", Lit::Str(lit)) => attribute_ident = Some(lit.value()),
                        ("invalid_field", Lit::Str(lit)) => invalid_field = Some(lit.value()),
                        _ => abort_call_site!(VALID_FORMAT),
                    }
                } else {
                    abort_call_site!(VALID_FORMAT);
                }
            }
        }
    }

    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let attribute_ident = attribute_ident
        .map(|attribute_ident| {
            quote! {
                # use attribute_derive::AttributeIdent;

                impl #impl_generics AttributeIdent for #ident #ty_generics #where_clause {
                    const ATTRIBUTE_IDENT: &'static str = #attribute_ident;
                }
            }
        })
        .unwrap_or_default();

    let mut options_ty: Punctuated<TokenStream, Token!(,)> = Punctuated::new();
    let mut parsing_positional: Punctuated<TokenStream, Token!(;)> = Punctuated::new();
    let mut parsing: Punctuated<TokenStream, Token!(,)> = Punctuated::new();
    let mut option_assignments: Punctuated<TokenStream, Token!(;)> = Punctuated::new();
    let mut assignments: Punctuated<TokenStream, Token!(,)> = Punctuated::new();
    let mut possible_variables: Vec<String> = Vec::new();

    match data {
        syn::Data::Struct(DataStruct {
            fields: Fields::Named(FieldsNamed { named, .. }),
            ..
        }) => {
            for Field {
                attrs, ident, ty, ..
            } in named.into_iter()
            {
                let mut default = false;
                let mut positional = false;
                let mut missing = None;
                let mut expected = None;
                for attribute in attrs
                    .into_iter()
                    .filter(|attribute| attribute.path.is_ident("attribute"))
                {
                    const VALID_FORMAT: &str = r#"Expected `#[attribute(default, positional, missing="error message", expected="error message"])`"#;
                    let meta: Meta = attribute.parse_meta().unwrap_or_abort();
                    if let Meta::List(meta) = meta {
                        for meta in meta.nested {
                            if let NestedMeta::Meta(meta) = meta {
                                match meta {
                                    Meta::NameValue(MetaNameValue { path, lit, .. }) => match (
                                        path.get_ident()
                                            .unwrap_or_else(|| abort_call_site!(VALID_FORMAT))
                                            .to_string()
                                            .as_str(),
                                        lit,
                                    ) {
                                        ("missing", Lit::Str(lit)) => missing = Some(lit.value()),
                                        ("expected", Lit::Str(lit)) => expected = Some(lit.value()),
                                        _ => abort_call_site!(VALID_FORMAT),
                                    },
                                    Meta::Path(path) => match path
                                        .get_ident()
                                        .unwrap_or_else(|| abort_call_site!(VALID_FORMAT))
                                        .to_string()
                                        .as_str()
                                    {
                                        "default" => default = true,
                                        "positional" => positional = true,
                                        _ => abort_call_site!(VALID_FORMAT),
                                    },
                                    _ => abort_call_site!(VALID_FORMAT),
                                }
                            } else {
                                abort_call_site!(VALID_FORMAT);
                            }
                        }
                    }
                }

                let ident = ident.expect("named struct fields have idents");
                let ident_str = ident.to_string();

                options_ty
                    .push(quote!(#ident: Option<<#ty as ::attribute_derive::ConvertParsed>::Type>));

                let error1 = format!("`{ident}` is specified multiple times");
                let error2 = format!("`{ident}` was already specified");
                option_assignments.push(quote! {
                    self.#ident = <#ty as ::attribute_derive::ConvertParsed>::aggregate(self.#ident.take(), $other.#ident, #error1, #error2)?;
                });

                let error = if let Some(expected) = expected {
                    quote! {.map_err(|__error| ::attribute_derive::__private::syn::Error::new(__error.span(), #expected)) }
                } else {
                    quote!()
                };

                if positional {
                    parsing_positional.push(quote! {
                        # use ::attribute_derive::__private::{syn, proc_macro2};
                        $parser.#ident = Some(syn::parse::Parse::parse($input)#error?);
                        if $input.is_empty() {
                            break
                        }
                        $input.step(|$cursor| match $cursor.punct() {
                            Some(($punct, $rest))
                                if $punct.as_char() == ',' =>
                            {
                                Ok(((), $rest))
                            }
                            _ => Err($cursor.error("Expected ,")),
                        })?;
                    })
                } else {
                    parsing.push(quote! {
                        # use ::attribute_derive::__private::{syn, proc_macro2};
                        #ident_str => {
                            $parser.#ident = Some(
                                if let Some(Some(__value)) = $is_flag.then(|| <#ty as ::attribute_derive::ConvertParsed>::as_flag()) {
                                    __value
                                } else {
                                    $input.step(|__cursor| match __cursor.punct() {
                                        Some((__punct, __rest))
                                            if __punct.as_char() == '=' && __punct.spacing() == proc_macro2::Spacing::Alone =>
                                        {
                                            Ok(((), __rest))
                                        }
                                        _ => Err(__cursor.error("Expected assignment `=`")),
                                    })?;
                                    syn::parse::Parse::parse($input)#error?
                                }
                            );
                        }
                    });
                }

                let error = missing.unwrap_or_else(|| {
                    format!("Mandatory `{ident}` was not specified via the attributes.")
                });
                assignments.push(if default {
                    quote! {
                        #ident: $parser.#ident.map(|t| ::attribute_derive::ConvertParsed::convert(t)).unwrap_or_else(|| Ok(<#ty as Default>::default()))?
                    }
                } else {
                    quote! {
                        # use ::attribute_derive::__private::{syn, proc_macro2};
                        #ident: match $parser.#ident.map(|t| ::attribute_derive::ConvertParsed::convert(t)) {
                                Some(__option) => __option?,
                                None if <#ty as ::attribute_derive::ConvertParsed>::default_by_default() => <#ty as ::attribute_derive::ConvertParsed>::default(),
                                _ => Err(syn::Error::new(proc_macro2::Span::call_site(), #error))?,
                            }
                    }
                });

                possible_variables.push(format!("`{ident_str}`"));
            }
        }
        _ => abort_call_site!("Only works on structs with named fields"),
    };

    let error_invalid_name = invalid_field.unwrap_or_else(|| match possible_variables.len() {
        0 => format!("Expected empty attribute"),
        1 => format!("Expected supported field {}", possible_variables[0]),
        _ => {
            let last = possible_variables.pop().unwrap();
            format!(
                "Supported fields are {} and {}",
                possible_variables.join(", "),
                last
            )
        }
    });

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
                loop {
                    #parsing_positional
                    break;
                }
                loop {
                    if $input.is_empty() {
                        break;
                    }

                    let $variable = Ident::parse($input)?;

                    let $is_flag = !$input.peek(Token!(=));

                    match $variable.to_string().as_str() {
                        #parsing
                        _ => {
                            return Err(syn::Error::new(
                                $variable.span(),
                                #error_invalid_name
                            ))
                        }
                    }

                    if $input.is_empty() {
                        break;
                    }

                    // Parse `,`
                    $input.step(|__cursor| match __cursor.punct() {
                        Some((__punct, __rest)) if __punct.as_char() == ',' => Ok(((), __rest)),
                        _ => Err(__cursor.error("Expected end of arg `,`")),
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

        #attribute_ident

        #[allow(unreachable_code)]
        impl #impl_generics ::attribute_derive::Attribute for #ident #ty_generics #where_clause {
            type Parser = #parser_ident;

            fn from_parser($parser: Self::Parser) -> Result<Self> {
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

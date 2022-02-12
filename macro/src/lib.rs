use proc_macro2::TokenStream;
use proc_macro_error::{abort_call_site, proc_macro_error, ResultExt};
use quote::quote;
use syn::{
    parse_macro_input, parse_quote, punctuated::Punctuated, DataStruct, DeriveInput, Field, Fields,
    FieldsNamed, Lit, Meta, MetaNameValue, NestedMeta, Path, Token,
};

// TODO generally should use fully qualified names for trait function calls

#[proc_macro_error]
#[proc_macro_derive(Attribute, attributes(attribute))]
pub fn attribute_derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let syn: Path = parse_quote!(::attribute_derive::__private::syn);
    let pm2: Path = parse_quote!(::attribute_derive::__private::proc_macro2);
    let some: Path = parse_quote!(::core::option::Option::Some);
    let ok: Path = parse_quote!(::core::result::Result::Ok);
    let err: Path = parse_quote!(::core::result::Result::Err);

    let DeriveInput {
        attrs,
        ident,
        generics,
        data,
        ..
    } = parse_macro_input!(input as DeriveInput);

    let mut attribute_ident = None;
    let mut invalid_field = None;

    for attribute in attrs
        .into_iter()
        .filter(|attribute| attribute.path.is_ident("attribute"))
    {
        const VALID_FORMAT: &str = r#"Expected `#[attribute(ident="name_of_your_attribute", invalid_field="error message")]`"#;
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
    let attribute_ident: String = attribute_ident.unwrap_or_else(|| {
        abort_call_site!(
            r#"You need to specify the attribute path via `#[attribute(ident="name_of_your_attribute")]`"#
        );
    });

    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let mut options_ty: Punctuated<TokenStream, Token!(,)> = Punctuated::new();
    let mut options_creation: Punctuated<TokenStream, Token!(,)> = Punctuated::new();
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
                let mut missing = None;
                let mut expected = None;
                for attribute in attrs
                    .into_iter()
                    .filter(|attribute| attribute.path.is_ident("attribute"))
                {
                    const VALID_FORMAT: &str = r#"Expected `#[attribute(default, missing="error message", expected="error message"])`"#;
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

                                    Meta::Path(path) => {
                                        if path.is_ident("default") {
                                            default = true;
                                        } else {
                                            abort_call_site!(VALID_FORMAT);
                                        }
                                    }
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
                options_creation.push(quote!(#ident: None));

                let error1 = format!("`{ident}` is specified multiple times");
                let error2 = format!("`{ident}` was already specified");
                option_assignments.push(quote! {
                    self.#ident = <#ty as ::attribute_derive::ConvertParsed>::aggregate(self.#ident.take(), __other.#ident, #error1, #error2)?;
                });

                let error = if let Some(expected) = expected {
                    quote! {.map_err(|__error| #syn::Error::new(__error.span(), #expected)) }
                } else {
                    quote!()
                };

                parsing.push(quote! {
                    #ident_str => {
                        __options.#ident = #some(
                            if let #some(#some(__value)) = __is_flag.then(|| <#ty as ::attribute_derive::ConvertParsed>::as_flag()) {
                                __value
                            } else {
                                __input.step(|__cursor| match __cursor.punct() {
                                    #some((__punct, __rest))
                                        if __punct.as_char() == '=' && __punct.spacing() == #pm2::Spacing::Alone =>
                                    {
                                        #ok(((), __rest))
                                    }
                                    _ => #err(__cursor.error("Expected assignment `=`")),
                                })?;
                                #syn::parse::Parse::parse(__input)#error?
                            }
                        );
                    }
                });

                let error = missing.unwrap_or_else(|| {
                    format!("Mandatory `{ident}` was not specified via the attributes.")
                });
                assignments.push(if default {
                    quote! {
                        #ident: __options.#ident.map(|t| ::attribute_derive::ConvertParsed::convert(t)).unwrap_or_else(|| #ok(<#ty as core::default::Default>::default()))?
                    }
                } else {
                    quote! {
                        #ident: match __options.#ident.map(|t| ::attribute_derive::ConvertParsed::convert(t)) {
                                Some(__option) => __option?,
                                None if <#ty as ::attribute_derive::ConvertParsed>::default_by_default() => <#ty as ::attribute_derive::ConvertParsed>::default(),
                                _ => #err(#syn::Error::new(#pm2::Span::call_site(), #error))?,
                            }
                    }
                });

                possible_variables.push(format!("`{ident_str}`"));
            }
        }
        _ => abort_call_site!("Only works on structs with named fields"),
    };

    let error_invalid_name = invalid_field.unwrap_or_else(|| {
        if possible_variables.len() > 1 {
            let last = possible_variables.pop().unwrap();
            format!(
                "Supported fields are {} and {}",
                possible_variables.join(", "),
                last
            )
        } else {
            format!("Expected supported field {}", possible_variables[0])
        }
    });

    quote! {
        #[allow(unreachable_code)]
        impl #impl_generics ::attribute_derive::Attribute for #ident #ty_generics #where_clause {
            fn from_attributes(__attrs: impl ::core::iter::IntoIterator<Item = #syn::Attribute>) -> #syn::Result<Self>{
                struct __Options{
                    #options_ty
                }
                impl __Options {
                    fn extend_with(&mut self, __other:Self) -> #syn::Result<()>{
                        #option_assignments
                        #ok(())
                    }
                }
                impl #syn::parse::Parse for __Options {
                    fn parse(__input: #syn::parse::ParseStream<'_>) -> #syn::Result<Self> {
                        let mut __options = __Options{
                            #options_creation
                        };
                        loop {
                            if __input.is_empty() {
                                break;
                            }

                            let __variable = #syn::Ident::parse(__input)?;

                            let __is_flag = !__input.peek(#syn::Token!(=));

                            match __variable.to_string().as_str() {
                                #parsing
                                _ => {
                                    return #err(#syn::Error::new(
                                        __variable.span(),
                                        #error_invalid_name
                                    ))
                                }
                            }

                            if __input.is_empty() {
                                break;
                            }

                            // Parse `,`
                            __input.step(|__cursor| match __cursor.punct() {
                                #some((__punct, __rest)) if __punct.as_char() == ',' => #ok(((), __rest)),
                                _ => #err(__cursor.error("Expected assignment `=`")),
                            })?;
                        }
                        Ok(__options)
                    }
                }
                let mut __options = __Options{
                    #options_creation
                };
                for __attribute in __attrs {
                    if __attribute.path.is_ident(#attribute_ident) {
                        __options.extend_with(__attribute.parse_args()?)?;
                    }
                }
                #ok(Self {
                    #assignments
                })
            }
        }
    }
    .into()
}

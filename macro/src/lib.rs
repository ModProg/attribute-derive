use proc_macro2::{Ident, TokenStream};
use proc_macro_error::{abort, abort_call_site, proc_macro_error, ResultExt};
use quote::quote;
use syn::{
    parse_macro_input, parse_quote, punctuated::Punctuated, DataStruct, DeriveInput, Field, Fields,
    FieldsNamed, Path, Token,
};

// TODO generally should use fully qualified names for trait function calls

#[proc_macro_error]
#[proc_macro_derive(Attribute, attributes(attribute))]
pub fn attribute_derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let syn: Path = parse_quote!(::attribute_derive::__private::syn);
    let pm2: Path = parse_quote!(::attribute_derive::__private::proc_macro2);
    let none: Path = parse_quote!(::core::option::Option::None);
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

    let attribute_ident: String = attrs
        .into_iter()
        .find_map(|attribute| {
            if attribute.path.is_ident("attribute") {
                let path: Path = attribute.parse_args().unwrap_or_abort();
                Some(
                    path.get_ident()
                        .unwrap_or_else(|| {
                            abort_call_site!("Only single idents are currently supported")
                        })
                        .to_string(),
                )
            } else {
                None
            }
        })
        .unwrap_or_else(|| {
            abort_call_site!(
                "You need to specify the attribute path via `#[attribute(name_of_your_attribute)]`"
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
                let default: bool =
                    attrs
                        .into_iter()
                        .find_map(|attribute| {
                            if attribute.path.is_ident("attribute") {
                                Some(
                                    attribute
                                        .parse_args()
                                        .ok()
                                        .and_then(|ident: Ident| {
                                            if ident == "default" {
                                                Some(true)
                                            } else {
                                                None
                                            }
                                        })
                                        .unwrap_or_else(|| {
                                            abort!(
                                            attribute,
                                            "Only `#[attribute(default)]` is currently supported"
                                        )
                                        }),
                                )
                            } else {
                                None
                            }
                        })
                        .unwrap_or_default();
                let ident = ident.expect("named struct fields have idents");
                let ident_str = ident.to_string();

                options_ty
                    .push(quote!(#ident: Option<<#ty as ::attribute_derive::ConvertParsed>::Type>));
                options_creation.push(quote!(#ident: None));

                let error1 = format!("`{ident}` is specified multiple times");
                let error2 = format!("`{ident}` was already specified");
                option_assignments.push(quote! {
                    match (&self.#ident, __other.#ident) {
                        (#none, __value @ #some(_)) => self.#ident = __value,
                        (#some(__first), #some(__second)) => {
                            let mut __error = <<#ty as ::attribute_derive::ConvertParsed>::Type as ::attribute_derive::Error>::error(__first, #error1);
                            __error.combine(<<#ty as ::attribute_derive::ConvertParsed>::Type as ::attribute_derive::Error>::error(
                                &__second,
                                #error2,
                            ));
                            return #err(__error);
                        }
                        _ => {}
                    }
                });

                parsing.push(quote! {
                    #ident_str => {
                        __options.#ident = #some(
                            // ::attribute_derive::ConvertParsed::convert(__input.parse()?)?
                            // TODO FQN
                            __input.parse()?
                        );
                    }
                });

                let error = format!("Mandatory `{ident}` was not specified via the attributes.");
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

    let error_invalid_name = if possible_variables.len() > 1 {
        let last = possible_variables.pop().unwrap();
        format!(
            "Supported fields are {} and {}",
            possible_variables.join(", "),
            last
        )
    } else {
        format!("Expected supported field {}", possible_variables[0])
    };

    quote! {
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

                            // Parse `=`
                            __input.step(|__cursor| match __cursor.punct() {
                                #some((__punct, __rest))
                                    if __punct.as_char() == '=' && __punct.spacing() == #pm2::Spacing::Alone =>
                                {
                                    #ok(((), __rest))
                                }
                                _ => #err(__cursor.error("Expected assignment `=`")),
                            })?;

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

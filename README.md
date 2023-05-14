# attribute-derive

[![docs.rs](https://img.shields.io/docsrs/attribute-derive)](https://docs.rs/attribute-derive/latest/attribute_derive/)
[![lib.rs](https://img.shields.io/crates/v/attribute-derive)](https://lib.rs/crates/attribute-derive)
[![MIT](https://img.shields.io/crates/l/attribute-derive)](LICENSE)

Basically clap for attribute macros:
```rust
#[derive(Attribute)]
#[attribute(ident = "collection")]
#[attribute(invalid_field = "Error when an unsupported value is set (e.g. meaning=42")]
struct CollectionAttribute {
    // Options are optional by default (will be set to None if not specified)
    authority: Option<String>,
    #[attribute(missing = "Error when the value is not set")]
    name: String,
    // Any type implementing default can be flagged as default
    // This will be set to Vec::default() when not specified
    #[attribute(default)]
    #[attribute(expected = "Error when an error occured while parsing")]
    views: Vec<Type>,
    // Booleans can be used without assiging a value. as a flag.
    // If omitted they are set to false
    some_flag
}
```

Will be able to parse an attribute like this:
```rust
#[collection(authority="Some String", name = r#"Another string"#, views = [Option, ()])]
```

## Limitations

There are some limitations in syntax parsing that will be lifted future releases.

- literals in top level (meaning something like `#[attr(42, 3.14, "hi")]`
- function like arguments (something like `#[attr(view(a = "test"))]`
- other syntaxes, maybe something like `key: value`

## Parse methods

There are multiple ways of parsing a struct deriving [`Attribute`](https://docs.rs/attribute-derive/latest/attribute_derive/trait.Attribute.html).

For helper attributes there is:
- [`Attribute::from_attributes`](https://docs.rs/attribute-derive/latest/attribute_derive/trait.Attribute.html#tymethod.from_attributes) which takes in an [`IntoIterator<Item = &'a
syn::Attribute`](https://docs.rs/syn/latest/syn/struct.Attribute.html)
(e.g. a [`&Vec<syn::Attribute>`](https://docs.rs/syn/latest/syn/struct.Attribute.html)). Most useful for derive macros.
- [`Attribute::remove_attributes`](https://docs.rs/attribute-derive/latest/attribute_derive/trait.Attribute.html#tymethod.remove_attributes) which takes an [`&mut Vec<syn::Attribute>`](https://docs.rs/syn/latest/syn/struct.Attribute.html)
and does not only parse the [`Attribute`](https://docs.rs/attribute-derive/latest/attribute_derive/trait.Attribute.html#tymethod.from_attributes) but also removes those matching. Useful for helper
attributes for proc macros, where the helper attributes need to be removed.

For parsing a single [`TokenStream`](https://docs.rs/proc-macro2/latest/proc_macro2/struct.TokenStream.html) e.g. for parsing the proc macro input there a two ways:

- [`Attribute::from_args`](https://docs.rs/attribute-derive/latest/attribute_derive/trait.Attribute.html#tymethod.from_args) taking in a [`TokenStream`](https://docs.rs/proc-macro2/latest/proc_macro2/struct.TokenStream.html)
- As `derive(Attribute)` also derives [`Parse`](https://docs.rs/syn/latest/syn/parse/trait.Parse.html) so you can use the [parse](https://docs.rs/syn/latest/syn/parse/index.html) API,
e.g. with [`parse_macro_input!(tokens as Attribute)`](https://docs.rs/syn/latest/syn/macro.parse_macro_input.html).

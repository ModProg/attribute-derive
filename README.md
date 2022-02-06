# attribute-derive

![docs.rs](https://img.shields.io/docsrs/attribute-derive)
![crates.io](https://img.shields.io/crates/v/attribute-derive)
![MIT](https://img.shields.io/crates/v/attribute-derive)

Basicly clap for attribute macros:
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

use attribute_derive::Attribute;
use syn::parse_quote;

#[test]
fn test() {
    use syn::{Expr, LitStr, Type};
    #[derive(Attribute)]
    #[attribute(test)]
    struct Test {
        // a: u8,
        b: LitStr,
        c: String,
        #[attribute(default)]
        oc: Option<String>,
        #[attribute(default)]
        od: Option<Type>,
        d: Type,
        e: Expr,
    }

    let parsed = Test::from_attributes([
        parse_quote!(#[test(b="hi", c="ho", oc="xD", d=(), e=if true{ "a" } else { "b" })]),
    ])
    .unwrap();
    assert_eq!(parsed.b.value(), "hi");
    assert_eq!(parsed.c, "ho");
    assert_eq!(parsed.oc, Some("xD".to_owned()));
    assert!(parsed.od.is_none());
    assert!(matches!(parsed.d, Type::Tuple(_)));
    assert!(matches!(parsed.e, Expr::If(_)));
}

#[test]
fn error() {
    #[derive(Attribute, Debug)]
    #[attribute(test)]
    struct Test {
        s: String,
    }

    assert_eq!(
        Test::from_attributes([parse_quote!(#[test()])])
            .unwrap_err()
            .to_string(),
        "Mandatory `s` was not specified via the attributes."
    );

    assert_eq!(
        Test::from_attributes([parse_quote!(#[test(s=())])])
            .unwrap_err()
            .to_string(),
        "expected string literal"
    );
}

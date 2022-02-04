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
        f: Vec<Type>
    }

    let parsed = Test::from_attributes([
        parse_quote!(#[test(b="hi", c="ho", oc="xD", d=(), e=if true{ "a" } else { "b" }, f= [(), Debug])]),
    ])
    .unwrap();
    assert_eq!(parsed.b.value(), "hi");
    assert_eq!(parsed.c, "ho");
    assert_eq!(parsed.oc, Some("xD".to_owned()));
    assert!(parsed.od.is_none());
    assert!(matches!(parsed.d, Type::Tuple(_)));
    assert!(matches!(parsed.e, Expr::If(_)));
    assert!(parsed.f.len() == 2);
}

#[test]
fn error() {
    #[derive(Attribute, Debug)]
    #[attribute(test)]
    struct Test {
        #[allow(dead_code)]
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

    assert_eq!(
        Test::from_attributes([parse_quote!(#[test(invalid_attribute)])])
            .unwrap_err()
            .to_string(),
        "unexpected end of input, Expected assignment `=`"
    );

    assert_eq!(
        Test::from_attributes([parse_quote!(#[test(invalid_attribute="")])])
            .unwrap_err()
            .to_string(),
        "Expected supported field `s`"
    );
}

#[test]
fn error2() {
    #[derive(Attribute, Debug)]
    #[attribute(test)]
    struct Test {
        #[allow(dead_code)]
        a: f32,
        b: u8
    }

    assert_eq!(
        Test::from_attributes([parse_quote!(#[test()])])
            .unwrap_err()
            .to_string(),
        "Mandatory `a` was not specified via the attributes."
    );

    assert_eq!(
        Test::from_attributes([parse_quote!(#[test(a="")])])
            .unwrap_err()
            .to_string(),
        "expected floating point literal"
    );

    assert_eq!(
        Test::from_attributes([parse_quote!(#[test(a=0., b=10.)])])
            .unwrap_err()
            .to_string(),
        "expected integer literal"
    );

    // FIXME I expected this to fail....
    // assert_eq!(
    //     Test::from_attributes([parse_quote!(#[test(a=1.7976931348623157E+308f64,b=0)])])
    //         .unwrap_err()
    //         .to_string(),
    //     "unexpected end of input, Expected assignment `=`"
    // );

    assert_eq!(
        Test::from_attributes([parse_quote!(#[test(a=0.,b=1000000)])])
            .unwrap_err()
            .to_string(),
        "number too large to fit in target type"
    );
}

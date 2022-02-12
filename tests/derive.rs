use attribute_derive::Attribute;
use proc_macro2::TokenStream;
use syn::parse_quote;

#[test]
fn test() {
    use syn::{Expr, LitStr, Type};
    #[derive(Attribute)]
    #[attribute(ident = "test")]
    struct Test {
        // a: u8,
        b: LitStr,
        c: String,
        oc: Option<String>,
        od: Option<Type>,
        d: Type,
        e: Expr,
        f: Vec<Type>,
        g: bool,
        h: bool,
        i: TokenStream,
    }

    let parsed = Test::from_attributes([
        parse_quote!(#[test(b="hi", c="ho", oc="xD", d=(), e=if true{ "a" } else { "b" }, f= [(), Debug], g, i = smth::hello + 24/3'a', b = c)]),
    ])
    .unwrap();
    assert_eq!(parsed.b.value(), "hi");
    assert_eq!(parsed.c, "ho");
    assert_eq!(parsed.oc, Some("xD".to_owned()));
    assert!(parsed.od.is_none());
    assert!(matches!(parsed.d, Type::Tuple(_)));
    assert!(matches!(parsed.e, Expr::If(_)));
    assert!(parsed.f.len() == 2);
    assert!(parsed.g);
    assert!(!parsed.h);
    assert_eq!(parsed.i.to_string(), "smth :: hello + 24 / 3 'a' , b = c");
}

#[test]
fn error() {
    #[derive(Attribute, Debug)]
    #[attribute(ident = "test")]
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
        "Expected supported field `s`"
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
    #[attribute(ident = "test")]
    #[allow(dead_code)]
    struct Test {
        #[attribute(expected = "no")]
        a: f32,
        b: u8,
        #[attribute(missing = "yes")]
        c: String,
    }

    assert_eq!(
        Test::from_attributes([parse_quote!(#[test(d="")])])
            .unwrap_err()
            .to_string(),
        "Supported fields are `a`, `b` and `c`"
    );

    assert_eq!(
        Test::from_attributes([parse_quote!(#[test()])])
            .unwrap_err()
            .to_string(),
        "Mandatory `a` was not specified via the attributes."
    );

    assert_eq!(
        Test::from_attributes([parse_quote!(#[test(a=1., b=1)])])
            .unwrap_err()
            .to_string(),
        "yes"
    );

    assert_eq!(
        Test::from_attributes([parse_quote!(#[test(a="")])])
            .unwrap_err()
            .to_string(),
        "no"
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

#[test]
fn error_specified() {
    #[derive(Attribute, Debug)]
    #[attribute(ident = "test")]
    #[attribute(invalid_field = "error message")]
    #[allow(dead_code)]
    struct Test {}

    assert_eq!(
        Test::from_attributes([parse_quote!(#[test(c="")])])
            .unwrap_err()
            .to_string(),
        "error message"
    );
}

#[test]
fn default() {
    #[derive(Attribute, Debug)]
    #[attribute(ident = "test")]
    struct Test {
        #[attribute(default)]
        hi: f32,
    }
    assert_eq!(Test::from_attributes([]).unwrap().hi, 0.);
}

#[test]
fn aggregate() {
    #[derive(Attribute, Debug)]
    #[attribute(ident = "test")]
    struct Test {
        strings: Vec<String>,
    }

    assert_eq!(
        Test::from_attributes([
            parse_quote!(#[test(strings=["a"])]),
            parse_quote!(#[test(strings=["b"])])
        ])
        .unwrap()
        .strings,
        vec!["a".to_owned(), "b".to_owned()]
    )
}

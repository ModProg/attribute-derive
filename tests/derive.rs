use attribute_derive::parsing::AttributeNamed;
use attribute_derive::FromAttr;
use proc_macro2::TokenStream;
use quote::quote;
use syn::parse::{ParseStream, Parser};
use syn::{parse_quote, Attribute};

#[test]
fn token_stream() {
    TokenStream::from_input(quote!("hello" "world")).unwrap();
    (|p: ParseStream| TokenStream::parse_named("name", p))
        .parse2(quote!(name("hello" "world")))
        .unwrap();
    (|p: ParseStream| TokenStream::parse_named("name", p))
        .parse2(quote!(name = "hello" "world"))
        .unwrap_err();
}

#[cfg(feature = "syn-full")]
#[test]
fn test() {
    use proc_macro2::TokenStream;
    use quote::quote;
    use syn::{parse2, Expr, LitStr, Type};

    #[derive(FromAttr)]
    #[attribute(ident = test)]
    struct Test {
        #[attribute(positional)]
        a: u8,
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
        parse_quote!(#[test(8, b="hi", c="ho", oc="xD", d=(), e=if true { "a" } else { "b" }, f= [(), Debug], g, i(smth::hello + 24/3'a', b = c))]),
    ].iter())
    .unwrap();
    assert_eq!(parsed.a, 8);
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

    let parsed = Test::from_input(
        quote!(8, b="hi", c="ho", oc="xD", d=(), e=if true{ "a" } else { "b" }, f= [(), Debug], g, i(smth::hello + 24/3'a', b = c))
    )
    .unwrap();
    // assert_eq!(parsed.a, 8);
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

    let mut attrs = vec![
        parse_quote!(#[something]),
        parse_quote!(#[test(8, b="hi", c="ho", oc="xD", d=(), e=if true{ "a" } else { "b" }, f= [(), Debug], g, i(smth::hello + 24/3'a', b = c))]),
        parse_quote!(#[another(smth)]),
    ];
    let parsed = Test::remove_attributes(&mut attrs).unwrap();
    // assert_eq!(parsed.a, 8);
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
    assert_eq!(attrs.len(), 2);

    let parsed: Test = parse2(
        quote!(8, b="hi", c="ho", oc="xD", d=(), e=if true{ "a" } else { "b" }, f= [(), Debug], g, i(smth::hello + 24/3'a', b = c))
    )
    .unwrap();
    // assert_eq!(parsed.a, 8);
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
fn default() {
    #[derive(FromAttr, Debug, PartialEq)]
    #[attribute(ident = test)]
    struct Test {
        #[attribute(optional)]
        hi: f32,
        #[attribute(default = 10)]
        ho: usize,
    }
    assert_eq!(Test::from_attributes::<Attribute>([]).unwrap(), Test {
        hi: 0.,
        ho: 10
    });
}

#[test]
fn aggregate() {
    #[derive(FromAttr, Debug)]
    #[attribute(ident = test)]
    struct Test {
        strings: Vec<String>,
    }

    assert_eq!(
        Test::from_attributes(&[
            parse_quote!(#[test(strings=["a"], strings=["b"])]),
            parse_quote!(#[test(strings=["c"])])
        ])
        .unwrap()
        .strings,
        ["a", "b", "c"].map(ToOwned::to_owned)
    )
}

#[test]
fn without_ident() {
    #[derive(FromAttr)]
    struct Test {
        a: u8,
    }

    let parsed: Test = parse_quote!(a = 5);
    assert_eq!(parsed.a, 5);
}

#[test]
fn empty() {
    #[derive(FromAttr)]
    #[attribute(ident = test)]
    struct Test {}
}

#[test]
fn convert_parsed_as_from_attr() {
    let parsed = String::from_input(quote!("hello")).unwrap();
    assert_eq!(parsed, "hello");
}

#[test]
fn sub_attr() {
    #[derive(FromAttr)]
    #[attribute(ident = test)]
    struct Test {
        sub: SubTest,
    }

    #[derive(FromAttr)]
    struct SubTest {
        value: String,
    }

    assert_eq!(
        Test::from_attributes(&[parse_quote!(#[test(sub(value="a"))])])
            .unwrap()
            .sub
            .value,
        "a"
    )
}

use example::Normal;

#[derive(Normal)]
struct None;

#[derive(Normal)]
#[ident(example = 2.)]
struct Example;

#[derive(Normal)]
#[ident(
    example = 2.,
    optional_implicit = 10,
    optional_default = 3,
    default = 2,
    conflict_a = "hello",
    mandatory_flag
)]
struct ExampleOI;

#[derive(Normal)]
#[ident(example = 1., mandatory_flag)]
#[a(conflict_a = "hey")]
#[b(conflict_b = "hi")]
struct Conflict;

#[derive(Normal)]
#[ident(hello)]
#[single(hello)]
#[empty(hello)]
struct UnknownField;

fn main() {}

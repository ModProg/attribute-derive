use example::Custom;

#[derive(Custom)]
struct None;

#[derive(Custom)]
#[ident(example = 2.)]
struct Example;

#[derive(Custom)]
#[ident(
    example = 2.,
    optional_implicit = {10},
    optional_default = 3,
    default = 2,
    conflict_a = "hello",
)]
struct ExampleOI;

#[derive(Custom)]
#[ident(example = 1.)]
#[a(conflict_a = "hey")]
#[b(conflict_b = "hi")]
struct Conflict;

#[derive(Custom)]
#[ident(hello)]
#[single(hello)]
#[empty(hello)]
struct UnknownField;

fn main() {}

use petty_lox::{run_get_string, Args, Subcommand};

macro_rules! test_file {
    ($name: ident, $path: literal, $expected: literal) => {
        #[test]
        fn $name() {
            let args = Args { subcommand: Subcommand::Tokenize { input: $path.into() } };
            let output = run_get_string(args).unwrap();
            let output = output.trim();
            let expected = $expected.trim();
            assert!(output == expected, "--- Expected ---\n\n{expected}\n\n---Got---\n\n{output}");
        }
    };
}

test_file! {
    hello_world,
    "examples/hello_world.lox",
    r#"
PRINT
STRING "Hello, World!"
"#
}

test_file! {
    numbers,
    "tests/lox/numbers.lox",
    r#"
NUMBER 123
MINUS
NUMBER 123.456
DOT
MINUS
NUMBER 123
NUMBER 456
DOT
NUMBER 123
DOT
DOT
NUMBER 456
"#
}

use petty_lox::{run_get_string, Args, Subcommand};

macro_rules! test_file {
    ($name: ident, $path: literal, $expected: literal) => {
        #[test]
        fn $name() {
            let args = Args {
                subcommand: Subcommand::Tokenize {
                    input: $path.into(),
                },
            };
            let output = run_get_string(args).unwrap();
            let expected = $expected;
            assert_eq!(output.trim(), expected.trim());
        }
    };
}

test_file!(
    hello_world,
    "examples/hello_world.lox",
    r#"
PRINT
STRING "Hello, World!"
EOF
"#
);

use petty_lox::{run, Args, Subcommand};

macro_rules! test_file {
    ($name: ident, $path: literal) => {
        #[test]
        fn $name() {
            let args = Args { subcommand: Subcommand::Parse { input: $path.into() } };
            // Just check if it parses sucessfully for now
            run(args).unwrap();
        }
    };
}

test_file!(hello_world, "examples/hello_world.lox");
test_file!(arithmetic, "examples/arithmetic.lox");
test_file!(data_types, "examples/data_types.lox");
test_file!(block, "examples/block.lox");
test_file!(variables, "examples/variables.lox");
test_file!(control_flow, "examples/control_flow.lox");

use clap::Parser;
use petty_lox::Args;

fn main() -> eyre::Result<()> {
    petty_lox::run(Args::parse())
}

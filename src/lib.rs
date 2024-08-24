use parser::tokenize_spanned;
use std::{io::Write, path::PathBuf};

#[derive(Debug, clap::Parser)]
pub struct Args {
    #[command(subcommand)]
    pub subcommand: Subcommand,
}

#[derive(Debug, clap::Subcommand)]
pub enum Subcommand {
    Tokenize { input: PathBuf },
}

pub fn run(args: Args) -> eyre::Result<()> {
    run_with(args, std::io::stdout())
}

pub fn run_get_string(args: Args) -> eyre::Result<String> {
    let mut buf = vec![];
    run_with(args, &mut buf)?;
    Ok(String::from_utf8(buf)?)
}

pub fn run_with(args: Args, mut stdout: impl Write) -> eyre::Result<()> {
    match args.subcommand {
        Subcommand::Tokenize { input } => {
            let source = std::fs::read_to_string(input)?;
            for (token, span) in tokenize_spanned(&source) {
                let token = token.map_err(|_| eyre::eyre!("Invalid token at {span:?}"))?;
                writeln!(stdout, "{token}")?;
            }
            writeln!(stdout, "EOF")?;
        }
    }
    Ok(())
}

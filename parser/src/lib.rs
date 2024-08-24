mod token;
use std::ops::Range;

use logos::Logos;
pub use token::Token;

pub fn tokenize(source: &str) -> impl Iterator<Item = Result<Token<'_>, ()>> {
    Token::lexer(source)
}

pub fn tokenize_spanned(
    source: &str,
) -> impl Iterator<Item = (Result<Token<'_>, ()>, Range<usize>)> {
    Token::lexer(source).spanned()
}

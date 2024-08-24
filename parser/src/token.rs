use std::{borrow::Cow, fmt};

use logos::Lexer;

#[rustfmt::skip]
#[derive(Debug, logos::Logos)]
#[logos(skip r"[ \t\s\r\n]")]
#[logos(skip r"//[^\n]\n")]
pub enum Token<'a> {
    //Tokens
    #[token("(")] LParen,
    #[token(")")] RParen,
    #[token("{")] LBrace,
    #[token("}")] RBrace,
    #[token(",")] Comma,
    #[token(".")] Dot,
    #[token("-")] Minus,
    #[token("+")] Plus,
    #[token(";")] Semicolon,
    #[token("/")] Slash,
    #[token("*")] Star,
    #[token("!")] Bang,
    #[token("!=")] BangEq,
    #[token("=")] Eq,
    #[token("==")] EqEq,
    #[token(">")] Greater,
    #[token("<")] Less,
    #[token(">=")] GreaterEq,
    #[token("<=")] LessEq,
    // Keywords
    #[token("and")] KwAnd,
    #[token("class")] KwClass,
    #[token("else")] KwElse,
    #[token("false")] KwFalse,
    #[token("fun")] KwFun,
    #[token("for")] KwFor,
    #[token("if")] KwIf,
    #[token("nil")] KwNil,
    #[token("or")] KwOr,
    #[token("print")] KwPrint,
    #[token("return")] KwReturn,
    #[token("super")] KwSuper,
    #[token("this")] KwThis,
    #[token("true")] KwTrue,
    #[token("var")] KwVar,
    #[token("while")] KwWhile,
    // Literals
    #[regex("[a-zA-Z_][a-zA-Z_0-9]*")]
    Ident(&'a str),
    #[regex(r#""([^"]|\\")*""#, parse_str)]
    String(Cow<'a, str>),
    #[regex("r-?[0-9_]+", parse_integer)]
    #[regex(r"-?[0-9_]+\.[0-9_]+", parse_float)]
    Number(f64),
}

fn parse_integer<'a>(lex: &mut Lexer<'a, Token<'a>>) -> Result<f64, ()> {
    lex.slice().parse::<i32>().map_err(|_| ()).map(f64::from)
}

fn parse_float<'a>(lex: &mut Lexer<'a, Token<'a>>) -> Result<f64, ()> {
    lex.slice().parse().map_err(|_| ())
}

fn parse_str<'a>(lex: &mut Lexer<'a, Token<'a>>) -> Cow<'a, str> {
    let raw = &lex.slice()[1..lex.slice().len() - 1];
    // TODO: impl escape sequence
    Cow::Borrowed(raw)
}

impl fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            //Tokens
            Self::LParen => write!(f, "LPAREN"),
            Self::RParen => write!(f, "RPAREN"),
            Self::LBrace => write!(f, "LBRACE"),
            Self::RBrace => write!(f, "RBRACE"),
            Self::Comma => write!(f, "COMMA"),
            Self::Dot => write!(f, "DOT"),
            Self::Minus => write!(f, "MINUS"),
            Self::Plus => write!(f, "PLUS"),
            Self::Semicolon => write!(f, "SEMICOLON"),
            Self::Slash => write!(f, "SLASH"),
            Self::Star => write!(f, "STAR"),
            Self::Bang => write!(f, "BANG"),
            Self::BangEq => write!(f, "BANG_EQUAL"),
            Self::Eq => write!(f, "EQUAL"),
            Self::EqEq => write!(f, "EQUAL_EQUAL"),
            Self::Greater => write!(f, "GREATER"),
            Self::Less => write!(f, "LESS"),
            Self::GreaterEq => write!(f, "GREATER_EQUAL"),
            Self::LessEq => write!(f, "LESS_EQUAL"),
            // Keywords
            Self::KwAnd => write!(f, "AND"),
            Self::KwClass => write!(f, "CLASS"),
            Self::KwElse => write!(f, "ELSE"),
            Self::KwFalse => write!(f, "FALSE"),
            Self::KwFun => write!(f, "FUN"),
            Self::KwFor => write!(f, "FOR"),
            Self::KwIf => write!(f, "IF"),
            Self::KwNil => write!(f, "NIL"),
            Self::KwOr => write!(f, "OR"),
            Self::KwPrint => write!(f, "PRINT"),
            Self::KwReturn => write!(f, "RETURN"),
            Self::KwSuper => write!(f, "SUPER"),
            Self::KwThis => write!(f, "THIS"),
            Self::KwTrue => write!(f, "TRUE"),
            Self::KwVar => write!(f, "VAR"),
            Self::KwWhile => write!(f, "WHILE"),
            // Literals
            Self::Ident(ident) => write!(f, "IDENT {ident}"),
            Self::String(string) => write!(f, "STRING {string:?}"),
            Self::Number(number) => write!(f, "NUMBER {number}"),
        }
    }
}

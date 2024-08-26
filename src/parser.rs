use std::fmt;

use logos::Logos;
use ustr::Ustr;

use crate::lexer::{self, Token};

pub type Block = Box<[Statement]>;
pub type ExpressionList = Box<[Expression]>;
pub type Ident = Ustr;

type Lexer<'a> = logos::Lexer<'a, Token>;
type Result<T, E = ParseError> = std::result::Result<T, E>;

#[derive(Debug, thiserror::Error)]
pub enum ParseError {
    #[error("{0}")]
    LexError(#[from] lexer::Error),
    #[error("Unexpected token: {0}")]
    UnexpectedToken(Token),
    #[error("Unexpected token: EOF")]
    UnexpectedEof,
    #[error("Unknown Parse Error")]
    UnknownErr,
}

pub fn parse(input: &str) -> Result<Node> {
    parse_statement_many(&mut Token::lexer(input))
        .map(|block| Node::Statement(Statement::Block(block)))
}

pub fn parse_statement(lexer: &mut Lexer) -> Result<Statement> {
    let pre = lexer.clone();
    Ok(match lexer.next().transpose()?.ok_or(ParseError::UnexpectedEof)? {
        Token::Ident(ident)
            if matches!(lexer.clone().next().transpose()?, Some(Token::Eq | Token::Dot)) =>
        {
            parse_assignment(lexer, ident)?
        }
        Token::LBrace => parse_block(lexer).map(Statement::Block)?,
        Token::KwIf => Statement::If(parse_if_stmt(lexer)?),
        Token::KwWhile => parse_while_loop(lexer)?,
        Token::KwFor => parse_for_loop(lexer)?,
        Token::KwVar => parse_var_decl(lexer)?,
        Token::KwPrint => Statement::Print(parse_expression(lexer)?),
        Token::KwBreak => Statement::Break(parse_expression(lexer)?),
        Token::KwReturn => Statement::Return(parse_expression(lexer)?),
        _ => {
            *lexer = pre;
            Statement::Lone(parse_expression(lexer)?)
        }
    })
}

pub fn parse_statement_many(lexer: &mut Lexer) -> Result<Block> {
    let mut block = vec![];
    loop {
        let pre = lexer.clone();
        let Some(next) = lexer.next().transpose()? else { break };
        let statement = match next {
            Token::Semicolon => continue,
            Token::RBrace => break *lexer = pre,
            _ => {
                *lexer = pre;
                parse_statement(lexer)?
            }
        };
        block.push(statement);
    }
    Ok(block.into())
}

pub fn parse_expression(lexer: &mut Lexer) -> Result<Expression> {
    parse_expr_precedence(lexer, 0)
}

pub fn parse_expr_precedence(lexer: &mut Lexer, precedence: usize) -> Result<Expression> {
    const OPS: &[&[BinaryOp]] = &[
        &[BinaryOp::Or],
        &[BinaryOp::And],
        &[
            BinaryOp::IsEq,
            BinaryOp::NotEq,
            BinaryOp::Greater,
            BinaryOp::Less,
            BinaryOp::GreaterEq,
            BinaryOp::LessEq,
        ],
        &[BinaryOp::Plus, BinaryOp::Minus],
        &[BinaryOp::Multiply, BinaryOp::Divide],
    ];
    let Some(&ops) = OPS.get(precedence) else {
        return parse_super_expr(lexer);
    };
    let mut root = parse_expr_precedence(lexer, precedence + 1)?;

    while let Some(token) = lexer.clone().next().transpose()? {
        let Ok(op) = BinaryOp::try_from(token) else {
            break;
        };
        if !ops.contains(&op) {
            break;
        }
        _ = lexer.next();
        let expr = parse_expr_precedence(lexer, precedence + 1)?;
        root = Expression::BinaryExpr { operator: op, operands: Box::new([root, expr]) };
    }

    Ok(root)
}

pub fn parse_super_expr(lexer: &mut Lexer) -> Result<Expression> {
    let mut root = parse_atom_inner(lexer)?;

    while let Some(token) = lexer.clone().next().transpose()? {
        let Token::Dot = token else { break };
        _ = lexer.next();
        let expr = parse_atom_inner(lexer)?;
        root = Expression::BinaryExpr {
            operator: BinaryOp::FieldAcess,
            operands: Box::new([root, expr]),
        };
    }

    while let Some(token) = lexer.clone().next().transpose()? {
        let Token::LParen = token else { break };
        _ = lexer.next();
        let args = parse_seperated_exprs(lexer, Token::Comma, Token::RParen)?;
        root = Expression::FunCall { expr: Box::new(root), args };
    }

    Ok(root)
}

pub fn parse_seperated_exprs(
    lexer: &mut Lexer,
    seperator: Token,
    terminator: Token,
) -> Result<Box<[Expression]>> {
    let mut atoms = vec![];
    loop {
        match lexer.clone().next().transpose()? {
            None => break,
            Some(token) if token == seperator => continue,
            Some(token) if token == terminator => break,
            Some(_) => atoms.push(parse_expression(lexer)?),
        }
    }
    _ = lexer.next();
    Ok(atoms.into())
}

pub fn parse_atom(lexer: &mut Lexer) -> Result<Expression> {
    let expr = parse_atom_inner(lexer)?;
    let Some(Token::LParen) = lexer.clone().next().transpose()? else {
        return Ok(expr);
    };
    _ = lexer.next();
    let args = parse_seperated_exprs(lexer, Token::Comma, Token::RParen)?;
    Ok(Expression::FunCall { expr: Box::new(expr), args })
}

pub fn parse_atom_inner(lexer: &mut Lexer) -> Result<Expression> {
    let token = lexer.next().transpose()?.ok_or(ParseError::UnexpectedEof)?;

    Ok(match token {
        Token::Bang => Expression::UnaryExpr {
            operator: UnaryOp::Not,
            operand: Box::new(parse_super_expr(lexer)?),
        },
        Token::Minus => Expression::UnaryExpr {
            operator: UnaryOp::Negative,
            operand: Box::new(parse_super_expr(lexer)?),
        },
        Token::Ident(ident) => Expression::Identifier(ident),
        Token::Number(number) => Literal::Number(number).into(),
        Token::String(string) => Literal::String(string).into(),
        Token::KwTrue => Literal::Boolean(true).into(),
        Token::KwFalse => Literal::Boolean(false).into(),
        Token::LParen => {
            let expr = parse_expression(lexer)?;
            expect(Token::RParen, lexer)?;
            expr
        }
        _ => return Err(ParseError::UnexpectedToken(token)),
    })
}

pub fn parse_block(lexer: &mut Lexer) -> Result<Block> {
    let body = parse_statement_many(lexer)?;
    expect(Token::RBrace, lexer)?;
    Ok(body)
}

pub fn parse_assignment(lexer: &mut Lexer, root: Ustr) -> Result<Statement> {
    let mut lhs = vec![root];

    while let Some(token) = lexer.next().transpose()? {
        match token {
            Token::Dot => {}
            Token::Eq => break,
            _ => return Err(ParseError::UnexpectedToken(token)),
        };
        let ident = match lexer.next().transpose()? {
            Some(Token::Ident(ident)) => ident,
            Some(token) => return Err(ParseError::UnexpectedToken(token)),
            None => return Err(ParseError::UnexpectedEof),
        };
        lhs.push(ident);
    }
    let expr = parse_expression(lexer)?;
    Ok(Statement::Assignment(lhs.into(), expr))
}

pub fn parse_var_decl(lexer: &mut Lexer) -> Result<Statement> {
    let ident = match lexer.next().transpose()? {
        None => return Err(ParseError::UnexpectedEof),
        Some(Token::Ident(ident)) => ident,
        Some(token) => return Err(ParseError::UnexpectedToken(token)),
    };
    match lexer.next().transpose()? {
        None => return Err(ParseError::UnexpectedEof),
        Some(Token::Eq) => {}
        Some(Token::Semicolon) => return Ok(Statement::Var(ident, None)),
        Some(token) => return Err(ParseError::UnexpectedToken(token)),
    }
    let expr = parse_expression(lexer)?;
    Ok(Statement::Var(ident, Some(expr)))
}

pub fn parse_if_stmt(lexer: &mut Lexer) -> Result<IfStatement> {
    let condition = parse_expression(lexer)?;
    expect(Token::LBrace, lexer)?;
    let block = parse_block(lexer)?;
    match lexer.clone().next().transpose()? {
        Some(Token::KwElse) => {
            _ = lexer.next();
            let or_else = match lexer.next().transpose()? {
                Some(Token::LBrace) => OrElse::Else(parse_block(lexer)?),
                Some(Token::KwIf) => OrElse::ElseIf(Box::new(parse_if_stmt(lexer)?)),
                Some(token) => return Err(ParseError::UnexpectedToken(token)),
                None => return Err(ParseError::UnexpectedEof),
            };
            Ok(IfStatement { condition, block, or_else: Some(or_else) })
        }
        _ => Ok(IfStatement { condition, block, or_else: None }),
    }
}
pub fn parse_while_loop(lexer: &mut Lexer) -> Result<Statement> {
    let condition = parse_expression(lexer)?;
    expect(Token::LBrace, lexer)?;
    let body = parse_block(lexer)?;
    Ok(Statement::While(condition, body))
}

pub fn parse_for_loop(lexer: &mut Lexer) -> Result<Statement> {
    expect(Token::LParen, lexer)?;
    let init = parse_statement(lexer)?;
    expect(Token::Semicolon, lexer)?;
    let condition = parse_expression(lexer)?;
    expect(Token::Semicolon, lexer)?;
    let counter = parse_statement(lexer)?;
    expect(Token::RParen, lexer)?;
    expect(Token::LBrace, lexer)?;
    let body = parse_block(lexer)?;
    Ok(Statement::For { init: Box::new(init), condition, counter: Box::new(counter), body })
}

fn expect(expected: Token, lexer: &mut Lexer) -> Result<()> {
    match lexer.next().transpose()? {
        None => Err(ParseError::UnexpectedEof),
        Some(token) if token == expected => Ok(()),
        Some(token) => Err(ParseError::UnexpectedToken(token)),
    }
}

pub enum Node {
    Expression(Expression),
    Statement(Statement),
}

pub enum Expression {
    Literal(Literal),
    Identifier(Ident),
    FunCall { expr: Box<Expression>, args: ExpressionList },
    UnaryExpr { operator: UnaryOp, operand: Box<Expression> },
    BinaryExpr { operator: BinaryOp, operands: Box<[Expression; 2]> },
}

pub enum Statement {
    Assignment(Box<[Ustr]>, Expression),
    Lone(Expression),
    Var(Ident, Option<Expression>),
    Return(Expression),
    Break(Expression),
    Print(Expression),
    While(Expression, Block),
    For { init: Box<Statement>, condition: Expression, counter: Box<Statement>, body: Block },
    FunDef(FunDefinition),
    If(IfStatement),
    Block(Block),
}

impl From<Literal> for Expression {
    fn from(value: Literal) -> Self {
        Self::Literal(value)
    }
}

#[derive(Debug)]
pub struct FunDefinition {
    pub name: Ustr,
    pub arguments: Box<[Ident]>,
    pub body: Block,
}

#[derive(Debug)]
pub struct IfStatement {
    pub condition: Expression,
    pub block: Block,
    pub or_else: Option<OrElse>,
}

#[derive(Debug)]
pub enum OrElse {
    ElseIf(Box<IfStatement>),
    Else(Block),
}

#[derive(Debug)]
pub enum Literal {
    Number(f64),
    String(Ustr),
    Boolean(bool),
    Nil,
}

#[derive(Debug)]
pub enum UnaryOp {
    Negative,
    Not,
}

#[derive(Debug, PartialEq)]
pub enum BinaryOp {
    IsEq,
    NotEq,
    Less,
    LessEq,
    Greater,
    GreaterEq,
    Plus,
    Minus,
    Multiply,
    Divide,
    And,
    Or,
    FieldAcess,
}

impl TryFrom<Token> for BinaryOp {
    type Error = ();
    fn try_from(token: Token) -> std::result::Result<Self, Self::Error> {
        Ok(match token {
            Token::EqEq => Self::IsEq,
            Token::BangEq => Self::NotEq,
            Token::Less => Self::Less,
            Token::Greater => Self::Greater,
            Token::LessEq => Self::LessEq,
            Token::GreaterEq => Self::GreaterEq,
            Token::Plus => Self::Plus,
            Token::Minus => Self::Minus,
            Token::Star => Self::Multiply,
            Token::Slash => Self::Divide,
            Token::KwAnd => Self::And,
            Token::KwOr => Self::Or,
            _ => return Err(()),
        })
    }
}

impl fmt::Debug for Node {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Expression(expr) => fmt::Debug::fmt(expr, f),
            Self::Statement(stmt) => fmt::Debug::fmt(stmt, f),
        }
    }
}

impl fmt::Debug for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Literal(literal) => fmt::Display::fmt(literal, f),
            Self::Identifier(ident) => write!(f, "IDENT {ident}"),
            Self::FunCall { expr, args } => f
                .debug_struct("FunctionCall")
                .field("Expr", expr)
                .field("arguments", &args)
                .finish(),
            Self::UnaryExpr { operator, operand } => {
                f.debug_tuple(&format!("{operator:?}")).field(operand).finish()
            }
            Self::BinaryExpr { operator, operands } => f
                .debug_struct(&format!("{operator:?}"))
                .field("lhs", &operands[0])
                .field("rhs", &operands[1])
                .finish(),
        }
    }
}

impl fmt::Debug for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Lone(expr) => fmt::Debug::fmt(expr, f),
            Self::Assignment(lhs, expr) => {
                let mut var = String::from(lhs[0].as_str());
                for ident in &lhs[1..] {
                    var.push('.');
                    var.push_str(ident);
                }
                f.debug_struct("Assign").field("var", &var).field("expr", expr).finish()
            }
            Self::Var(ident, expr) => {
                let mut var = f.debug_tuple("Var");
                var.field(&ident.as_str());
                if let Some(expr) = expr {
                    var.field(expr);
                }
                var.finish()
            }
            Self::Return(expr) => f.debug_tuple("Return").field(expr).finish(),
            Self::Break(expr) => f.debug_tuple("Break").field(expr).finish(),
            Self::Print(expr) => f.debug_tuple("Print").field(expr).finish(),
            Self::Block(block) => f.debug_list().entries(block).finish(),
            Self::FunDef(fun) => fmt::Debug::fmt(fun, f),
            Self::If(if_stmt) => fmt::Debug::fmt(if_stmt, f),
            Self::While(condition, block) => {
                f.debug_struct("While").field("condition", condition).field("body", block).finish()
            }

            Self::For { init, condition, counter, body } => f
                .debug_struct("For")
                .field("init", &init)
                .field("condition", &condition)
                .field("counter", &counter)
                .field("body", &body)
                .finish(),
        }
    }
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Boolean(true) => write!(f, "TRUE"),
            Self::Boolean(false) => write!(f, "FALSE"),
            Self::Nil => write!(f, "NIL"),
            Self::Number(number) => write!(f, "NUMBER {number}"),
            Self::String(string) => write!(f, "STRING {:?}", string.as_str()),
        }
    }
}

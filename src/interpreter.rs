use ustr::Ustr;

use crate::parser::{Block, Expression, Literal, Node, Statement};

pub struct Interpreter;

impl Interpreter {
    pub fn execute(&mut self, node: &Node) -> Value {
        match node {
            Node::Expression(expr) => self.exec_expression(expr),
            Node::Statement(stmt) => self.exec_statement(stmt),
        }
    }
    fn exec_statement(&mut self, stmt: &Statement) -> Value {
        match stmt {
            Statement::Block(block) => self.exec_block(block),
            Statement::Print(expr) => self.print(expr),
            Statement::Lone(expr) => return self.exec_expression(expr),
            stmt => todo!("{stmt:?}"),
        }
        Value::Nil
    }
    fn exec_block(&mut self, block: &Block) {
        for stmt in block {
            self.exec_statement(stmt);
        }
    }
    fn exec_expression(&mut self, expr: &Expression) -> Value {
        match expr {
            Expression::Literal(literal) => Value::from(literal.clone()),
            _ => todo!(),
        }
    }
    fn print(&mut self, expr: &Expression) {
        let value = self.exec_expression(expr);
        match value {
            Value::Nil => println!("nil"),
            Value::Boolean(bool) => println!("{bool}"),
            Value::Number(number) => println!("{number}"),
            Value::String(string) => println!("{string}"),
        }
    }
}

pub enum Value {
    Nil,
    Boolean(bool),
    Number(f64),
    String(Ustr),
}

impl From<Literal> for Value {
    fn from(literal: Literal) -> Self {
        match literal {
            Literal::Nil => Self::Nil,
            Literal::Boolean(bool) => Self::Boolean(bool),
            Literal::Number(number) => Self::Number(number),
            Literal::String(string) => Self::String(string),
        }
    }
}

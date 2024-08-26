use ustr::Ustr;

use crate::parser::{BinaryOp, Block, Expression, Literal, Node, Statement, UnaryOp};

#[derive(Default)]
pub struct Interpreter {}

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
            Expression::BinaryExpr { operator, operands } => {
                self.exec_bin_expr(*operator, operands)
            }
            Expression::UnaryExpr { operator: UnaryOp::Negative, operand } => {
                let Value::Number(number) = self.exec_expression(operand) else { todo!() };
                Value::Number(-number)
            }
            Expression::UnaryExpr { operator: UnaryOp::Not, operand } => {
                let Ok(bool) = bool::try_from(self.exec_expression(operand)) else { todo!() };
                Value::Boolean(!bool)
            }
            expr => todo!("{expr:?}"),
        }
    }
    fn exec_bin_expr(&mut self, operator: BinaryOp, [lhs, rhs]: &[Expression; 2]) -> Value {
        use BinaryOp as Op;
        match operator {
            Op::Plus
            | Op::Minus
            | Op::Multiply
            | Op::Divide
            | Op::Less
            | Op::Greater
            | Op::LessEq
            | Op::GreaterEq => {
                let lhs = self.exec_expression(lhs);
                let rhs = self.exec_expression(rhs);

                let Value::Number(lhs) = lhs else { todo!("{lhs:?}") };
                let Value::Number(rhs) = rhs else { todo!("{rhs:?}") };

                match operator {
                    Op::Plus => Value::Number(lhs + rhs),
                    Op::Minus => Value::Number(lhs - rhs),
                    Op::Multiply => Value::Number(lhs * rhs),
                    Op::Divide => Value::Number(lhs / rhs),
                    Op::Less => Value::Boolean(lhs < rhs),
                    Op::Greater => Value::Boolean(lhs > rhs),
                    Op::LessEq => Value::Boolean(lhs <= rhs),
                    Op::GreaterEq => Value::Boolean(lhs >= rhs),
                    _ => unreachable!(),
                }
            }
            Op::IsEq | Op::NotEq => {
                let lhs = self.exec_expression(lhs);
                let rhs = self.exec_expression(rhs);

                let is_eq = match (lhs, rhs) {
                    (Value::Nil, Value::Nil) => true,
                    (Value::Boolean(lhs), Value::Boolean(rhs)) => lhs == rhs,
                    (Value::Number(lhs), Value::Number(rhs)) => lhs == rhs,
                    (Value::String(lhs), Value::String(rhs)) => lhs == rhs,
                    _ => false,
                };
                Value::Boolean(if operator == Op::IsEq { is_eq } else { !is_eq })
            }
            Op::And | Op::Or => {
                let Ok(lhs) = bool::try_from(self.exec_expression(lhs)) else { panic!() };

                match (lhs, operator) {
                    (true, Op::Or) => Value::Boolean(true),
                    (false, Op::And) => Value::Boolean(false),
                    (false, Op::Or) | (true, Op::And) => {
                        let Value::Boolean(rhs) = self.exec_expression(rhs) else { todo!() };
                        Value::Boolean(rhs)
                    }
                    _ => unreachable!(),
                }
            }
            Op::FieldAcess => todo!("Field access"),
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

#[derive(Debug, Clone)]
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

impl TryFrom<Value> for bool {
    type Error = ();
    fn try_from(value: Value) -> Result<Self, Self::Error> {
        Ok(match value {
            Value::Boolean(bool) => bool,
            Value::Nil => false,
            _ => return Err(()),
        })
    }
}

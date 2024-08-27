use core::fmt;
use std::rc::Rc;

use ustr::{Ustr, UstrMap};

use crate::parser::{
    BinaryOp, Block, Expression, FunDefinition, Literal, Node, Statement, UnaryOp,
};

#[allow(unused)]
enum ControlFlow {
    Continue,
    Break,
    Return(Value),
}

#[derive(Default)]
pub struct Interpreter {
    control_flow: Option<ControlFlow>,
    variables: UstrMap<Value>,
}

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
            Statement::Var(name, expr) => {
                let value = match expr {
                    Some(expr) => self.exec_expression(expr),
                    None => Value::Nil,
                };
                self.variables.insert(*name, value);
            }
            Statement::Assignment(name, expr) => {
                let value = self.exec_expression(expr);
                let name = name[0];
                self.variables.insert(name, value);
            }
            Statement::FunDef(def) => {
                self.variables.insert(def.name, Value::Function(def.clone()));
            }
            Statement::Return(expr) => {
                let value = self.exec_expression(expr);
                self.control_flow = Some(ControlFlow::Return(value));
            }
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
            Expression::Identifier(ident) => self.variables.get(ident).expect(ident).clone(),
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
            Expression::FunCall { fun, args } => {
                let fun = match self.exec_expression(fun) {
                    Value::Function(fun) => fun,
                    value => panic!("Expected function got {value:?}"),
                };
                let mut arg_values = Vec::with_capacity(args.len());
                for arg in args {
                    arg_values.push(self.exec_expression(arg));
                }
                for (&arg, value) in fun.arguments.iter().zip(arg_values) {
                    self.variables.insert(arg, value);
                }
                self.exec_block(&fun.body);
                match self.control_flow.take() {
                    Some(ControlFlow::Break | ControlFlow::Continue) => todo!(),
                    Some(ControlFlow::Return(value)) => value,
                    None => Value::Nil,
                }
            }
            expr => todo!("{expr:?}"),
        }
    }
    fn exec_bin_expr(&mut self, operator: BinaryOp, [lhs, rhs]: &[Expression; 2]) -> Value {
        use BinaryOp as Op;
        match operator {
            Op::Plus => {
                let lhs = self.exec_expression(lhs);
                let rhs = self.exec_expression(rhs);
                match (lhs, rhs) {
                    (Value::Number(lhs), Value::Number(rhs)) => Value::Number(lhs + rhs),
                    (Value::String(lhs), Value::String(rhs)) => {
                        Value::String((lhs.to_owned() + &rhs).into())
                    }
                    _ => panic!(),
                }
            }
            Op::Minus
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
        println!("{}", DisplayValue(self, value));
    }
}

#[allow(unused)]
struct DisplayValue<'a>(&'a Interpreter, Value);
impl fmt::Display for DisplayValue<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.1 {
            Value::Nil => write!(f, "nil"),
            Value::Boolean(bool) => write!(f, "{bool}"),
            Value::Number(number) => write!(f, "{number}"),
            Value::String(string) => write!(f, "{string}"),
            // Print functions like python does for now
            Value::Function(function) => {
                let ptr = std::ptr::from_ref(function.as_ref());
                write!(f, r#"<function {} at {ptr:#?}>)"#, function.name,)
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum Value {
    Nil,
    Boolean(bool),
    Number(f64),
    String(Ustr),
    Function(Rc<FunDefinition>),
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

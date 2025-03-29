use crate::{
    lexer::token::{Literal, TokenType},
    parser::expr::Expression,
};

pub fn visit(expr: Expression) -> String {
    let mut printout = String::new();
    match expr {
        Expression::Literal(lit) => match lit {
            Literal::Nil => printout.push_str("nil"),
            Literal::Number(x) => printout.push_str(&x.to_string()),
            Literal::String(x) => printout.push_str(&x),
            Literal::Boolean(x) => printout.push_str(&x.to_string()),
        },
        Expression::Binary(l, o, r) => {
            printout.push('(');
            printout.push_str(&repr_token_type(o));
            printout.push(' ');
            printout.push_str(&visit(*l));
            printout.push(' ');
            printout.push_str(&visit(*r));
            printout.push(')');
        }
        Expression::Grouping(e) => {
            printout.push('(');
            printout.push_str("group ");
            printout.push_str(&visit(*e));
            printout.push(')');
        }
        Expression::Unary(o, r) => {
            printout.push('(');
            printout.push_str(&repr_token_type(o));
            printout.push(' ');
            printout.push_str(&visit(*r));
            printout.push(')');
        }
    }
    printout
}

fn repr_token_type(tt: TokenType) -> String {
    let s = match tt {
        TokenType::Bang => "!",
        TokenType::Eq => "==",
        TokenType::Neq => "!=",
        TokenType::Gt => ">",
        TokenType::Geq => ">=",
        TokenType::Lt => "<",
        TokenType::Leq => "<=",
        TokenType::Plus => "+",
        TokenType::Minus => "-",
        TokenType::Star => "*",
        TokenType::Slash => "/",
        TokenType::Assign => "=",
        _ => "",
    };
    String::from(s)
}

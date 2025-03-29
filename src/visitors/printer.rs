use crate::{lexer::token::Literal, parser::expr::Expression};

pub fn visit(expr: Expression) -> String {
    let mut printout = String::new();
    match expr {
        Expression::Literal(token) => match token.get_literal() {
            Literal::Nil => printout.push_str("nil"),
            Literal::Number(x) => printout.push_str(&x.to_string()),
            Literal::String(x) => printout.push_str(&x),
            Literal::Boolean(x) => printout.push_str(&x.to_string()),
        },
        Expression::Binary(left_expr, op_token, right_expr) => {
            printout.push('(');
            printout.push_str(op_token.get_lexeme());
            printout.push(' ');
            printout.push_str(&visit(*left_expr));
            printout.push(' ');
            printout.push_str(&visit(*right_expr));
            printout.push(')');
        }
        Expression::Grouping(expr) => {
            printout.push('(');
            printout.push_str("group ");
            printout.push_str(&visit(*expr));
            printout.push(')');
        }
        Expression::Unary(op_token, right_expr) => {
            printout.push('(');
            printout.push_str(op_token.get_lexeme());
            printout.push(' ');
            printout.push_str(&visit(*right_expr));
            printout.push(')');
        }
    }
    printout
}

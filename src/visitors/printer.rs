// use crate::parser::expr::Expression;

// pub struct ASTPrinter {}

// impl ASTPrinter {
//     pub fn new() -> Self {
//         Self {}
//     }

//     pub fn repr(&self, expr: &Expression) -> String {
//         let mut ast = String::new();
//         match expr {
//             Expression::Literal(token) => ast.push_str(&format!("{}", token.get_literal())),
//             Expression::Binary(left_expr, op_token, right_expr) => {
//                 ast.push_str(&format!(
//                     "({} {} {})",
//                     op_token.get_lexeme(),
//                     self.repr(left_expr),
//                     self.repr(right_expr)
//                 ));
//             }
//             Expression::Grouping(expr) => {
//                 ast.push_str(&format!("(group {})", self.repr(expr)));
//             }
//             Expression::Unary(op_token, right_expr) => {
//                 ast.push_str(&format!("({} {})", op_token.get_lexeme(), self.repr(right_expr)));
//             }
//         }

//         ast
//     }
// }

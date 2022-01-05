use std::hint::unreachable_unchecked;

use super::*;
use crate::token::{Token, TokenKind as TK};

/// Generates a recursive descent parser and parses the tokens.
/// LOWEST TO HIGHEST PRECEDENCE
/// Syntax:
///     <TOKENS> =>
///         <RULE NAME> (, BIN/UN + LTR/RTL)? = @ <PATTERN> => <PARSING BLOCK>, <PATTERN> => <PARSING BLOCK>, ...;
///         <RULE NAME> (, BIN/UN + LTR/RTL)? = <PATTERN> => <PARSING BLOCK>;
///         .
///         .
///         .
///
///     TOKENS - Token stream to parse.
///
///     RULE NAME - Identifier for the parser function.
///
///     BIN/UN + LTR/RTL - Specifies whether the rule is binary or unary and its associativity.
///
///     PATTERN - Required pattern for rule to execute.
///     
///     PARSING BLOCK - Closure which returns Result<Expr>
///     @ - Whether the closure accepts tokens
macro_rules! def_expr {
    ($tokens:expr => $(
        $rule_name:ident = $($pattern:pat => $parser:expr),+
    );+ $(;)?) => {
        def_expr! { @ $tokens; $(
            $rule_name = $($pattern => $parser),+
        );+ }
    };

    // Implementation

    // Main rule which calls more specific rules to define the functions and then calls the first one.
    // Public rule accepts at least one or more definition so we can be sure this will get called.
    (@ $tokens:expr;
        $rule:ident = $($pattern:pat => $parser:expr),+
        $(; $rule_r:ident = $($pattern_r:pat => $parser_r:expr),+ )*
    ) => {
        def_expr! { @@
            $rule = $($pattern => $parser),+
            $(; $rule_r = $($pattern_r => $parser_r),+ )*
        }

        $rule ( $tokens )
    };

    // In case only one definition is provided, we don't need to do any recursion.
    // Just generate that single function.
    (@@ $rule:ident = $($pattern:pat => $parser:expr),+) => {
        def_expr! { @funcdef $rule = $($pattern => $parser),+ }
    };

    // Two or more rules provided.
    (@@
        $first:ident = $($pattern_f:pat => $parser_f:expr),+ ;
        $($rule:ident = $($pattern:pat => $parser:expr),+ ;)*
        $last:ident = $($pattern_l:pat => $parser_l:expr),+
    ) => {
        compile_error!("Not implemented yet")
    };

    (@funcdef $ident:ident = $($pattern:pat => $parser:expr),+) => {
        fn $ident(tokens: &mut Stream) -> Result<Expr> {
            let tok = tokens
                .peek()
                .ok_or_else(|| eof("expression"))?
                .as_ref()
                .map_err(|err| ParseError::from(err.clone()))?;

            match &**tok {
                $(
                    #[allow(clippy::redundant_closure_call)]
                    $pattern => Ok({ ( $parser )(tokens)? }),
                )+
                _ => Err(ut(tokens.next().unwrap()?, "expression"))
            }
        }
    };
}

impl Expr {
	pub fn parse(tokens: &mut Stream) -> Result<Expr> {
		def_expr! { tokens =>
			primary =
				TK::Lit(_) => parse_lit,
				TK::Ident(_) => |_| { todo!(); Err(eof("TODO")) },
				TK::LBracket => |_| { todo!(); Err(eof("TODO")) } // BlockExpr::parse
		}
	}

	pub fn requires_semi(&self) -> bool {
		match &**self {
			ExprDef::Block(_) => false,
			_ => true,
		}
	}
}

fn parse_lit(tokens: &mut Stream) -> Result<Expr> {
	match tokens.next() {
		Some(Ok(Token {
			val: TK::Lit(lit),
			start,
			end,
			path,
		})) => Ok(ExprDef::Lit(lit).span(start, end, path)),
		_ => unsafe { unreachable_unchecked() },
	}
}

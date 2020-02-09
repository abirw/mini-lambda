/* Compiler Construction - Minimal Lambda Language
 *
 * This file defined the grammar of the language, with
 * the productions builtin the Abstract Syntax Tree.
 */

%{
open Ast
%}

%token <int> INT
%token <string> IDENT
%token <bool> BOOL
%token PLUS
%token MINUS
%token EQUALS
%token NEQ
%token AND
%token OR
%token LPAREN RPAREN LBRACE RBRACE
%token FUNC
%token RETURN
%token ARROW
%token LAMBDA
%token BIND
%token COMMA
%token SEMI
%token EOF

%start program
%type <Ast.program> program

%%

program:
  funcs = list(func) EOF { Array.of_list funcs }


func:
  | FUNC name = IDENT;
    LPAREN params = separated_list(COMMA, IDENT); RPAREN
    body = func_body
    { { name; params; body; loc = $startpos } }

func_body:
  | LBRACE body = statements; RBRACE { Some(body) }
  | SEMI { None }

statements:
  | statement statements { $1 :: $2 }
  | { [] }

statement:
  | RETURN expr SEMI { ReturnStmt($startpos, $2) }
  | IDENT BIND expr SEMI { BindStmt($startpos, $1, $3) }
  | expr SEMI { ExprStmt($startpos, $1) }

expr:
  | unary_expr { $1 }
  | lhs = expr; PLUS; rhs = unary_expr
    { AddExpr($startpos, lhs, rhs) }
  | lhs = expr; MINUS; rhs = unary_expr
    { MinusExpr($startpos, lhs, rhs) }
  | lhs = expr; EQUALS; rhs = unary_expr
    { EqualsExpr($startpos, lhs, rhs) }
  | lhs = expr; NEQ; rhs = unary_expr
    { NeqExpr($startpos, lhs, rhs) }
  | lhs = expr; AND; rhs = unary_expr
    { NeqExpr($startpos, lhs, rhs) }
  | lhs = expr; OR; rhs = unary_expr
    { NeqExpr($startpos, lhs, rhs) }

unary_expr:
  | LAMBDA
    LPAREN params = separated_list(COMMA, IDENT); RPAREN
    ARROW
    body = postfix_expr;
    { LambdaExpr($startpos, params, body) }
  | postfix_expr { $1 }

postfix_expr:
  | primary_expr { $1 }
  | callee = primary_expr; LPAREN args = separated_list(COMMA, expr); RPAREN
    { CallExpr($startpos, callee, args) }

primary_expr:
  | LPAREN e = expr; RPAREN { e }
  | name = IDENT { IdentExpr($startpos, name) }
  | decimal = INT { IntExpr($startpos, decimal) }
  | boolean = BOOL { BoolExpr($startpos, boolean) }



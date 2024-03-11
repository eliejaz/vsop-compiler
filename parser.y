%skeleton "lalr1.cc"
%language "c++"
%require "3.7.5"
%locations

%defines

%define api.namespace {VSOP}
%define api.parser.class {Parser}
%define api.token.raw
%define api.value.type variant
%define api.token.constructor

%define parse.assert

// C++ code put inside header file
%code requires {
    #include "ASTClasses.cpp"
    #include <string>
    #include <memory>
    #include <vector>
    #include <tuple>
    namespace VSOP {
        class Driver;
    }
}

%parse-param {VSOP::Driver &driver}

%code {
    #include "driver.hpp"
    using namespace std;
}

// Token and symbols definitions
%token
    AND "and"
    BOOL "bool"
    CLASS "class"
    DO "do"
    ELSE "else"
    EXTENDS "extends"
    FALSE "false"
    IF "if"
    IN "in"
    INT32 "int32"
    ISNULL "isnull"
    LET "let"
    NEW "new"
    NOT "not"
    SELF "self"
    STRING "string"
    THEN "then"
    TRUE "true"
    UNIT "unit"
    WHILE "while"

    LBRACE "{"
    RBRACE "}"
    LPAR "("
    RPAR ")"
    COLON ":"
    SEMICOLON ";"
    COMMA ","
    PLUS "+"
    MINUS "-"
    TIMES "*"
    DIV "/"
    POW "^"
    DOT "."
    EQUAL "="
    LOWER "<"
    LOWEREQUAL "<="
    ASSIGN "<-"
;

// For some symbols, need to store a value
%token <std::string> TYPEIDENTIFIER "type_identifier"
%token <std::string> OBJECTIDENTIFIER "object_identifier"
%token <std::string> STRINGLITERAL "string_literal"
%token <int> INTEGERLITERAL "integer_literal"

%type <Expression*> expression
%type <Block*> block
%type <Program*> program
%type <Class*> class_declaration
%type <Method*> method_declaration 
%type <Field*> field_declaration
%type<std::tuple<std::vector<Field*>, std::vector<Method*>>> class_members
%type <Formal*> formal_declaration
%type <std::vector<Class*>> classes
%type <std::vector<Expression*>> expressions
%type <std::vector<Expression*>> func_arguments
%type <std::vector<Formal*>> formals
%type <std::string> all_types


// Precedence
%precedence IF LET IN THEN WHILE DO
%precedence ELSE //higher precedence to asociate the else part with the closest preceding if

%right ASSIGN
%left OR
%left AND
%right NOT
%nonassoc EQUAL LOWER LOWEREQUAL
%left PLUS MINUS
%left TIMES DIV
%right ISNULL
%right POW
%left DOT

%%
// Grammar rules

%start program;

program:
    classes { driver.result = new Program($1); }
    ;

classes:
         { $$ = std::vector<Class*>();}
      | classes class_declaration { ($1).push_back($2); $$ = $1 ;}
    ;


class_declaration:
    CLASS all_types LBRACE class_members RBRACE { $$ = new Class($2, std::get<0>($4), std::get<1>($4)); }
    | CLASS all_types EXTENDS all_types LBRACE class_members RBRACE { $$ = new Class($2, std::get<0>($6), std::get<1>($6), $4); }
    ;

class_members:
        {$$ = std::tuple<std::vector<Field*>, std::vector<Method*>>{}; }
    | class_members field_declaration   { std::get<0>($1).push_back($2); $$ = $1; }
    | class_members method_declaration  { std::get<1>($1).push_back($2); $$ = $1;}
    ;

field_declaration:
    OBJECTIDENTIFIER COLON all_types SEMICOLON { $$ = new Field($1, $3); }
    | OBJECTIDENTIFIER COLON all_types ASSIGN expression SEMICOLON { $$ = new Field($1, $3, $5); }
    ;

method_declaration:
    OBJECTIDENTIFIER LPAR formals RPAR COLON all_types block { $$ = new Method($1, $3, $6, $7); }
    ;

formals:
                                       { $$ = std::vector<Formal*>(); }
    | formal_declaration               { $$ = std::vector<Formal*>(); ($$).push_back($1); }
    | formals COMMA formal_declaration { ($1).push_back($3); $$ = $1; }   
    ;

formal_declaration:
    OBJECTIDENTIFIER COLON all_types { $$ = new Formal($1, $3); }
    ;

block:
    LBRACE expressions RBRACE { $$ = new Block($2); }
    ;

expressions:
                                       { $$ = std::vector<Expression*>(); }
    | expression                       {  $$ = std::vector<Expression*>(); $$.push_back($1); }
    | expressions SEMICOLON expression { ($1).push_back($3); $$ = $1; }

    ;

expression:
     block                                { $$ = $1; }
    | LPAR expression RPAR                { $$ = $2; }
    | INTEGERLITERAL                      { $$ = new IntegerLiteral($1); }
    | STRINGLITERAL                       { $$ = new StringLiteral($1); }
    | TRUE                                { $$ = new BooleanLiteral(true); }
    | FALSE                               { $$ = new BooleanLiteral(false); }
    | OBJECTIDENTIFIER                    { $$ = new ObjectId($1); }
    | SELF                                { $$ = new ObjectId("self"); }
    | expression PLUS expression          { $$ = new BinaryOp("+", $1, $3); }
    | expression MINUS expression         { $$ = new BinaryOp("-", $1, $3); }
    | expression TIMES expression         { $$ = new BinaryOp("*", $1, $3); }
    | expression DIV expression           { $$ = new BinaryOp("/", $1, $3); }
    | expression POW expression           { $$ = new BinaryOp("^", $1, $3); }
    | expression EQUAL expression         { $$ = new BinaryOp("=", $1, $3); }
    | expression LOWER expression         { $$ = new BinaryOp("<", $1, $3); }
    | expression LOWEREQUAL expression    { $$ = new BinaryOp("<=", $1, $3); }
    | expression AND expression           { $$ = new BinaryOp("and", $1, $3); }
    | NOT expression                      { $$ = new UnaryOp("not", $2); }
    | MINUS expression  { $$ = new UnaryOp("-", $2); }
    | ISNULL expression                   { $$ = new UnaryOp("isnull", $2); }
    | OBJECTIDENTIFIER ASSIGN expression  { $$ = new Assign($1, $3); }
    | IF expression THEN expression ELSE expression { $$ = new If($2, $4, $6); }
    | IF expression THEN expression                 { $$ = new If($2, $4); }
    | WHILE expression DO expression                { $$ = new While($2, $4); }
    | LET OBJECTIDENTIFIER COLON all_types IN expression { $$ = new Let($2, $4, $6); }
    | LET OBJECTIDENTIFIER COLON all_types ASSIGN expression IN expression { $$ = new Let($2, $4, $8, $6); }
    | NEW all_types { $$ = new New($2); }
    | OBJECTIDENTIFIER LPAR func_arguments RPAR { $$ = new Call($1, $3); }
    | expression DOT OBJECTIDENTIFIER LPAR func_arguments RPAR { $$ = new Call($3, $5, $1); }
        ;

func_arguments:
                            { $$ = std::vector<Expression*>(); }
    | expression            {  $$ = std::vector<Expression*>(); $$.push_back($1); }
    | func_arguments COMMA expression { ($1).push_back($3); $$ = $1; }
    ;

all_types: 
    TYPEIDENTIFIER  { $$ = $1; }
    | INT32         { $$ = "int32"; }
    | BOOL          { $$ = "bool"; }
    | STRING        { $$ = "string"; }
    | UNIT          { $$ = "unit"; }
    ;

%%

void VSOP::Parser::error(const location_type& l, const std::string& m)
{
    const position &pos = l.begin;

    cerr << *(pos.filename) << ":"
         << pos.line << ":" 
         << pos.column << ": "
         << m
         << endl;
}

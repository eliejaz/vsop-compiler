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
%define parse.error detailed
%define parse.lac full


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
    void setAstNodePostition(ASTNode* node, const VSOP::Parser::location_type & l);
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
%type <Type*> all_types

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
    classes { driver.result = new Program($1); setAstNodePostition(driver.result, yyla.location); }
    ;

classes:
         { $$ = std::vector<Class*>();}
      | classes class_declaration { ($1).push_back($2); $$ = $1 ;}
    ;


class_declaration:
    CLASS TYPEIDENTIFIER LBRACE class_members RBRACE { $$ = new Class($2, std::get<0>($4), std::get<1>($4)); setAstNodePostition($$, yyla.location); }
    | CLASS TYPEIDENTIFIER EXTENDS TYPEIDENTIFIER LBRACE class_members RBRACE { $$ = new Class($2, std::get<0>($6), std::get<1>($6), $4); setAstNodePostition($$, yyla.location); }
    ;

class_members:
        {$$ = std::tuple<std::vector<Field*>, std::vector<Method*>>{}; }
    | class_members field_declaration   { std::get<0>($1).push_back($2); $$ = $1; }
    | class_members method_declaration  { std::get<1>($1).push_back($2); $$ = $1;}
    ;

field_declaration:
    OBJECTIDENTIFIER COLON all_types SEMICOLON { $$ = new Field($1, $3); setAstNodePostition($$, yyla.location); }
    | OBJECTIDENTIFIER COLON all_types ASSIGN expression SEMICOLON { $$ = new Field($1, $3, $5); setAstNodePostition($$, yyla.location); }
    ;

method_declaration:
    OBJECTIDENTIFIER LPAR formals RPAR COLON all_types block { $$ = new Method($1, $3, $6, $7); setAstNodePostition($$, yyla.location); }
    ;

formals:
                                       { $$ = std::vector<Formal*>(); }
    | formal_declaration               { $$ = std::vector<Formal*>(); ($$).push_back($1); }
    | formals COMMA formal_declaration { ($1).push_back($3); $$ = $1; }   
    ;

formal_declaration:
    OBJECTIDENTIFIER COLON all_types { $$ = new Formal($1, $3); setAstNodePostition($$, yyla.location); }
    ;

block:
    LBRACE RBRACE {
        std::string errorMessage = "Blocks should contain at least one statement or expression.";
        error(yyla.location, YY_MOVE(errorMessage));
        YYERROR;
    }
    | LBRACE expressions RBRACE { $$ = new Block($2); setAstNodePostition($$, yyla.location);}
    | LBRACE expressions {
        std::string errorMessage = "A closing '}' is missing.";
        error(yyla.location, YY_MOVE(errorMessage));
        YYERROR;
    }
    ;
expressions:
                                       { $$ = std::vector<Expression*>(); }
    | expression                       {  $$ = std::vector<Expression*>(); $$.push_back($1);}
    | expressions SEMICOLON expression { ($1).push_back($3); $$ = $1;}

    ;

expression:
     block                                { $$ = $1; }
    | LPAR expression RPAR                { $$ = $2; }
    | INTEGERLITERAL                      { $$ = new IntegerLiteral($1);setAstNodePostition($$, yyla.location); }
    | STRINGLITERAL                       { $$ = new StringLiteral($1);setAstNodePostition($$, yyla.location); }
    | TRUE                                { $$ = new BooleanLiteral(true);setAstNodePostition($$, yyla.location); }
    | FALSE                               { $$ = new BooleanLiteral(false);setAstNodePostition($$, yyla.location); }
    | OBJECTIDENTIFIER                    { $$ = new ObjectId($1);setAstNodePostition($$, yyla.location); }
    | SELF                                { $$ = new ObjectId();setAstNodePostition($$, yyla.location); }
    | expression PLUS expression          { $$ = new BinaryOp(BinaryOp::Op::Add, $1, $3);setAstNodePostition($$, yyla.location); }
    | expression MINUS expression         { $$ = new BinaryOp(BinaryOp::Op::Subtract, $1, $3);setAstNodePostition($$, yyla.location); }
    | expression TIMES expression         { $$ = new BinaryOp(BinaryOp::Op::Multiply, $1, $3);setAstNodePostition($$, yyla.location); }
    | expression DIV expression           { $$ = new BinaryOp(BinaryOp::Op::Divide, $1, $3);setAstNodePostition($$, yyla.location); }
    | expression POW expression           { $$ = new BinaryOp(BinaryOp::Op::Power, $1, $3);setAstNodePostition($$, yyla.location); }
    | expression EQUAL expression         { $$ = new BinaryOp(BinaryOp::Op::Equal, $1, $3);setAstNodePostition($$, yyla.location); }
    | expression LOWER expression         { $$ = new BinaryOp(BinaryOp::Op::LessThan, $1, $3);setAstNodePostition($$, yyla.location); }
    | expression LOWEREQUAL expression    { $$ = new BinaryOp(BinaryOp::Op::LessEqual, $1, $3);setAstNodePostition($$, yyla.location); }
    | expression AND expression           { $$ = new BinaryOp(BinaryOp::Op::And, $1, $3);setAstNodePostition($$, yyla.location); }
    | NOT expression                      { $$ = new UnaryOp(UnaryOp::Op::Not, $2);setAstNodePostition($$, yyla.location); }
    | MINUS expression                    { $$ = new UnaryOp(UnaryOp::Op::Negate, $2);setAstNodePostition($$, yyla.location); }
    | ISNULL expression                   { $$ = new UnaryOp(UnaryOp::Op::IsNull, $2);setAstNodePostition($$, yyla.location); }
    | OBJECTIDENTIFIER ASSIGN expression  { $$ = new Assign($1, $3);setAstNodePostition($$, yyla.location); }
    | IF expression THEN expression ELSE expression { $$ = new If($2, $4, $6);setAstNodePostition($$, yyla.location); }
    | IF expression THEN expression                 { $$ = new If($2, $4);setAstNodePostition($$, yyla.location); }
    | WHILE expression DO expression                { $$ = new While($2, $4);setAstNodePostition($$, yyla.location); }
    | LET OBJECTIDENTIFIER COLON all_types IN expression { $$ = new Let($2, $4, $6);setAstNodePostition($$, yyla.location); }
    | LET OBJECTIDENTIFIER COLON all_types ASSIGN expression IN expression { $$ = new Let($2, $4, $8, $6);setAstNodePostition($$, yyla.location); }
    | NEW TYPEIDENTIFIER { $$ = new New($2);setAstNodePostition($$, yyla.location); }
    | OBJECTIDENTIFIER LPAR func_arguments RPAR { $$ = new Call($1, $3);setAstNodePostition($$, yyla.location); }
    | expression DOT OBJECTIDENTIFIER LPAR func_arguments RPAR { $$ = new Call($3, $5, $1);setAstNodePostition($$, yyla.location); }
    ;

func_arguments:
                            { $$ = std::vector<Expression*>(); }
    | expression            {  $$ = std::vector<Expression*>(); $$.push_back($1); }
    | func_arguments COMMA expression { ($1).push_back($3); $$ = $1; }
    ;

all_types:
      TYPEIDENTIFIER  { $$ = new Type($1);setAstNodePostition($$, yyla.location); }
    | INT32           { $$ = new Type(Type::TypeName::Int32);setAstNodePostition($$, yyla.location); }
    | BOOL            { $$ = new Type(Type::TypeName::Bool);setAstNodePostition($$, yyla.location); }
    | STRING          { $$ = new Type(Type::TypeName::String);setAstNodePostition($$, yyla.location); }
    | UNIT            { $$ = new Type(Type::TypeName::Unit);setAstNodePostition($$, yyla.location); }
    ;

%%



void setAstNodePostition(ASTNode* node, const VSOP::Parser::location_type & l) {
    if (node) {
        node->setPosition(Position(*(l.begin.filename), l.begin.line, l.begin.column));
    }
    else{
        std::string errorMessage = "ERROR NODE: ";
        cerr
         << l.begin.line << ":" 
         << l.begin.column << ": " << errorMessage<< endl;;
    }
}

void VSOP::Parser::error(const location_type& l, const std::string& m)
{
    const position &pos = l.begin;
    
    cerr << *(pos.filename) << ":"
         << pos.line << ":" 
         << pos.column << ": "
         << m 
         << endl;
}




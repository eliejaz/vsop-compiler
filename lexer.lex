%{
    /* Includes */
    #include <string>

    #include "parser.hpp"
    #include "driver.hpp"
    #include <stack>

%}


%option noyywrap nounput noinput batch

%x COMMENT
%x STRING


%{
    /* Code to include at the beginning of the lexer file. */
    using namespace std;
    using namespace VSOP;

    // Create a new NUMBER token from the value s.
    Parser::symbol_type make_INTEGERLITERAL(const string &s,
                                    const location &loc);

    // Print an lexical error message.
    static void print_error(const position &pos,
                            const string &m);

    // Code run each time a pattern is matched.
    #define YY_USER_ACTION  loc.columns(yyleng);

    // Global variable used to maintain the current location.
    location loc;

    int comment_nesting = 0;
    stack<location>  commentPos;

    string string_buf;
    location stringPos;
%}

    /* Definitions */
uppercase    [A-Z]
lowercase    [a-z]
letter       [a-zA-Z]
digit [0-9]
hexadecimal {digit}|[a-fA-F]
integer ({digit}+|"0x"{hexadecimal}+)
typeId       {uppercase}[a-zA-Z0-9_]*
objectId     {lowercase}[a-zA-Z0-9_]*
notInteger [0-9]+[a-zA-A]+
notHexadecimal "0x"([a-zA-Z]|[0-9])+

id [a-zA-Z][a-zA-Z_0-9]*
blank [ \t\r\f]

%%
%{
    // Code run each time yylex is called.
    loc.step();
%}
    /* Rules */

    /* White spaces */
{blank}+    loc.step();
\n+         loc.lines(yyleng); loc.step();

    /* Numbers and identifiers */
{integer}   { return make_INTEGERLITERAL(yytext, loc); }

    /* Comments */
"//".*      { loc.lines(0);}

"(*"        { BEGIN(COMMENT); commentPos.push(loc); comment_nesting = 1; loc.step(); }
<COMMENT>{
    "*)"    { commentPos.pop(); comment_nesting--; if (comment_nesting == 0) { BEGIN(INITIAL); loc.step(); } }
    "(*"    {commentPos.push(loc); comment_nesting++; loc.step(); }
    .       { loc.step(); }
    \n+      { loc.lines(yyleng); loc.step(); }
    <<EOF>> {
        BEGIN(INITIAL); 
        print_error(commentPos.top().begin, "Unfinished comment at end of file"); 
        return Parser::make_YYerror(commentPos.top()); 
        }
}

    /* keywords */
"and"       { return Parser::make_AND(loc); }
"bool"      { return Parser::make_BOOL(loc); }
"class"     { return Parser::make_CLASS(loc); }
"do"        { return Parser::make_DO(loc); }
"else"      { return Parser::make_ELSE(loc); }
"extends"   { return Parser::make_EXTENDS(loc); }
"false"     { return Parser::make_FALSE(loc); }
"if"        { return Parser::make_IF(loc); }
"in"        { return Parser::make_IN(loc); }
"int32"     { return Parser::make_INT32(loc); }
"isnull"    { return Parser::make_ISNULL(loc); }
"let"       { return Parser::make_LET(loc); }
"new"       { return Parser::make_NEW(loc); }
"not"       { return Parser::make_NOT(loc); }
"self"      { return Parser::make_SELF(loc); }
"string"    { return Parser::make_STRING(loc); }
"then"      { return Parser::make_THEN(loc); }
"true"      { return Parser::make_TRUE(loc); }
"unit"      { return Parser::make_UNIT(loc); }
"while"     { return Parser::make_WHILE(loc); }

    /* Operators */
"{"         { return Parser::make_LBRACE(loc); }
"}"         { return Parser::make_RBRACE(loc); }
"("         { return Parser::make_LPAR(loc); }
")"         { return Parser::make_RPAR(loc); }
":"         { return Parser::make_COLON(loc); }
";"         { return Parser::make_SEMICOLON(loc); }
","         { return Parser::make_COMMA(loc); }
"+"         { return Parser::make_PLUS(loc); }
"-"         { return Parser::make_MINUS(loc); }
"*"         { return Parser::make_TIMES(loc); }
"/"         { return Parser::make_DIV(loc); }
"^"         { return Parser::make_POW(loc); }
"."         { return Parser::make_DOT(loc); }
"="         { return Parser::make_EQUAL(loc); }
"<"         { return Parser::make_LOWER(loc); }
"<="        { return Parser::make_LOWEREQUAL(loc); }
"<-"        { return Parser::make_ASSIGN(loc); }

    /* Type Identifiers */
{typeId} { return Parser::make_TYPEIDENTIFIER(yytext, loc); }

    /* Object Identifiers */
{objectId} { return Parser::make_OBJECTIDENTIFIER(yytext, loc); }

    
    /* String */
\"                  { BEGIN(STRING); stringPos = loc; string_buf = "\""; } //matches a double quote  "
<STRING>{
    [^\\\"\n]+         { string_buf += yytext; loc.step(); } // Matches one or more characters that are not a backslash, double quote, or newline
    \\n                { string_buf += "\\x0a"; loc.step(); } // Matches new line seq
    \\r                { string_buf += "\\x0d"; loc.step(); }
    \\t                { string_buf += "\\x09"; loc.step(); }
    \\b                { string_buf += "\\x08"; loc.step(); }
    \\\"               { string_buf += "\\x22"; loc.step(); }
    \\\\               { string_buf += "\\x5c"; loc.step(); }
    \\\n[ \t]*         { loc.lines(1); loc.columns(yyleng-2);}
    \\x[0-9a-fA-F]{2}  {  string_buf += yytext; loc.step(); } // matches escaped hexadecimal
    \"                 { string_buf += '\"'; BEGIN(INITIAL); return Parser::make_STRINGLITERAL(string_buf, stringPos); }
    
    \\[^btnr\"]    { print_error(loc.begin, "Invalid escape sequence: " + string(yytext)); return Parser::make_YYerror(loc); } // Matches an escaped character that is not one of the valid escape sequences
    \n                 { print_error(loc.begin, "String literal not terminated"); BEGIN(INITIAL); return Parser::make_YYerror(loc); }
    <<EOF>>            { print_error(stringPos.begin, "EOF encountered within string literal");  BEGIN(INITIAL); return Parser::make_YYerror(stringPos); }
}

{notInteger} {print_error(loc.begin, "Error parsing the integer.");  return Parser::make_YYerror(loc);}
{notHexadecimal} {print_error(loc.begin, "Error parsing the hexadecimal.");  return Parser::make_YYerror(loc);}
    /* Invalid characters */
.           {
                print_error(loc.begin, "invalid character: " + string(yytext));
                return Parser::make_YYerror(loc);
    }

    /* End of file */
<<EOF>>     return Parser::make_YYEOF(loc);
%%


Parser::symbol_type make_INTEGERLITERAL(const string &s, const location& loc) {
    int n = 0;
    // Check if the string represents a hexadecimal number
    if (s.size() > 2 && s[0] == '0' && (s[1] == 'x' || s[1] == 'X')) {
        n = stoi(s.substr(2), nullptr, 16);
    } else {
        n = stoi(s);
    }
    return Parser::make_INTEGERLITERAL(n, loc);
}


static void print_error(const position &pos, const string &m)
{
    cerr << *(pos.filename) << ":"
         << pos.line << ":"
         << pos.column << ":"
         << " lexical error: "
         << m
         << endl;
}

void Driver::scan_begin()
{
    loc.initialize(&source_file);

    if (source_file.empty() || source_file == "-")
        yyin = stdin;
    else if (!(yyin = fopen(source_file.c_str(), "r")))
    {
        cerr << "cannot open " << source_file << ": " << strerror(errno) << '\n';
        exit(EXIT_FAILURE);
    }
}

void Driver::scan_end()
{
    fclose(yyin);
}

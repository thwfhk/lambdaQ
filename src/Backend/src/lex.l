%{
    #include "parser.tab.h"
    #include "tree.h"
    // 这里parser.tab.h里面定义了各种token
    extern "C"
    {
        int yywrap(void);
        int yylex(void);
    }
%}

%%

"qreg" {
    yylval = createLeaf(QREG, yytext);
    return QREG;
}
"creg" {
    yylval = createLeaf(CREG, yytext);
    printf("creg found\n");
    return CREG;
}
"barrier" { 
    yylval = createLeaf(BARRIER, yytext);
    return BARRIER;
}
"gate" {
    yylval = createLeaf(GATE, yytext);
    return GATE;
}
"measure" {
    yylval = createLeaf(MEASURE, yytext);
    return MEASURE;
}
"reset" {
    yylval = createLeaf(RESET, yytext);
    return RESET;
}
"include" {
    yylval = createLeaf(INCLUDE, yytext);
    return INCLUDE;
}
"opaque" {
    yylval = createLeaf(OPAQUE, yytext);
    return OPAQUE;
}
"if" {
    yylval = createLeaf(IF, yytext);
    return IF;
}
"sin" {
    yylval = createLeaf(SIN, yytext);
    return SIN;
}
"cos" {
    yylval = createLeaf(COS, yytext);
    return COS;
}
"tan" {
    yylval = createLeaf(TAN, yytext);
    return TAN;
}
"exp" {
    yylval = createLeaf(EXP, yytext);
    return EXP;
}
"ln" {
    yylval = createLeaf(LN, yytext);
    return LN;
}
"sqrt" {
    yylval = createLeaf(SQRT, yytext);
    return SQRT;
}
"pi" {
    yylval = createLeaf(PI, yytext);
    return PI;
}
";" {
    yylval = createLeaf(SEMICOLON, yytext);
    return SEMICOLON;
}
"," {
    yylval = createLeaf(COMMA, yytext);
    return COMMA;
}
"[" {
    yylval = createLeaf(LSPAREN, yytext);
    return LSPAREN;
}
"]" {
    yylval = createLeaf(RSPAREN, yytext);
    return RSPAREN;
}
"{" {
    yylval = createLeaf(LCPAREN, yytext);
    return LCPAREN;
}
"}" {
    yylval = createLeaf(RCPAREN, yytext);
    return RCPAREN;
}
"==" {
    yylval = createLeaf(EQUAL, yytext);
    return EQUAL;
}
"->" {
    yylval = createLeaf(ARROW, yytext);
    return ARROW;
}
"+" {
    yylval = createLeaf(PLUS, yytext);
    return PLUS;
}
"-" {
    yylval = createLeaf(MINUS, yytext);
    return MINUS;
}
"*" {
    yylval = createLeaf(TIMES, yytext);
    return TIMES;
}
"/" {
    yylval = createLeaf(DIVIDE, yytext);
    return DIVIDE;
}
"^" {
    yylval = createLeaf(POWER, yytext);
    return POWER;
}
"(" {
    yylval = createLeaf(LPAREN, yytext);
    return LPAREN;
}
")" {
    yylval = createLeaf(RPAREN, yytext);
    return RPAREN;
}

"openqasm" {
    yylval = createLeaf(OPENQASM, yytext);
    return OPENQASM;
}

"u" {
    yylval = createLeaf(U, yytext);
    return U;
}

"cx" {
    yylval = createLeaf(CX, yytext);
    return CX;
}

([0-9]+\.[0-9]*|[0-9]\.[0-9]+)([eE][-+]?[0-9]+)? {
    yylval = createLeaf(REAL, yytext);
    return REAL;
}

[1-9]+[0-9]*|0 {
    yylval = createLeaf(NNINTEGER, yytext);
    return NNINTEGER;
}

[a-z][A-Za-z0-9_]* {
    yylval = createLeaf(ID, yytext);
    return ID;
}

[ \n\t\r]+ {
}

%%

int yywrap(void)
{
    puts("this file is end **********");
    return 1;
}
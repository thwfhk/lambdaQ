/* A Bison parser, made by GNU Bison 3.7.3.  */

/* Bison interface for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2020 Free Software Foundation,
   Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
   especially those whose name start with YY_ or yy_.  They are
   private implementation details that can be changed or removed.  */

#ifndef YY_YY_PARSER_TAB_H_INCLUDED
# define YY_YY_PARSER_TAB_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 1
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token kinds.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    YYEMPTY = -2,
    YYEOF = 0,                     /* "end of file"  */
    YYerror = 256,                 /* error  */
    YYUNDEF = 257,                 /* "invalid token"  */
    QREG = 258,                    /* QREG  */
    CREG = 259,                    /* CREG  */
    BARRIER = 260,                 /* BARRIER  */
    GATE = 261,                    /* GATE  */
    MEASURE = 262,                 /* MEASURE  */
    RESET = 263,                   /* RESET  */
    INCLUDE = 264,                 /* INCLUDE  */
    OPAQUE = 265,                  /* OPAQUE  */
    IF = 266,                      /* IF  */
    SIN = 267,                     /* SIN  */
    COS = 268,                     /* COS  */
    TAN = 269,                     /* TAN  */
    EXP = 270,                     /* EXP  */
    LN = 271,                      /* LN  */
    SQRT = 272,                    /* SQRT  */
    PI = 273,                      /* PI  */
    SEMICOLON = 274,               /* SEMICOLON  */
    COMMA = 275,                   /* COMMA  */
    LSPAREN = 276,                 /* LSPAREN  */
    RSPAREN = 277,                 /* RSPAREN  */
    LCPAREN = 278,                 /* LCPAREN  */
    RCPAREN = 279,                 /* RCPAREN  */
    EQUAL = 280,                   /* EQUAL  */
    ARROW = 281,                   /* ARROW  */
    PLUS = 282,                    /* PLUS  */
    MINUS = 283,                   /* MINUS  */
    TIMES = 284,                   /* TIMES  */
    DIVIDE = 285,                  /* DIVIDE  */
    POWER = 286,                   /* POWER  */
    LPAREN = 287,                  /* LPAREN  */
    RPAREN = 288,                  /* RPAREN  */
    REAL = 289,                    /* REAL  */
    NNINTEGER = 290,               /* NNINTEGER  */
    ID = 291,                      /* ID  */
    OPENQASM = 292,                /* OPENQASM  */
    U = 293,                       /* U  */
    CX = 294,                      /* CX  */
    UMINUS = 295                   /* UMINUS  */
  };
  typedef enum yytokentype yytoken_kind_t;
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef struct Node * YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;

int yyparse (struct Node ** root);

#endif /* !YY_YY_PARSER_TAB_H_INCLUDED  */

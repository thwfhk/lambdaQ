/* A Bison parser, made by GNU Bison 3.7.3.  */

/* Bison implementation for Yacc-like parsers in C

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

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
   especially those whose name start with YY_ or yy_.  They are
   private implementation details that can be changed or removed.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "3.7.3"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1




/* First part of user prologue.  */
#line 1 "parser.y"

#include "tree.h"

extern "C"{
void yyerror(struct Node** root, const char *s);
extern int yylex(void);
#define YYDEBUG 1
// 这里yylex是在lex.yy.c中定义的
}

#line 82 "parser.tab.c"

# ifndef YY_CAST
#  ifdef __cplusplus
#   define YY_CAST(Type, Val) static_cast<Type> (Val)
#   define YY_REINTERPRET_CAST(Type, Val) reinterpret_cast<Type> (Val)
#  else
#   define YY_CAST(Type, Val) ((Type) (Val))
#   define YY_REINTERPRET_CAST(Type, Val) ((Type) (Val))
#  endif
# endif
# ifndef YY_NULLPTR
#  if defined __cplusplus
#   if 201103L <= __cplusplus
#    define YY_NULLPTR nullptr
#   else
#    define YY_NULLPTR 0
#   endif
#  else
#   define YY_NULLPTR ((void*)0)
#  endif
# endif

#include "parser.tab.h"
/* Symbol kind.  */
enum yysymbol_kind_t
{
  YYSYMBOL_YYEMPTY = -2,
  YYSYMBOL_YYEOF = 0,                      /* "end of file"  */
  YYSYMBOL_YYerror = 1,                    /* error  */
  YYSYMBOL_YYUNDEF = 2,                    /* "invalid token"  */
  YYSYMBOL_QREG = 3,                       /* QREG  */
  YYSYMBOL_CREG = 4,                       /* CREG  */
  YYSYMBOL_BARRIER = 5,                    /* BARRIER  */
  YYSYMBOL_GATE = 6,                       /* GATE  */
  YYSYMBOL_MEASURE = 7,                    /* MEASURE  */
  YYSYMBOL_RESET = 8,                      /* RESET  */
  YYSYMBOL_INCLUDE = 9,                    /* INCLUDE  */
  YYSYMBOL_OPAQUE = 10,                    /* OPAQUE  */
  YYSYMBOL_IF = 11,                        /* IF  */
  YYSYMBOL_SIN = 12,                       /* SIN  */
  YYSYMBOL_COS = 13,                       /* COS  */
  YYSYMBOL_TAN = 14,                       /* TAN  */
  YYSYMBOL_EXP = 15,                       /* EXP  */
  YYSYMBOL_LN = 16,                        /* LN  */
  YYSYMBOL_SQRT = 17,                      /* SQRT  */
  YYSYMBOL_PI = 18,                        /* PI  */
  YYSYMBOL_SEMICOLON = 19,                 /* SEMICOLON  */
  YYSYMBOL_COMMA = 20,                     /* COMMA  */
  YYSYMBOL_LSPAREN = 21,                   /* LSPAREN  */
  YYSYMBOL_RSPAREN = 22,                   /* RSPAREN  */
  YYSYMBOL_LCPAREN = 23,                   /* LCPAREN  */
  YYSYMBOL_RCPAREN = 24,                   /* RCPAREN  */
  YYSYMBOL_EQUAL = 25,                     /* EQUAL  */
  YYSYMBOL_ARROW = 26,                     /* ARROW  */
  YYSYMBOL_PLUS = 27,                      /* PLUS  */
  YYSYMBOL_MINUS = 28,                     /* MINUS  */
  YYSYMBOL_TIMES = 29,                     /* TIMES  */
  YYSYMBOL_DIVIDE = 30,                    /* DIVIDE  */
  YYSYMBOL_POWER = 31,                     /* POWER  */
  YYSYMBOL_LPAREN = 32,                    /* LPAREN  */
  YYSYMBOL_RPAREN = 33,                    /* RPAREN  */
  YYSYMBOL_REAL = 34,                      /* REAL  */
  YYSYMBOL_NNINTEGER = 35,                 /* NNINTEGER  */
  YYSYMBOL_ID = 36,                        /* ID  */
  YYSYMBOL_OPENQASM = 37,                  /* OPENQASM  */
  YYSYMBOL_U = 38,                         /* U  */
  YYSYMBOL_CX = 39,                        /* CX  */
  YYSYMBOL_UMINUS = 40,                    /* UMINUS  */
  YYSYMBOL_YYACCEPT = 41,                  /* $accept  */
  YYSYMBOL_start = 42,                     /* start  */
  YYSYMBOL_mainprogram = 43,               /* mainprogram  */
  YYSYMBOL_program = 44,                   /* program  */
  YYSYMBOL_statement = 45,                 /* statement  */
  YYSYMBOL_decl = 46,                      /* decl  */
  YYSYMBOL_exp = 47,                       /* exp  */
  YYSYMBOL_unaryop = 48,                   /* unaryop  */
  YYSYMBOL_explist = 49,                   /* explist  */
  YYSYMBOL_argument = 50,                  /* argument  */
  YYSYMBOL_idlist = 51,                    /* idlist  */
  YYSYMBOL_mixedlist = 52,                 /* mixedlist  */
  YYSYMBOL_anylist = 53,                   /* anylist  */
  YYSYMBOL_uop = 54,                       /* uop  */
  YYSYMBOL_qop = 55,                       /* qop  */
  YYSYMBOL_goplist = 56,                   /* goplist  */
  YYSYMBOL_gatedecl = 57                   /* gatedecl  */
};
typedef enum yysymbol_kind_t yysymbol_kind_t;




#ifdef short
# undef short
#endif

/* On compilers that do not define __PTRDIFF_MAX__ etc., make sure
   <limits.h> and (if available) <stdint.h> are included
   so that the code can choose integer types of a good width.  */

#ifndef __PTRDIFF_MAX__
# include <limits.h> /* INFRINGES ON USER NAME SPACE */
# if defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stdint.h> /* INFRINGES ON USER NAME SPACE */
#  define YY_STDINT_H
# endif
#endif

/* Narrow types that promote to a signed type and that can represent a
   signed or unsigned integer of at least N bits.  In tables they can
   save space and decrease cache pressure.  Promoting to a signed type
   helps avoid bugs in integer arithmetic.  */

#ifdef __INT_LEAST8_MAX__
typedef __INT_LEAST8_TYPE__ yytype_int8;
#elif defined YY_STDINT_H
typedef int_least8_t yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef __INT_LEAST16_MAX__
typedef __INT_LEAST16_TYPE__ yytype_int16;
#elif defined YY_STDINT_H
typedef int_least16_t yytype_int16;
#else
typedef short yytype_int16;
#endif

#if defined __UINT_LEAST8_MAX__ && __UINT_LEAST8_MAX__ <= __INT_MAX__
typedef __UINT_LEAST8_TYPE__ yytype_uint8;
#elif (!defined __UINT_LEAST8_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST8_MAX <= INT_MAX)
typedef uint_least8_t yytype_uint8;
#elif !defined __UINT_LEAST8_MAX__ && UCHAR_MAX <= INT_MAX
typedef unsigned char yytype_uint8;
#else
typedef short yytype_uint8;
#endif

#if defined __UINT_LEAST16_MAX__ && __UINT_LEAST16_MAX__ <= __INT_MAX__
typedef __UINT_LEAST16_TYPE__ yytype_uint16;
#elif (!defined __UINT_LEAST16_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST16_MAX <= INT_MAX)
typedef uint_least16_t yytype_uint16;
#elif !defined __UINT_LEAST16_MAX__ && USHRT_MAX <= INT_MAX
typedef unsigned short yytype_uint16;
#else
typedef int yytype_uint16;
#endif

#ifndef YYPTRDIFF_T
# if defined __PTRDIFF_TYPE__ && defined __PTRDIFF_MAX__
#  define YYPTRDIFF_T __PTRDIFF_TYPE__
#  define YYPTRDIFF_MAXIMUM __PTRDIFF_MAX__
# elif defined PTRDIFF_MAX
#  ifndef ptrdiff_t
#   include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  endif
#  define YYPTRDIFF_T ptrdiff_t
#  define YYPTRDIFF_MAXIMUM PTRDIFF_MAX
# else
#  define YYPTRDIFF_T long
#  define YYPTRDIFF_MAXIMUM LONG_MAX
# endif
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned
# endif
#endif

#define YYSIZE_MAXIMUM                                  \
  YY_CAST (YYPTRDIFF_T,                                 \
           (YYPTRDIFF_MAXIMUM < YY_CAST (YYSIZE_T, -1)  \
            ? YYPTRDIFF_MAXIMUM                         \
            : YY_CAST (YYSIZE_T, -1)))

#define YYSIZEOF(X) YY_CAST (YYPTRDIFF_T, sizeof (X))


/* Stored state numbers (used for stacks). */
typedef yytype_uint8 yy_state_t;

/* State numbers in computations.  */
typedef int yy_state_fast_t;

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif


#ifndef YY_ATTRIBUTE_PURE
# if defined __GNUC__ && 2 < __GNUC__ + (96 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_PURE __attribute__ ((__pure__))
# else
#  define YY_ATTRIBUTE_PURE
# endif
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# if defined __GNUC__ && 2 < __GNUC__ + (7 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_UNUSED __attribute__ ((__unused__))
# else
#  define YY_ATTRIBUTE_UNUSED
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(E) ((void) (E))
#else
# define YYUSE(E) /* empty */
#endif

#if defined __GNUC__ && ! defined __ICC && 407 <= __GNUC__ * 100 + __GNUC_MINOR__
/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                            \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")              \
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# define YY_IGNORE_MAYBE_UNINITIALIZED_END      \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif

#if defined __cplusplus && defined __GNUC__ && ! defined __ICC && 6 <= __GNUC__
# define YY_IGNORE_USELESS_CAST_BEGIN                          \
    _Pragma ("GCC diagnostic push")                            \
    _Pragma ("GCC diagnostic ignored \"-Wuseless-cast\"")
# define YY_IGNORE_USELESS_CAST_END            \
    _Pragma ("GCC diagnostic pop")
#endif
#ifndef YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_END
#endif


#define YY_ASSERT(E) ((void) (0 && (E)))

#if !defined yyoverflow

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
             && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* !defined yyoverflow */

#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yy_state_t yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (YYSIZEOF (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (YYSIZEOF (yy_state_t) + YYSIZEOF (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYPTRDIFF_T yynewbytes;                                         \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * YYSIZEOF (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / YYSIZEOF (*yyptr);                        \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, YY_CAST (YYSIZE_T, (Count)) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYPTRDIFF_T yyi;                      \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  7
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   194

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  41
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  17
/* YYNRULES -- Number of rules.  */
#define YYNRULES  62
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  158

/* YYMAXUTOK -- Last valid token kind.  */
#define YYMAXUTOK   295


/* YYTRANSLATE(TOKEN-NUM) -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, with out-of-bounds checking.  */
#define YYTRANSLATE(YYX)                                \
  (0 <= (YYX) && (YYX) <= YYMAXUTOK                     \
   ? YY_CAST (yysymbol_kind_t, yytranslate[YYX])        \
   : YYSYMBOL_YYUNDEF)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex.  */
static const yytype_int8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_int16 yyrline[] =
{
       0,    31,    31,    36,    42,    49,    53,    59,    63,    67,
      71,    75,    79,    83,    87,    91,    96,   100,   105,   109,
     113,   117,   121,   125,   129,   133,   137,   141,   145,   149,
     154,   158,   162,   166,   170,   174,   180,   184,   190,   194,
     200,   204,   210,   214,   218,   223,   229,   233,   239,   243,
     247,   251,   255,   261,   265,   269,   275,   279,   283,   287,
     293,   297,   301
};
#endif

/** Accessing symbol of state STATE.  */
#define YY_ACCESSING_SYMBOL(State) YY_CAST (yysymbol_kind_t, yystos[State])

#if YYDEBUG || 0
/* The user-facing name of the symbol whose (internal) number is
   YYSYMBOL.  No bounds checking.  */
static const char *yysymbol_name (yysymbol_kind_t yysymbol) YY_ATTRIBUTE_UNUSED;

/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "\"end of file\"", "error", "\"invalid token\"", "QREG", "CREG",
  "BARRIER", "GATE", "MEASURE", "RESET", "INCLUDE", "OPAQUE", "IF", "SIN",
  "COS", "TAN", "EXP", "LN", "SQRT", "PI", "SEMICOLON", "COMMA", "LSPAREN",
  "RSPAREN", "LCPAREN", "RCPAREN", "EQUAL", "ARROW", "PLUS", "MINUS",
  "TIMES", "DIVIDE", "POWER", "LPAREN", "RPAREN", "REAL", "NNINTEGER",
  "ID", "OPENQASM", "U", "CX", "UMINUS", "$accept", "start", "mainprogram",
  "program", "statement", "decl", "exp", "unaryop", "explist", "argument",
  "idlist", "mixedlist", "anylist", "uop", "qop", "goplist", "gatedecl", YY_NULLPTR
};

static const char *
yysymbol_name (yysymbol_kind_t yysymbol)
{
  return yytname[yysymbol];
}
#endif

#ifdef YYPRINT
/* YYTOKNUM[NUM] -- (External) token number corresponding to the
   (internal) symbol number NUM (which must be that of a token).  */
static const yytype_int16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
     295
};
#endif

#define YYPACT_NINF (-68)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-1)

#define yytable_value_is_error(Yyn) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
      -3,    -8,    24,    69,   -68,   -68,    58,   -68,    13,    67,
      94,   106,   107,   109,   109,   110,   112,    29,   116,   109,
      13,   -68,   -68,   -68,   -68,     8,   126,   128,   129,   131,
     132,   134,    59,   133,   130,   136,    60,   122,    72,   138,
      97,   139,   -68,   124,   -68,   -68,    21,   127,   135,   137,
     125,   140,   -68,    20,   -68,    10,   142,   109,   -68,    31,
     108,   141,   -68,   -68,   -68,   -68,   -68,   -68,   -68,    97,
      97,   106,   -68,   -68,   -68,    52,   143,   -11,   -68,    -6,
     109,   115,   124,   -68,   -68,   145,   146,   147,   144,   150,
     124,    15,   148,   -68,   151,   155,   124,    30,   -68,   152,
     -68,    45,   159,    97,    97,    97,    97,    97,    97,    97,
     106,   109,   160,   -68,   117,   161,   162,   -68,   153,   154,
     101,   124,   -68,   -68,   -68,   119,   124,   149,   -68,   -68,
     -19,   -19,   163,   163,   163,    89,    52,   164,   166,   -68,
     -68,   -68,   -68,   168,   169,   -68,   103,   -68,   121,    63,
     -68,   -68,   -68,   -68,   -68,   -68,   -68,   -68
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_int8 yydefact[] =
{
       0,     0,     0,     0,     2,     3,     0,     1,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       4,     5,     7,    53,    13,     0,     0,     0,    40,    46,
      47,     0,     0,    38,     0,     0,     0,     0,     0,     0,
       0,     0,     6,     0,     9,    56,     0,     0,     0,     0,
       0,     0,    15,     0,    40,     0,     0,     0,    55,     0,
       0,     0,    30,    31,    32,    33,    34,    35,    18,     0,
       0,     0,    21,    20,    19,    36,     0,     0,    50,     0,
       0,     0,     0,     8,    58,     0,     0,     0,    41,    43,
       0,     0,     0,    60,     0,     0,     0,     0,    10,     0,
      27,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    57,     0,     0,     0,    42,     0,     0,
       0,     0,    41,    39,    54,     0,     0,     0,    28,    51,
      25,    22,    23,    24,    26,     0,    37,     0,     0,    49,
      59,    16,    17,     0,     0,    61,     0,    11,     0,     0,
      29,    52,    48,    45,    44,    62,    12,    14
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
     -68,   -68,   185,   -68,   172,   -68,   -67,   -68,   123,   -14,
     -28,   -68,   -16,   -18,    44,   -68,   -68
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int8 yydefgoto[] =
{
      -1,     3,     4,    20,    21,    22,    75,    76,    77,    34,
      29,    30,    31,    23,    24,    46,    25
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_uint8 yytable[] =
{
      35,    39,   100,   101,    55,    41,     1,    45,    60,   109,
     105,   106,   107,    43,   109,    81,     9,    10,    11,    12,
      13,    14,   110,    15,    16,    91,    82,   111,    84,     2,
      92,    97,    44,    93,     2,    92,   130,   131,   132,   133,
     134,   135,   136,    95,    17,    83,    18,    19,   121,    17,
      92,    18,    19,    90,   114,   102,    54,    17,     6,    18,
      19,    38,   120,   126,    96,    28,   112,    54,   125,     7,
      13,    14,   103,   104,   105,   106,   107,     8,   128,   103,
     104,   105,   106,   107,    62,    63,    64,    65,    66,    67,
      68,    53,    59,   146,   137,    54,    54,   138,   148,    17,
      69,    18,    19,    26,    70,    71,    72,    73,    74,    62,
      63,    64,    65,    66,    67,    68,   103,   104,   105,   106,
     107,    92,   150,    92,   145,    69,   155,    98,    92,    70,
      27,    72,    73,    74,   113,    92,   140,    92,   147,    92,
     156,    92,    28,    32,    37,    33,    36,    47,    40,    48,
      49,    50,    51,    52,    56,    58,    57,    78,    61,    80,
      54,    88,    85,    79,     0,   118,    99,   115,   116,   117,
      86,   119,    87,   123,   124,   108,    89,    94,   129,   139,
     141,   142,   149,   151,   122,   152,     5,   127,   143,   144,
     153,   154,    42,   157,   107
};

static const yytype_int16 yycheck[] =
{
      14,    17,    69,    70,    32,    19,     9,    25,    36,    20,
      29,    30,    31,     5,    20,    43,     3,     4,     5,     6,
       7,     8,    33,    10,    11,    53,     5,    33,    46,    37,
      20,    59,    24,    23,    37,    20,   103,   104,   105,   106,
     107,   108,   109,    57,    36,    24,    38,    39,    33,    36,
      20,    38,    39,    33,    82,    71,    36,    36,    34,    38,
      39,    32,    90,    33,    33,    36,    80,    36,    96,     0,
       7,     8,    27,    28,    29,    30,    31,    19,    33,    27,
      28,    29,    30,    31,    12,    13,    14,    15,    16,    17,
      18,    32,    32,   121,   110,    36,    36,   111,   126,    36,
      28,    38,    39,    36,    32,    33,    34,    35,    36,    12,
      13,    14,    15,    16,    17,    18,    27,    28,    29,    30,
      31,    20,    33,    20,    23,    28,    23,    19,    20,    32,
      36,    34,    35,    36,    19,    20,    19,    20,    19,    20,
      19,    20,    36,    36,    32,    36,    36,    21,    32,    21,
      21,    20,    20,    19,    21,    19,    26,    19,    36,    20,
      36,    36,    35,    40,    -1,    21,    25,    22,    22,    22,
      35,    21,    35,    22,    19,    32,    36,    35,    19,    19,
      19,    19,    33,    19,    36,    19,     1,    35,    35,    35,
      22,    22,    20,   149,    31
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_int8 yystos[] =
{
       0,     9,    37,    42,    43,    43,    34,     0,    19,     3,
       4,     5,     6,     7,     8,    10,    11,    36,    38,    39,
      44,    45,    46,    54,    55,    57,    36,    36,    36,    51,
      52,    53,    36,    36,    50,    50,    36,    32,    32,    53,
      32,    50,    45,     5,    24,    54,    56,    21,    21,    21,
      20,    20,    19,    32,    36,    51,    21,    26,    19,    32,
      51,    36,    12,    13,    14,    15,    16,    17,    18,    28,
      32,    33,    34,    35,    36,    47,    48,    49,    19,    49,
      20,    51,     5,    24,    54,    35,    35,    35,    36,    36,
      33,    51,    20,    23,    35,    50,    33,    51,    19,    25,
      47,    47,    53,    27,    28,    29,    30,    31,    32,    20,
      33,    33,    50,    19,    51,    22,    22,    22,    21,    21,
      51,    33,    36,    22,    19,    51,    33,    35,    33,    19,
      47,    47,    47,    47,    47,    47,    47,    53,    50,    19,
      19,    19,    19,    35,    35,    23,    51,    19,    51,    33,
      33,    19,    19,    22,    22,    23,    19,    55
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_int8 yyr1[] =
{
       0,    41,    42,    42,    43,    44,    44,    45,    45,    45,
      45,    45,    45,    45,    45,    45,    46,    46,    47,    47,
      47,    47,    47,    47,    47,    47,    47,    47,    47,    47,
      48,    48,    48,    48,    48,    48,    49,    49,    50,    50,
      51,    51,    52,    52,    52,    52,    53,    53,    54,    54,
      54,    54,    54,    55,    55,    55,    56,    56,    56,    56,
      57,    57,    57
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     1,     2,     4,     1,     2,     1,     3,     2,
       4,     6,     7,     1,     7,     3,     6,     6,     1,     1,
       1,     1,     3,     3,     3,     3,     3,     2,     3,     4,
       1,     1,     1,     1,     1,     1,     1,     3,     1,     4,
       1,     3,     4,     3,     6,     6,     1,     1,     6,     5,
       3,     5,     6,     1,     5,     3,     1,     3,     2,     4,
       4,     6,     7
};


enum { YYENOMEM = -2 };

#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                    \
  do                                                              \
    if (yychar == YYEMPTY)                                        \
      {                                                           \
        yychar = (Token);                                         \
        yylval = (Value);                                         \
        YYPOPSTACK (yylen);                                       \
        yystate = *yyssp;                                         \
        goto yybackup;                                            \
      }                                                           \
    else                                                          \
      {                                                           \
        yyerror (root, YY_("syntax error: cannot back up")); \
        YYERROR;                                                  \
      }                                                           \
  while (0)

/* Backward compatibility with an undocumented macro.
   Use YYerror or YYUNDEF. */
#define YYERRCODE YYUNDEF


/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)

/* This macro is provided for backward compatibility. */
# ifndef YY_LOCATION_PRINT
#  define YY_LOCATION_PRINT(File, Loc) ((void) 0)
# endif


# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Kind, Value, root); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*-----------------------------------.
| Print this symbol's value on YYO.  |
`-----------------------------------*/

static void
yy_symbol_value_print (FILE *yyo,
                       yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep, struct Node ** root)
{
  FILE *yyoutput = yyo;
  YYUSE (yyoutput);
  YYUSE (root);
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yykind < YYNTOKENS)
    YYPRINT (yyo, yytoknum[yykind], *yyvaluep);
# endif
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YYUSE (yykind);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/*---------------------------.
| Print this symbol on YYO.  |
`---------------------------*/

static void
yy_symbol_print (FILE *yyo,
                 yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep, struct Node ** root)
{
  YYFPRINTF (yyo, "%s %s (",
             yykind < YYNTOKENS ? "token" : "nterm", yysymbol_name (yykind));

  yy_symbol_value_print (yyo, yykind, yyvaluep, root);
  YYFPRINTF (yyo, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yy_state_t *yybottom, yy_state_t *yytop)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yy_state_t *yyssp, YYSTYPE *yyvsp,
                 int yyrule, struct Node ** root)
{
  int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %d):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       YY_ACCESSING_SYMBOL (+yyssp[yyi + 1 - yynrhs]),
                       &yyvsp[(yyi + 1) - (yynrhs)], root);
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, Rule, root); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args) ((void) 0)
# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif






/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg,
            yysymbol_kind_t yykind, YYSTYPE *yyvaluep, struct Node ** root)
{
  YYUSE (yyvaluep);
  YYUSE (root);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yykind, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YYUSE (yykind);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/* Lookahead token kind.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;
/* Number of syntax errors so far.  */
int yynerrs;




/*----------.
| yyparse.  |
`----------*/

int
yyparse (struct Node ** root)
{
    yy_state_fast_t yystate = 0;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus = 0;

    /* Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* Their size.  */
    YYPTRDIFF_T yystacksize = YYINITDEPTH;

    /* The state stack: array, bottom, top.  */
    yy_state_t yyssa[YYINITDEPTH];
    yy_state_t *yyss = yyssa;
    yy_state_t *yyssp = yyss;

    /* The semantic value stack: array, bottom, top.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs = yyvsa;
    YYSTYPE *yyvsp = yyvs;

  int yyn;
  /* The return value of yyparse.  */
  int yyresult;
  /* Lookahead symbol kind.  */
  yysymbol_kind_t yytoken = YYSYMBOL_YYEMPTY;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;



#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yychar = YYEMPTY; /* Cause a token to be read.  */
  goto yysetstate;


/*------------------------------------------------------------.
| yynewstate -- push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;


/*--------------------------------------------------------------------.
| yysetstate -- set current state (the top of the stack) to yystate.  |
`--------------------------------------------------------------------*/
yysetstate:
  YYDPRINTF ((stderr, "Entering state %d\n", yystate));
  YY_ASSERT (0 <= yystate && yystate < YYNSTATES);
  YY_IGNORE_USELESS_CAST_BEGIN
  *yyssp = YY_CAST (yy_state_t, yystate);
  YY_IGNORE_USELESS_CAST_END
  YY_STACK_PRINT (yyss, yyssp);

  if (yyss + yystacksize - 1 <= yyssp)
#if !defined yyoverflow && !defined YYSTACK_RELOCATE
    goto yyexhaustedlab;
#else
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYPTRDIFF_T yysize = yyssp - yyss + 1;

# if defined yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        yy_state_t *yyss1 = yyss;
        YYSTYPE *yyvs1 = yyvs;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * YYSIZEOF (*yyssp),
                    &yyvs1, yysize * YYSIZEOF (*yyvsp),
                    &yystacksize);
        yyss = yyss1;
        yyvs = yyvs1;
      }
# else /* defined YYSTACK_RELOCATE */
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yy_state_t *yyss1 = yyss;
        union yyalloc *yyptr =
          YY_CAST (union yyalloc *,
                   YYSTACK_ALLOC (YY_CAST (YYSIZE_T, YYSTACK_BYTES (yystacksize))));
        if (! yyptr)
          goto yyexhaustedlab;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
#  undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

      YY_IGNORE_USELESS_CAST_BEGIN
      YYDPRINTF ((stderr, "Stack size increased to %ld\n",
                  YY_CAST (long, yystacksize)));
      YY_IGNORE_USELESS_CAST_END

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }
#endif /* !defined yyoverflow && !defined YYSTACK_RELOCATE */

  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;


/*-----------.
| yybackup.  |
`-----------*/
yybackup:
  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either empty, or end-of-input, or a valid lookahead.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token\n"));
      yychar = yylex ();
    }

  if (yychar <= YYEOF)
    {
      yychar = YYEOF;
      yytoken = YYSYMBOL_YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else if (yychar == YYerror)
    {
      /* The scanner already issued an error message, process directly
         to error recovery.  But do not keep the error token as
         lookahead, it is too special and may lead us to an endless
         loop in error recovery. */
      yychar = YYUNDEF;
      yytoken = YYSYMBOL_YYerror;
      goto yyerrlab1;
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);
  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  /* Discard the shifted token.  */
  yychar = YYEMPTY;
  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     '$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
  case 2: /* start: mainprogram  */
#line 32 "parser.y"
        {
		yyval = yyvsp[0];
        *root = yyval;
	}
#line 1229 "parser.tab.c"
    break;

  case 3: /* start: INCLUDE mainprogram  */
#line 37 "parser.y"
        {
		// 其实并不支持include
	}
#line 1237 "parser.tab.c"
    break;

  case 4: /* mainprogram: OPENQASM REAL SEMICOLON program  */
#line 43 "parser.y"
        {
		yyval = createNode(HEADER, 2, yyvsp[-2], yyvsp[0]);
		// 只记录了版本号和程序
	}
#line 1246 "parser.tab.c"
    break;

  case 5: /* program: statement  */
#line 50 "parser.y"
        {
		yyval = createNode(STATEMENT, 1, yyvsp[0]);
	}
#line 1254 "parser.tab.c"
    break;

  case 6: /* program: program statement  */
#line 54 "parser.y"
        {
		yyval = createNode(PROGRAM_STATEMENT, 2, yyvsp[-1], yyvsp[0]);
	}
#line 1262 "parser.tab.c"
    break;

  case 7: /* statement: decl  */
#line 60 "parser.y"
        {
		yyval = createNode(DECL, 1, yyvsp[0]);
	}
#line 1270 "parser.tab.c"
    break;

  case 8: /* statement: gatedecl goplist RCPAREN  */
#line 64 "parser.y"
        {
		yyval = createNode(STATEMENT_GATE_GOPLIST, 2, yyvsp[-2], yyvsp[-1]);
	}
#line 1278 "parser.tab.c"
    break;

  case 9: /* statement: gatedecl RCPAREN  */
#line 68 "parser.y"
        {
		yyval = createNode(STATEMENT_GATEDECL, 1, yyvsp[-1]);
	}
#line 1286 "parser.tab.c"
    break;

  case 10: /* statement: OPAQUE ID idlist SEMICOLON  */
#line 72 "parser.y"
        {
		yyval = createNode(OPAQUE, 3, yyvsp[-3], NULL, yyvsp[-2]);
	}
#line 1294 "parser.tab.c"
    break;

  case 11: /* statement: OPAQUE ID LPAREN RPAREN idlist SEMICOLON  */
#line 76 "parser.y"
        {
		yyval = createNode(OPAQUE, 3, yyvsp[-4], NULL, yyvsp[-1]);
	}
#line 1302 "parser.tab.c"
    break;

  case 12: /* statement: OPAQUE ID LPAREN idlist RPAREN idlist SEMICOLON  */
#line 80 "parser.y"
        {
		yyval = createNode(OPAQUE, 3, yyvsp[-5], yyvsp[-3], yyvsp[-1]);
	}
#line 1310 "parser.tab.c"
    break;

  case 13: /* statement: qop  */
#line 84 "parser.y"
        {
		yyval = yyvsp[0];
	}
#line 1318 "parser.tab.c"
    break;

  case 14: /* statement: IF LPAREN ID EQUAL NNINTEGER RPAREN qop  */
#line 88 "parser.y"
        {
		yyval = createNode(IF, 3, yyvsp[-4], yyvsp[-2], yyvsp[0]);
	}
#line 1326 "parser.tab.c"
    break;

  case 15: /* statement: BARRIER anylist SEMICOLON  */
#line 92 "parser.y"
        {
		yyval = createNode(BARRIER, 1, yyvsp[-1]);
	}
#line 1334 "parser.tab.c"
    break;

  case 16: /* decl: QREG ID LSPAREN NNINTEGER RSPAREN SEMICOLON  */
#line 97 "parser.y"
        {
		yyval = createNode(Q_DECL, 2, yyvsp[-4], yyvsp[-2]);
	}
#line 1342 "parser.tab.c"
    break;

  case 17: /* decl: CREG ID LSPAREN NNINTEGER RSPAREN SEMICOLON  */
#line 101 "parser.y"
        {
		yyval = createNode(C_DECL, 2, yyvsp[-4], yyvsp[-2]);
	}
#line 1350 "parser.tab.c"
    break;

  case 18: /* exp: PI  */
#line 106 "parser.y"
        {
        yyval = yyvsp[0];
    }
#line 1358 "parser.tab.c"
    break;

  case 19: /* exp: ID  */
#line 110 "parser.y"
        {
        yyval = yyvsp[0];
    }
#line 1366 "parser.tab.c"
    break;

  case 20: /* exp: NNINTEGER  */
#line 114 "parser.y"
        {
        yyval = yyvsp[0];
    }
#line 1374 "parser.tab.c"
    break;

  case 21: /* exp: REAL  */
#line 118 "parser.y"
    {
        yyval = yyvsp[0];
    }
#line 1382 "parser.tab.c"
    break;

  case 22: /* exp: exp MINUS exp  */
#line 122 "parser.y"
        {
        yyval = createNode(yyvsp[-1]->tag, 2, yyvsp[-2], yyvsp[0]);
    }
#line 1390 "parser.tab.c"
    break;

  case 23: /* exp: exp TIMES exp  */
#line 126 "parser.y"
        {
        yyval = createNode(yyvsp[-1]->tag, 2, yyvsp[-2], yyvsp[0]);
    }
#line 1398 "parser.tab.c"
    break;

  case 24: /* exp: exp DIVIDE exp  */
#line 130 "parser.y"
        {
        yyval = createNode(yyvsp[-1]->tag, 2, yyvsp[-2], yyvsp[0]);
    }
#line 1406 "parser.tab.c"
    break;

  case 25: /* exp: exp PLUS exp  */
#line 134 "parser.y"
    {
        yyval = createNode(yyvsp[-1]->tag, 2, yyvsp[-2], yyvsp[0]);
    }
#line 1414 "parser.tab.c"
    break;

  case 26: /* exp: exp POWER exp  */
#line 138 "parser.y"
        {
		yyval = createNode(yyvsp[-1]->tag, 2, yyvsp[-2], yyvsp[0]);
	}
#line 1422 "parser.tab.c"
    break;

  case 27: /* exp: MINUS exp  */
#line 142 "parser.y"
    {
        yyval = createNode(NEG, 1, yyvsp[0]);
    }
#line 1430 "parser.tab.c"
    break;

  case 28: /* exp: LPAREN exp RPAREN  */
#line 146 "parser.y"
    {
        yyval = createNode(PAREN, 1, yyvsp[-1]);
    }
#line 1438 "parser.tab.c"
    break;

  case 29: /* exp: unaryop LPAREN exp RPAREN  */
#line 150 "parser.y"
    {
        yyval = createNode(EXP_UNARY, 2, yyvsp[-3], yyvsp[-1]);
    }
#line 1446 "parser.tab.c"
    break;

  case 30: /* unaryop: SIN  */
#line 155 "parser.y"
        {
		yyval = yyvsp[0];
	}
#line 1454 "parser.tab.c"
    break;

  case 31: /* unaryop: COS  */
#line 159 "parser.y"
        {
        yyval = yyvsp[0];
    }
#line 1462 "parser.tab.c"
    break;

  case 32: /* unaryop: TAN  */
#line 163 "parser.y"
        {
        yyval = yyvsp[0];
    }
#line 1470 "parser.tab.c"
    break;

  case 33: /* unaryop: EXP  */
#line 167 "parser.y"
        {
        yyval = yyvsp[0];
    }
#line 1478 "parser.tab.c"
    break;

  case 34: /* unaryop: LN  */
#line 171 "parser.y"
        {
        yyval = yyvsp[0];
    }
#line 1486 "parser.tab.c"
    break;

  case 35: /* unaryop: SQRT  */
#line 175 "parser.y"
    {
        yyval = yyvsp[0];
    }
#line 1494 "parser.tab.c"
    break;

  case 36: /* explist: exp  */
#line 181 "parser.y"
    {
        yyval = yyvsp[0];
    }
#line 1502 "parser.tab.c"
    break;

  case 37: /* explist: explist COMMA exp  */
#line 185 "parser.y"
    {
        yyval = createNode(EXPLIST, 2, yyvsp[-2], yyvsp[0]);
    }
#line 1510 "parser.tab.c"
    break;

  case 38: /* argument: ID  */
#line 191 "parser.y"
        {
		yyval = yyvsp[0];
	}
#line 1518 "parser.tab.c"
    break;

  case 39: /* argument: ID LSPAREN NNINTEGER RSPAREN  */
#line 195 "parser.y"
        {
		yyval = createNode(ARG_IDINT, 2, yyvsp[-3], yyvsp[-1]);
	}
#line 1526 "parser.tab.c"
    break;

  case 40: /* idlist: ID  */
#line 201 "parser.y"
        {
		yyval = yyvsp[0];
	}
#line 1534 "parser.tab.c"
    break;

  case 41: /* idlist: idlist COMMA ID  */
#line 205 "parser.y"
        {
		yyval = createNode(ID_LIST, 2, yyvsp[-2], yyvsp[0]);
	}
#line 1542 "parser.tab.c"
    break;

  case 42: /* mixedlist: ID LSPAREN NNINTEGER RSPAREN  */
#line 211 "parser.y"
        {
		yyval = createNode(MIXEDLIST1, 2, yyvsp[-3], yyvsp[-1]);
	}
#line 1550 "parser.tab.c"
    break;

  case 43: /* mixedlist: mixedlist COMMA ID  */
#line 215 "parser.y"
        {
		yyval = createNode(MIXEDLIST2, 2, yyvsp[-2], yyvsp[0]);
	}
#line 1558 "parser.tab.c"
    break;

  case 44: /* mixedlist: mixedlist COMMA ID LSPAREN NNINTEGER RSPAREN  */
#line 219 "parser.y"
        {
		struct Node* tmp = createNode(MIXEDLIST, 2, yyvsp[-3], yyvsp[-1]);
		yyval = createNode(MIXEDLIST3, 2, yyvsp[-5], tmp);
	}
#line 1567 "parser.tab.c"
    break;

  case 45: /* mixedlist: idlist COMMA ID LSPAREN NNINTEGER RSPAREN  */
#line 224 "parser.y"
        {
		struct Node* tmp = createNode(MIXEDLIST, 2, yyvsp[-3], yyvsp[-1]);
		yyval = createNode(MIXEDLIST3, 2, yyvsp[-5], tmp);
	}
#line 1576 "parser.tab.c"
    break;

  case 46: /* anylist: idlist  */
#line 230 "parser.y"
        {
		yyval = yyvsp[0];
	}
#line 1584 "parser.tab.c"
    break;

  case 47: /* anylist: mixedlist  */
#line 234 "parser.y"
        {
		yyval = yyvsp[0];
	}
#line 1592 "parser.tab.c"
    break;

  case 48: /* uop: U LPAREN explist RPAREN argument SEMICOLON  */
#line 240 "parser.y"
        {
		yyval = createNode(UOP_U, 2, yyvsp[-3], yyvsp[-1]);
	}
#line 1600 "parser.tab.c"
    break;

  case 49: /* uop: CX argument COMMA argument SEMICOLON  */
#line 244 "parser.y"
        {
		yyval = createNode(UOP_CX, 2, yyvsp[-3], yyvsp[-1]);
	}
#line 1608 "parser.tab.c"
    break;

  case 50: /* uop: ID anylist SEMICOLON  */
#line 248 "parser.y"
        {
		yyval = createNode(UOP_ID, 3, yyvsp[-2], NULL, yyvsp[-1]);
	}
#line 1616 "parser.tab.c"
    break;

  case 51: /* uop: ID LPAREN RPAREN anylist SEMICOLON  */
#line 252 "parser.y"
        {
		yyval = createNode(UOP_ID, 3, yyvsp[-4], NULL, yyvsp[-1]);
	}
#line 1624 "parser.tab.c"
    break;

  case 52: /* uop: ID LPAREN explist RPAREN anylist SEMICOLON  */
#line 256 "parser.y"
        {
		yyval = createNode(UOP_ID, 3, yyvsp[-5], yyvsp[-3], yyvsp[-1]);
	}
#line 1632 "parser.tab.c"
    break;

  case 53: /* qop: uop  */
#line 262 "parser.y"
        {
		yyval = yyvsp[0];
	}
#line 1640 "parser.tab.c"
    break;

  case 54: /* qop: MEASURE argument ARROW argument SEMICOLON  */
#line 266 "parser.y"
        {
		yyval = createNode(MEASURE, 2, yyvsp[-3], yyvsp[-1]);
	}
#line 1648 "parser.tab.c"
    break;

  case 55: /* qop: RESET argument SEMICOLON  */
#line 270 "parser.y"
        {
		yyval = createNode(RESET, 1, yyvsp[-1]);
	}
#line 1656 "parser.tab.c"
    break;

  case 56: /* goplist: uop  */
#line 276 "parser.y"
        {
		yyval = yyvsp[0];
	}
#line 1664 "parser.tab.c"
    break;

  case 57: /* goplist: BARRIER idlist SEMICOLON  */
#line 280 "parser.y"
        {
		yyval = createNode(GOPLIST_BARRIER, 1, yyvsp[-1]);
	}
#line 1672 "parser.tab.c"
    break;

  case 58: /* goplist: goplist uop  */
#line 284 "parser.y"
        {
		yyval = createNode(GOPLIST_GOPUOP, 2, yyvsp[-1], yyvsp[0]);
	}
#line 1680 "parser.tab.c"
    break;

  case 59: /* goplist: goplist BARRIER idlist SEMICOLON  */
#line 288 "parser.y"
        {
		yyval = createNode(GOPLIST_GOPBARRIER, 2, yyvsp[-3], yyvsp[-1]);
	}
#line 1688 "parser.tab.c"
    break;

  case 60: /* gatedecl: GATE ID idlist LCPAREN  */
#line 294 "parser.y"
        {
		yyval = createNode(GATEDECL, 3, yyvsp[-2], NULL, yyvsp[-1]);
	}
#line 1696 "parser.tab.c"
    break;

  case 61: /* gatedecl: GATE ID LPAREN RPAREN idlist LCPAREN  */
#line 298 "parser.y"
        {
		yyval = createNode(GATEDECL, 3, yyvsp[-4], NULL, yyvsp[-1]);
	}
#line 1704 "parser.tab.c"
    break;

  case 62: /* gatedecl: GATE ID LPAREN idlist RPAREN idlist LCPAREN  */
#line 302 "parser.y"
        {
		yyval = createNode(GATEDECL, 3, yyvsp[-5], yyvsp[-3], yyvsp[-1]);
	}
#line 1712 "parser.tab.c"
    break;


#line 1716 "parser.tab.c"

      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", YY_CAST (yysymbol_kind_t, yyr1[yyn]), &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;

  *++yyvsp = yyval;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */
  {
    const int yylhs = yyr1[yyn] - YYNTOKENS;
    const int yyi = yypgoto[yylhs] + *yyssp;
    yystate = (0 <= yyi && yyi <= YYLAST && yycheck[yyi] == *yyssp
               ? yytable[yyi]
               : yydefgoto[yylhs]);
  }

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYSYMBOL_YYEMPTY : YYTRANSLATE (yychar);
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
      yyerror (root, YY_("syntax error"));
    }

  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
         error, discard it.  */

      if (yychar <= YYEOF)
        {
          /* Return failure if at end of input.  */
          if (yychar == YYEOF)
            YYABORT;
        }
      else
        {
          yydestruct ("Error: discarding",
                      yytoken, &yylval, root);
          yychar = YYEMPTY;
        }
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:
  /* Pacify compilers when the user code never invokes YYERROR and the
     label yyerrorlab therefore never appears in user code.  */
  if (0)
    YYERROR;

  /* Do not reclaim the symbols of the rule whose action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

  /* Pop stack until we find a state that shifts the error token.  */
  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYSYMBOL_YYerror;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYSYMBOL_YYerror)
            {
              yyn = yytable[yyn];
              if (0 < yyn)
                break;
            }
        }

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
        YYABORT;


      yydestruct ("Error: popping",
                  YY_ACCESSING_SYMBOL (yystate), yyvsp, root);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", YY_ACCESSING_SYMBOL (yyn), yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;


/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;


#if !defined yyoverflow
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (root, YY_("memory exhausted"));
  yyresult = 2;
  goto yyreturn;
#endif


/*-------------------------------------------------------.
| yyreturn -- parsing is finished, clean up and return.  |
`-------------------------------------------------------*/
yyreturn:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval, root);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  YY_ACCESSING_SYMBOL (+*yyssp), yyvsp, root);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif

  return yyresult;
}

#line 305 "parser.y"


void yyerror(struct Node **, const char *s)	//当yacc遇到语法错误时，会回调yyerror函数，并且把错误信息放在参数s中
{
	cerr<<s<<endl;//直接输出错误信息
}
 
int main()//程序主函数，这个函数也可以放到其它.c, .cpp文件里
{
    yydebug = 1;

	const char* sFile="test/file.txt";//打开要读取的文本文件
	FILE* fp=fopen(sFile, "r");
	if(fp==NULL)
	{
		printf("cannot open %s\n", sFile);
		return -1;
	}
	extern FILE* yyin;	//yyin和yyout都是FILE*类型
	yyin=fp;//yacc会从yyin读取输入，yyin默认是标准输入，这里改为磁盘文件。yacc默认向yyout输出，可修改yyout改变输出目的
 
	printf("-----begin parsing %s\n", sFile);
	struct Node* root;
    yyparse(&root);//使yacc开始读取输入和解析，它会调用lex的yylex()读取记号
	puts("-----end parsing");
    treePrint(root);
    generate(root);
	fclose(fp);
 
	return 0;
}

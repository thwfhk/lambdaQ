%{
#include "tree.h"

extern "C"{
void yyerror(struct Node** root, const char *s);
extern int yylex(void);
#define YYDEBUG 1
// 这里yylex是在lex.yy.c中定义的
}
%}

%define api.value.type {struct Node *}
//设置value的类型
%parse-param {struct Node ** root}
//为yyparse新增一个参数

%token QREG CREG BARRIER GATE MEASURE RESET INCLUDE OPAQUE IF SIN COS TAN EXP LN SQRT PI
%token SEMICOLON COMMA LSPAREN RSPAREN LCPAREN RCPAREN EQUAL ARROW PLUS MINUS TIMES DIVIDE POWER LPAREN RPAREN REAL NNINTEGER ID  
%token OPENQASM U CX
%token UMINUS


%left PLUS MINUS
%left TIMES DIVIDE
%right POWER
%right UMINUS

%%

start:
	mainprogram
	{
		$$ = $1;
        *root = $$;
	}
	| INCLUDE mainprogram
	{
		// 其实并不支持include
	};

mainprogram:
	OPENQASM REAL SEMICOLON program
	{
		$$ = createNode(HEADER, 2, $2, $4);
		// 只记录了版本号和程序
	};

program:
	statement
	{
		$$ = createNode(STATEMENT, 1, $1);
	}
	| program statement
	{
		$$ = createNode(PROGRAM_STATEMENT, 2, $1, $2);
	};
//开始statement 部分
statement:
	decl
	{
		$$ = createNode(DECL, 1, $1);
	}
	| gatedecl goplist RCPAREN
	{
		$$ = createNode(STATEMENT_GATE_GOPLIST, 2, $1, $2);
	}
	| gatedecl RCPAREN 
	{
		$$ = createNode(STATEMENT_GATEDECL, 1, $1);
	}
	| OPAQUE ID idlist SEMICOLON
	{
		$$ = createNode(OPAQUE, 3, $1, NULL, $2);
	}
	| OPAQUE ID LPAREN RPAREN idlist SEMICOLON
	{
		$$ = createNode(OPAQUE, 3, $2, NULL, $5);
	}
	| OPAQUE ID LPAREN idlist RPAREN idlist SEMICOLON
	{
		$$ = createNode(OPAQUE, 3, $2, $4, $6);
	}
	| qop
	{
		$$ = $1;
	}
	| IF LPAREN ID EQUAL NNINTEGER RPAREN qop
	{
		$$ = createNode(IF, 3, $3, $5, $7);
	}
	| BARRIER anylist SEMICOLON
	{
		$$ = createNode(BARRIER, 1, $2);
	};
decl:
	QREG ID LSPAREN NNINTEGER RSPAREN SEMICOLON
	{
		$$ = createNode(Q_DECL, 2, $2, $4);
	}
	| CREG ID LSPAREN NNINTEGER RSPAREN SEMICOLON
	{
		$$ = createNode(C_DECL, 2, $2, $4);
	};
exp:
    PI 
	{
        $$ = $1;
    }
	| ID 
	{
        $$ = $1;
    }
	| NNINTEGER 
	{
        $$ = $1;
    }
	| REAL
    {
        $$ = $1;
    }
    | exp MINUS exp 
	{
        $$ = createNode($2->tag, 2, $1, $3);
    }
	| exp TIMES exp 
	{
        $$ = createNode($2->tag, 2, $1, $3);
    }
	| exp DIVIDE exp 
	{
        $$ = createNode($2->tag, 2, $1, $3);
    }
	| exp PLUS exp
    {
        $$ = createNode($2->tag, 2, $1, $3);
    }
	| exp POWER exp
	{
		$$ = createNode($2->tag, 2, $1, $3);
	}
    | MINUS exp %prec UMINUS
    {
        $$ = createNode(NEG, 1, $2);
    }
    | LPAREN exp RPAREN
    {
        $$ = createNode(PAREN, 1, $2);
    }
    | unaryop LPAREN exp RPAREN
    {
        $$ = createNode(EXP_UNARY, 2, $1, $3);
    };
unaryop:
    SIN 
	{
		$$ = $1;
	}
	| COS 
	{
        $$ = $1;
    }
	| TAN 
	{
        $$ = $1;
    }
	| EXP 
	{
        $$ = $1;
    }
	| LN 
	{
        $$ = $1;
    }
	| SQRT
    {
        $$ = $1;
    };

explist:
    exp
    {
        $$ = $1;
    }
    | explist COMMA exp
    {
        $$ = createNode(EXPLIST, 2, $1, $3);
    };

argument: 
	ID
	{
		$$ = $1;
	}
	| ID LSPAREN NNINTEGER RSPAREN
	{
		$$ = createNode(ARG_IDINT, 2, $1, $3);
	};

idlist:
	ID
	{
		$$ = $1;
	}
	| idlist COMMA ID
	{
		$$ = createNode(ID_LIST, 2, $1, $3);
	}

mixedlist:
	ID LSPAREN NNINTEGER RSPAREN
	{
		$$ = createNode(MIXEDLIST1, 2, $1, $3);
	}
	| mixedlist COMMA ID
	{
		$$ = createNode(MIXEDLIST2, 2, $1, $3);
	}
	| mixedlist COMMA ID LSPAREN NNINTEGER RSPAREN
	{
		struct Node* tmp = createNode(MIXEDLIST, 2, $3, $5);
		$$ = createNode(MIXEDLIST3, 2, $1, tmp);
	}
	| idlist COMMA ID LSPAREN NNINTEGER RSPAREN
	{
		struct Node* tmp = createNode(MIXEDLIST, 2, $3, $5);
		$$ = createNode(MIXEDLIST3, 2, $1, tmp);
	};
anylist:
	idlist
	{
		$$ = $1;
	}
	| mixedlist 
	{
		$$ = $1;
	};

uop:
	U LPAREN explist RPAREN argument SEMICOLON
	{
		$$ = createNode(UOP_U, 2, $3, $5);
	}
	| CX argument COMMA argument SEMICOLON
	{
		$$ = createNode(UOP_CX, 2, $2, $4);
	}
	| ID anylist SEMICOLON
	{
		$$ = createNode(UOP_ID, 3, $1, NULL, $2);
	}
	| ID LPAREN RPAREN anylist SEMICOLON
	{
		$$ = createNode(UOP_ID, 3, $1, NULL, $4);
	}
	| ID LPAREN explist RPAREN anylist SEMICOLON
	{
		$$ = createNode(UOP_ID, 3, $1, $3, $5);
	};

qop:
	uop
	{
		$$ = $1;
	}
	| MEASURE argument ARROW argument SEMICOLON
	{
		$$ = createNode(MEASURE, 2, $2, $4);
	}
	| RESET argument SEMICOLON
	{
		$$ = createNode(RESET, 1, $2);
	};

goplist:
	uop
	{
		$$ = $1;
	}
	| BARRIER idlist SEMICOLON
	{
		$$ = createNode(GOPLIST_BARRIER, 1, $2);
	}
	| goplist uop
	{
		$$ = createNode(GOPLIST_GOPUOP, 2, $1, $2);
	}
	| goplist BARRIER idlist SEMICOLON
	{
		$$ = createNode(GOPLIST_GOPBARRIER, 2, $1, $3);
	};

gatedecl:
	GATE ID idlist LCPAREN
	{
		$$ = createNode(GATEDECL, 3, $2, NULL, $3);
	}
	| GATE ID LPAREN RPAREN idlist LCPAREN
	{
		$$ = createNode(GATEDECL, 3, $2, NULL, $5);
	}
	| GATE ID LPAREN idlist RPAREN idlist LCPAREN
	{
		$$ = createNode(GATEDECL, 3, $2, $4, $6);
	};
%%

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

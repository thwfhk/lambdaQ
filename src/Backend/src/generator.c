#include "parser.tab.h"
#include "tree.h"
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

void generate(struct Node * tree){
    switch(tree->tag)
    {
        case NNINTEGER:
        case REAL:
        case ID:
        {
            printf("%s", tree->value);
            break;
        }
        case HEADER:{
            printf("OPENQASM ");
            generate(tree->cld[0]);
            printf(";\n");
            generate(tree->cld[1]);
            break;
        }
        case Q_DECL:{
            printf("qreg ");
            generate(tree->cld[0]);
            printf("[");
            generate(tree->cld[1]);
            printf("]\n");
            break;
        }
        case C_DECL:{
            printf("creg ");
            generate(tree->cld[0]);
            printf("[");
            generate(tree->cld[1]);
            printf("]\n");
            break;
        }
        case PLUS:{
            generate(tree->cld[0]);
            printf("+");
            generate(tree->cld[1]);
            break;
        }
        case MINUS:{
            generate(tree->cld[0]);
            printf("-");
            generate(tree->cld[1]);
            break;
        }
        case TIMES:{
            generate(tree->cld[0]);
            printf("*");
            generate(tree->cld[1]);
            break;
        }
        case DIVIDE:{
            generate(tree->cld[0]);
            printf("/");
            generate(tree->cld[1]);
            break;
        }
        case POWER:{
            generate(tree->cld[0]);
            printf("^");
            generate(tree->cld[1]);
            break;
        }
        case NEG:{
            printf("-");
            generate(tree->cld[0]);
            break;
        }
        case PAREN:{
            printf("(");
            generate(tree->cld[0]);
            printf(")");
            break;
        }
        case PI:{
            printf("pi");
            break;
        }
        case MIXEDLIST2:
        case MIXEDLIST3:
        case ID_LIST:
        case EXPLIST:{
            generate(tree->cld[0]);
            printf(", ");
            generate(tree->cld[1]);
            break;
        }
        case EXP_UNARY:{
            generate(tree->cld[0]);
            printf("(");
            generate(tree->cld[1]);
            printf(")");
            break;
        }
        case SIN:{
            printf("sin");
            break;
        }
        case COS:{
            printf("cos");
            break;
        }
        case TAN:{
            printf("tan");
            break;
        }
        case EXP:{
            printf("exp");
            break;
        }
        case LN:{
            printf("ln");
            break;
        }
        case SQRT:{
            printf("sqrt");
            break;
        }
        case MIXEDLIST:
        case MIXEDLIST1:
        case ARG_IDINT:{
            generate(tree->cld[0]);
            printf("[");
            generate(tree->cld[1]);
            printf("]");
            break;
        }
        case UOP_U:{
            printf("U(");
            generate(tree->cld[0]);
            printf(") ");
            generate(tree->cld[1]);
            printf(";\n");
            break;
        }
        case UOP_CX:{
            printf("CX ");
            generate(tree->cld[0]);
            printf(", ");
            generate(tree->cld[1]);
            printf(";\n");
            break;
        }
        case OPAQUE:
        case UOP_ID:{
            generate(tree->cld[0]);
            printf("(");
            if(tree->cld[1]) generate(tree->cld[1]);
            printf(") ");
            generate(tree->cld[2]);
            printf(";\n");
            break;
        }
        case MEASURE:{
            printf("measure ");
            generate(tree->cld[0]);
            printf("->");
            generate(tree->cld[1]);
            printf(";\n");
            break;
        }
        case RESET:{
            printf("reset ");
            generate(tree->cld[0]);
            printf(";\n");
            break;
        }
        case BARRIER:
        case GOPLIST_BARRIER:{
            printf("barrier ");
            generate(tree->cld[0]);
            printf(";\n");
            break;
        }
        case GOPLIST_GOPBARRIER:{
            generate(tree->cld[0]);
            printf("barrier ");
            generate(tree->cld[1]);
            printf(";\n");
            break;
        }
        case GATEDECL:{
            printf("gate ");
            generate(tree->cld[0]);
            printf("(");
            if(tree->cld[1]) generate(tree->cld[1]);
            printf(") ");
            generate(tree->cld[2]);
            printf("{\n");
            break;
        }
        case STATEMENT_GATE_GOPLIST:{
            generate(tree->cld[0]);
            generate(tree->cld[1]);
            printf("}\n");
            break;
        }
        case STATEMENT_GATEDECL:{
            generate(tree->cld[0]);
            printf("}\n");
            break;
        }
        case IF:{
            printf("if(");
            generate(tree->cld[0]);
            printf(" == ");
            generate(tree->cld[1]);
            printf(") ");
            generate(tree->cld[2]);
            break;
        }
        default:
            for(int i = 0;i < tree->ncld; i++){
                generate(tree->cld[i]);
            }
    }
}

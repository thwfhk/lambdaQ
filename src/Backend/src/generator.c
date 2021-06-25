#include "parser.tab.h"
#include "tree.h"
#include <string.h>
#include <stdlib.h>
#include <stdio.h>



extern int nphy; //物理qubit总数
extern Vertex vertex_phy[MAXN]; //physical qubits
extern vector <int> h[MAXN]; //phsical qubit constraint graph
extern vector <int> all_phy;
extern int hcnt[MAXN][MAXN];
 
extern int gcnt[MAXN][MAXN]; // logic qubit
extern Vertex vertex_logic[MAXN]; // logic qubits
extern vector <int> g[MAXN]; //logic qubit constraint graph
extern queue <int> q; // used by bfs
extern int visit[MAXN]; // 是否被bfs遍历过，同时也意味着是否被映射到一个物理qubit(因为只有被成功映射的logic qubit才会入队)
extern int used[MAXN]; // phsical qubit是否被映射过

extern int h_nd[MAXN][MAXN];//h的无向图版本
extern int pre[MAXN]; //bfs中记录前驱节点

extern int l[MAXN]; // allocaltion policy
extern int l_inv[MAXN]; 

extern int l_cur[MAXN]; // swap过后当前的l
extern int l_cur_inv[MAXN];

extern int freeze[MAXN]; // 为0 说明已经出现过，不能改变initial mapping


int transform(int u, int v);

void generate_qreg(struct Node * node){
    int offset, base;
    if(node->tag == ID){
        offset = 0;
        base = qmap[node->value];
    }
    else {
        offset = atoi(node->cld[1]->value);
        base = atoi(node->cld[0]->value);
    }
    int phy_u = l_cur[base + offset];
    printf("q[%d]", phy_u);
    return;
}
// 只需要对qreg 单独处理，creg 不需要，因为不涉及变换

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
            //这里直接声明了所有qreg
            printf("qreg q[%d];\n", qcnt);
            generate(tree->cld[1]);
            break;
        }
        case Q_DECL:{
            /*
            printf("qreg ");
            generate(tree->cld[0]);
            printf("[");
            generate(tree->cld[1]);
            printf("];\n");
            */
            // 不需要原来的声明
            break;
        }
        case C_DECL:{
            printf("creg ");
            generate(tree->cld[0]);
            printf("[");
            generate(tree->cld[1]);
            printf("];\n");
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
            generate_qreg(tree->cld[1]);
            printf(";\n");
            break;
        }
        case UOP_X:{
            printf("X ");
            generate_qreg(tree->cld[0]);
            printf(";\n");
            break;
        }
        case UOP_H:{
            printf("H ");
            generate_qreg(tree->cld[0]);
            printf(";\n");
            break;
        }
        case UOP_Y:{
            printf("Y ");
            generate_qreg(tree->cld[0]);
            printf(";\n");
            break;
        }
        case UOP_Z:{
            printf("Z ");
            generate_qreg(tree->cld[0]);
            printf(";\n");
            break;
        }
        case UOP_CX:{
            /*
            printf("cx ");
            generate(tree->cld[0]);
            printf(", ");
            generate(tree->cld[1]);
            printf(";\n");
            */
            // printf("%d %d cx\n", get_qid(tree->cld[0]), get_qid(tree->cld[1]));
            int u = get_qid(tree->cld[0]), v = get_qid(tree->cld[1]);
            transform(u, v);
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
            generate_qreg(tree->cld[0]);
            printf("->");
            generate(tree->cld[1]);
            printf(";\n");
            break;
        }
        case RESET:{
            printf("reset ");
            generate_qreg(tree->cld[0]);
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
            //都是creg 不需要特殊处理
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

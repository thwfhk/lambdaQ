#include "parser.tab.h"
#include "tree.h"
#include "graph.h"
#include <cstring>
#include <cstdlib>
#include <cstdio>
#include <string>
#include<iostream>



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


string generate_exp(struct Node * root);
int transform(int u, int v, Graph * g);

int generate_qreg(struct Node * node){
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
    return phy_u;
}
// 只需要对qreg 单独处理，creg 不需要，因为不涉及变换

void generate(struct Node * tree, Graph * g){
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
            generate(tree->cld[0], g);
            printf(";\n");
            int t = g->addVertex(OPENQASM);
            g->vertex[t].arg_char = tree->cld[0]->value;//版本号
            //这里直接声明了所有qreg
            printf("qreg q[%d];\n", qcnt);
            t = g->addVertex(QREG);
            g->vertex[t].arg_int[0] = qcnt;
            for(int i = 0;i < qcnt;i++){
                q_last[i] = t;// 初始化q_last
            }

            generate(tree->cld[1], g);

            break;
        }
        case Q_DECL:{
            /*
            printf("qreg ");
            generate(tree->cld[0], g);
            printf("[");
            generate(tree->cld[1], g);
            printf("];\n");
            */
            // 不需要原来的声明
            break;
        }
        case C_DECL:{
            printf("creg ");
            generate(tree->cld[0], g);
            printf("[");
            generate(tree->cld[1], g);
            printf("];\n");
            int t = g->addVertex(CREG);
            g->vertex[t].arg_char = tree->cld[0]->value;
            g->vertex[t].arg_int[0] = atoi(tree->cld[1]->value);
            //获取base, 这里主要为了初始化依赖关系
            int base = get_cid_from_value(tree->cld[0]->value, (char*)"0");
            int ind = atoi(tree->cld[1]->value);
            for(int i = 0;i < ind;i++){
                c_last[base + i] = t;
            }

            break;
        }
        case PLUS:{
            generate(tree->cld[0], g);
            printf("+");
            generate(tree->cld[1], g);
            break;
        }
        case MINUS:{
            generate(tree->cld[0], g);
            printf("-");
            generate(tree->cld[1], g);
            break;
        }
        case TIMES:{
            generate(tree->cld[0], g);
            printf("*");
            generate(tree->cld[1], g);
            break;
        }
        case DIVIDE:{
            generate(tree->cld[0], g);
            printf("/");
            generate(tree->cld[1], g);
            break;
        }
        case POWER:{
            generate(tree->cld[0], g);
            printf("^");
            generate(tree->cld[1], g);
            break;
        }
        case NEG:{
            printf("-");
            generate(tree->cld[0], g);
            break;
        }
        case PAREN:{
            printf("(");
            generate(tree->cld[0], g);
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
            generate(tree->cld[0], g);
            printf(", ");
            generate(tree->cld[1], g);
            break;
        }
        case EXP_UNARY:{
            generate(tree->cld[0], g);
            printf("(");
            generate(tree->cld[1], g);
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
            generate(tree->cld[0], g);
            printf("[");
            generate(tree->cld[1], g);
            printf("]");
            break;
        }
        case UOP_U:{
            printf("U(");
            generate(tree->cld[0], g);
            printf(") ");
            int t = generate_qreg(tree->cld[1]);
            printf(";\n");
            struct Node * explist = tree->cld[0];
            string *s = new string[3];
            s[2] = generate_exp(explist->cld[1]);
            s[1] = generate_exp(explist->cld[0]->cld[1]);
            s[0] = generate_exp(explist->cld[0]->cld[0]);
            //这里计算了U的三个参数的字符串表达式
            g->addRZVertex(t, (char*)s[1].c_str());
            g->addSingleQubitVertex(H, t);
            g->addRZVertex(t, (char*)"-pi/2");
            g->addSingleQubitVertex(H, t);
            g->addRZVertex(t, (char*)s[0].c_str());
            g->addSingleQubitVertex(H, t);
            g->addRZVertex(t, (char*)"pi/2");
            g->addSingleQubitVertex(H, t);
            g->addRZVertex(t, (char*)s[2].c_str());

            break;
        }
        case UOP_RZ:{
            printf("RZ(");
            generate(tree->cld[0], g);
            printf(") ");
            int u = generate_qreg(tree->cld[1]);
            printf(";\n");
            printf("rz string %s\n", tree->cld[0]->value);
            string* s = new string;
            *s = generate_exp(tree->cld[0]);
            g->addRZVertex(u, (char*)s->c_str());
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
            generate(tree->cld[0], g);
            printf(", ");
            generate(tree->cld[1], g);
            printf(";\n");
            */
            // printf("%d %d cx\n", get_qid(tree->cld[0]), get_qid(tree->cld[1]));
            int u = get_qid(tree->cld[0]), v = get_qid(tree->cld[1]);
            transform(u, v, g);
            break;
        }
        case OPAQUE:
        case UOP_ID:{
            generate(tree->cld[0], g);
            printf("(");
            if(tree->cld[1]) generate(tree->cld[1], g);
            printf(") ");
            generate(tree->cld[2], g);
            printf(";\n");
            break;
        }
        case MEASURE:{
            printf("measure ");
            //qreg 的编号
            int u = generate_qreg(tree->cld[0]);
            printf("->");
            generate(tree->cld[1], g);
            printf(";\n");
            int t = g->addVertex(MEASURE);
            g->vertex[t].arg_int[0] = u;
            g->vertex[t].arg_int[1] = atoi(tree->cld[1]->cld[1]->value);
            g->vertex[t].arg_char = tree->cld[1]->cld[0]->value;
            g->addEdge(q_last[u], t);
            int cid = get_cid(tree->cld[1]);// creg 的编号
            g->addEdge(c_last[cid], t);
            q_last[u] = c_last[cid] = t;
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
            generate(tree->cld[0], g);
            printf(";\n");
            break;
        }
        case GOPLIST_GOPBARRIER:{
            generate(tree->cld[0], g);
            printf("barrier ");
            generate(tree->cld[1], g);
            printf(";\n");
            break;
        }
        case GATEDECL:{
            printf("gate ");
            generate(tree->cld[0], g);
            printf("(");
            if(tree->cld[1]) generate(tree->cld[1], g);
            printf(") ");
            generate(tree->cld[2], g);
            printf("{\n");
            break;
        }
        case STATEMENT_GATE_GOPLIST:{
            generate(tree->cld[0], g);
            generate(tree->cld[1], g);
            printf("}\n");
            break;
        }
        case STATEMENT_GATEDECL:{
            generate(tree->cld[0], g);
            printf("}\n");
            break;
        }
        case IF:{
            //都是creg 不需要特殊处理
            printf("if(");
            generate(tree->cld[0], g);
            printf(" == ");
            generate(tree->cld[1], g);
            printf(") ");
            generate(tree->cld[2], g);
            break;
        }
        default:
            for(int i = 0;i < tree->ncld; i++){
                generate(tree->cld[i], g);
            }
    }
}
string generate_exp(struct Node *root){
    if(root->tag == ID || root->tag == NNINTEGER || root->tag == REAL) 
        return string(root->value);
    else if(root->tag == PI)
        return string("pi");
    else if(root->tag == MINUS)
        return generate_exp(root->cld[0]) + "-" + generate_exp(root->cld[1]);
    else if(root->tag == DIVIDE)
        return generate_exp(root->cld[0]) + "/" + generate_exp(root->cld[1]);
    else if(root->tag == PLUS)
        return generate_exp(root->cld[0]) + "+" + generate_exp(root->cld[1]);
    else if(root->tag == POWER)
        return generate_exp(root->cld[0]) + "^" + generate_exp(root->cld[1]);
    else if(root->tag == TIMES)
        return generate_exp(root->cld[0]) + "*" + generate_exp(root->cld[1]);
    else if(root->tag == NEG)
        return "-" + generate_exp(root->cld[0]);
    else if(root->tag == PAREN)
        return "(" + generate_exp(root->cld[0]) + ")";
    else if(root->tag == EXP_UNARY)
        return generate_exp(root->cld[0]) + "(" + generate_exp(root->cld[1]) + ")";
    else if(root->tag == SIN)
        return string("sin");
    else if(root->tag == COS)
        return string("cos");
    else if(root->tag == TAN)
        return string("tan");
    else if(root->tag == EXP)
        return string("exp");
    else if(root->tag == LN)
        return string("ln");
    else if(root->tag == SQRT)
        return string("sqrt");
    return NULL;
}

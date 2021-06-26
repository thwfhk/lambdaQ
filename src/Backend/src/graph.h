#ifndef __GH__
#define __GH__
#include <queue>
#include <cstdio>
#include "parser.tab.h"
#define MAXN 100
using namespace std;

extern int q_last[MAXN]; //记录当前每个q寄存器的最后一个vertex
extern int c_last[MAXN]; //creg last vertex

class edge{
    public:
    int u,v;
    edge* next_edge;
    edge(int u, int v, edge* next_edge):u(u), v(v), next_edge(next_edge) {};
};
class node{
    public:
    int tag;
    int arg_int[2];
    char* arg_char;
    node* cld;
    edge* in;
    edge* out;

    node (int tag): tag (tag){
        in = NULL;
        out = NULL;
        arg_int[0] = arg_int[1] = -1;
    };
    node(){
        in = NULL;
        out = NULL;
        arg_int[0] = arg_int[1] = -1;
    };
    void print(){
        if(tag == OPENQASM){
            printf("openqasm %s;\n", arg_char);
        }
        else if(tag == QREG){
            printf("qreg q[%d];\n", arg_int[0]);
        }
        else if(tag == CX){
            printf("CX q[%d], q[%d];\n", arg_int[0], arg_int[1]);
        }
        else if(tag == RZ){
            printf("RZ(%s) q[%d];\n", arg_char, arg_int[0]);
        }
        else if(tag == CREG){
            printf("creg %s[%d];\n", arg_char, arg_int[0]);
        }
        else if(tag == MEASURE){
            printf("measure q[%d]->%s[%d];\n", arg_int[0], arg_char, arg_int[1]);
        }
        else if(tag == H){
            printf("H q[%d];\n", arg_int[0]);
        }
        else if(tag == X){
            printf("X q[%d];\n", arg_int[0]);
        }
        else if(tag == Y){
            printf("Y q[%d];\n", arg_int[0]);
        }
        else if(tag == Z){
            printf("Z q[%d];\n", arg_int[0]);
        }
        else if (tag == RESET){
            printf("reset q[%d];\n", arg_int[0]);
        }
        else if(tag == IF){
            printf("if(%s==%d) ", arg_char, arg_int[0]);
            cld->print();
        }

    }
};

struct Graph{
    struct node vertex[MAXN];
    int in_degree[MAXN];
    int v_cnt;

    Graph(){
        v_cnt = 0;
        for(int i = 0;i < MAXN;i++){
            in_degree[i] = 0;
        }
    };
    int addVertex(int tag){
        vertex[v_cnt].tag = tag;
        v_cnt ++;
        return v_cnt - 1;
    }
    int addCXVertex(int u, int v){
        int t = this->addVertex(CX);
        vertex[t].arg_int[0] = u;
        vertex[t].arg_int[1] = v;
        this->addEdge(q_last[u], t);
        if(q_last[v] != q_last[u])
            this->addEdge(q_last[v], t);
        q_last[v] = q_last[u] = t;
        return t;
    }
    //所有的single qubit operation 都是这个来做
    int addSingleQubitVertex(int tag, int u){
        int t = this->addVertex(tag);
        vertex[t].arg_int[0] = u;
        this->addEdge(q_last[u], t);
        q_last[u] = t;
        return t;
    }
    int addRZVertex(int u, char *s){
        int t = addSingleQubitVertex(RZ, u);
        vertex[t].arg_char = s;
        return t;
    }
    void addEdge(int i, int j){
        vertex[i].out = new edge(i, j, vertex[i].out);
        vertex[j].in = new edge(i, j, vertex[j].in);
        in_degree[j] ++;
    }
    void toposort(){
        int d[v_cnt];
        queue<int> q;
        for(int i = 0;i < v_cnt;i++){
            d[i] = in_degree[i];
            if(d[i] == 0)
                q.push(i);
        }
        while(!q.empty()){
            int t = q.front();
            q.pop();
            //printf("id: %d\n", t);
            struct node u = vertex[t];
            u.print();
            for(edge* v = u.out;v;v = v->next_edge){
                int to = v->v;
                d[to]--;
                if(d[to] == 0){
                    q.push(to);
                }
            }
        }
    }
    void printedge(){
        for(int i = 0;i < v_cnt;i++){
            printf("node %d:\n", i);
            struct node u = vertex[i];
            u.print();
            for(edge* v = u.out;v;v = v->next_edge){
                int to = v->v;
                printf("%d ", to);
            }
            printf("\n\n");
        }
    }
    // next node of node u in qreg i (-1 if not found)
    int next_node(int t, int i){
        struct node u = vertex[t];
        for(edge* v = u.out;v;v = v->next_edge){
            int to = v->v;
            if(vertex[to].arg_int[0] == i || vertex[to].arg_int[1] == i){
                return to;
            }
        }
        return -1;
    }
    int prev_node(int t, int i){
        struct node u = vertex[t];
        for(edge* v = u.in;v;v = v->next_edge){
            int to = v->v;
            if(vertex[to].arg_int[0] == i || vertex[to].arg_int[1] == i){
                return to;
            }
        }
        return -1;
    }
};
#endif
#ifndef __GH__
#define __GH__
#include <queue>
#include <cstdio>
#include <cstring>
#include <cstdlib>
#include "parser.tab.h"
#define MAXN 100
using namespace std;

extern int q_last[MAXN]; //记录当前每个q寄存器的最后一个vertex
extern int q_first[MAXN]; //主要为了优化的时候方便作为起点
extern int c_last[MAXN]; //creg last vertex

class edge{
    public:
    int u,v;
    int qid; //用来标记这个哪条链上的边, cid 的话就使用负数了
    edge* next_edge;
    edge(int u, int v, int qid, edge* next_edge):u(u), v(v), qid(qid), next_edge(next_edge) {};
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
    node (int tag, char* theta):tag (tag){
        if(tag != RZ)
            printf("Wrong usage of constructor. It is only used for RZ\n");
        arg_char = theta;
    }
    //flag 0 表示这是control bit 
    node(int tag, bool flag, bool opt):tag(tag){
        if(tag != CX)
            printf("Wrong usage of constructor. It is only used for CX\n");
        arg_int[0] = flag;
    }
    bool operator==(const node& x) const{
        if(tag == x.tag){
            if(tag == RZ){
                //printf("%s, %s, %d\n", arg_char, x.arg_char, strcmp(arg_char, arg_char));
                //如果有任何一个是any 的话直接返回相等
                if(!strcmp(x.arg_char, "any") || !strcmp(arg_char, "any")){
                    return true;
                }
                return !strcmp(arg_char, x.arg_char);
            }
            else 
                return true;
        }
        else 
            return false;
    }
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
        else if(tag == -1){
            printf("vertex has been deleted\n");
        }
        else if(tag == 0){
            printf("vertex not exists\n");
        }
        else{
            printf("unkown tag vertex\n");
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
    //advertex 不会被直接调用，而是被包裹在一些具体的操作中
    int addVertex(int tag){
        vertex[v_cnt].tag = tag;
        v_cnt ++;
        return v_cnt - 1;
    }
    int addVertex(node * u){
        vertex[v_cnt] = *u;
        v_cnt++;
        return v_cnt - 1;
    }
    void removeEdge(edge ** prev, edge * e, int t){
        *prev = e->next_edge;
        //有时候需要设置-1因为同一条边不能计算两次
        if (t >= 0) 
            in_degree[t] --;
        delete e;
    }
    void removeVertex(int t){
        node & u = vertex[t];
        //printf("removing vertex %d now\n", t);
        //删除后继节点中的相关边
        for(edge* v = u.out;v;v = v->next_edge){
                int to = v->v;
                edge ** prev = &vertex[to].in;
                for(edge* w = vertex[to].in;w;w = w->next_edge){
                    int w_from = w->u;
                    if(w_from == t){
                        removeEdge(prev, w, to);
                    }
                    prev = &(w->next_edge);
                }
        }
        //删除前驱节点中的相关边
        for(edge* v = u.in;v;v = v->next_edge){
                int v_from = v->u;
                edge ** prev = &vertex[v_from].out;
                for(edge* w = vertex[v_from].out;w;w = w->next_edge){
                    int w_to = w->v;
                    if(w_to == t){
                        removeEdge(prev, w, t);
                    }
                    prev = &(w->next_edge);
                }
        }
        edge ** prev = &(u.out);
        for(edge* v = u.out;v;){
            edge * w = v;
            v = v->next_edge;
            removeEdge(prev, w, -1);
        }
        prev = &(u.in);
        for(edge * v = u.in;v;){
            edge * w = v;
            v = v->next_edge;
            removeEdge(prev, w, -1);
        }
       // printf("u.out after: %ld\n", (long)u.out);

        vertex[t].tag = -1;// 删除之后的节点tag就变成-1吧
    }
    int addCXVertex(int u, int v){
        int t = this->addVertex(CX);
        if(q_first[u] == 0){
            q_first[u] = t;
        }
        if(q_first[v] == 0){
            q_first[v] = t;
        }
        vertex[t].arg_int[0] = u;
        vertex[t].arg_int[1] = v;
        this->addEdge(q_last[u], t, u);
        //if(q_last[v] != q_last[u])
        //略加修改，现在可以有两条边连这同一对节点
        this->addEdge(q_last[v], t, v);
        q_last[v] = q_last[u] = t;
        return t;
    }
    //所有的single qubit operation 都是这个来做
    int addSingleQubitVertex(int tag, int u){
        int t = this->addVertex(tag);
        if(q_first[u] == 0){
            q_first[u] = t;
        }
        vertex[t].arg_int[0] = u;
        this->addEdge(q_last[u], t, u);
        q_last[u] = t;
        return t;
    }
    //这个在优化中专用的，不需要考虑边的事情
    //另外这两个操作也不需要更新q_first因为优化过程中不会使用新的qubits
    int addSingleQubitVertexOpt(node * s, int u){
        int t = this->addVertex(s);
        vertex[t].arg_int[0] = u;
        return t;
    }
    int addCXVertexOpt(int u, int v){
        int t = this->addVertex(CX);
        vertex[t].arg_int[0] = u;
        vertex[t].arg_int[1] = v;
        return t;
    }
    int addRZVertex(int u, char *s){
        int t = addSingleQubitVertex(RZ, u);
        if(q_first[u] == 0){
            q_first[u] = t;
        }
        vertex[t].arg_char = s;
        return t;
    }
    void addEdge(int i, int j, int qid){
        vertex[i].out = new edge(i, j, qid, vertex[i].out);
        vertex[j].in = new edge(i, j, qid, vertex[j].in);
        in_degree[j] ++;
    }
    void toposort(){
        int d[v_cnt];
        queue<int> q;
        for(int i = 0;i < v_cnt;i++){
            d[i] = in_degree[i];
            if(d[i] == 0 && vertex[i].tag != -1)
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
            printf("indegree :%d\n", in_degree[i]);
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
            //注意这里没有特殊判断一些特殊的操作，只考虑正常的two-single qubit gate
            if(v->qid == i){
                return to;
            }
        }
        return -1;
    }
    int prev_node(int t, int i){
        struct node u = vertex[t];
        for(edge* v = u.in;v;v = v->next_edge){
            int to = v->u;
            //printf("to is %d\n", to);
            //qreg 需要单独判断
            if(v->qid == i){
                return to;
            }
        }
        return -1;
    }
};
#endif
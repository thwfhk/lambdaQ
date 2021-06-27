#include "graph.h"
#include "tree.h"

node P = node(RZ, (char*)"pi/2"), P_dagger = node(RZ, (char*)"-pi/2");
#define MAXPAT 5
#define MAXRES 3
#define NPAT 5
node pattern[NPAT][MAXPAT] = {{node(H), P, node(H), 0, 0},
{node(H), P_dagger, node(H), 0, 0},
{node(H), node(CX), node(H), 0, 0},
{node(H), P, node(CX), P_dagger, node(H)},
{node(H), P_dagger, node(CX), P, node(H)}
};

node result[NPAT][MAXRES] = {{P_dagger, H, P_dagger},
{P, H, P},
{node(CX,0,1), 0, 0},
{P_dagger, node(CX,1,1), P},
{P, node(CX,1,1), P_dagger}};

void H_reduction(Graph * g){
    for(int qid = 0;qid < qcnt;qid++){
        //每一个都从记录的最早的gate 开始
        int cur = q_first[qid];
        printf("current qid %d\n",qid);
        // 遍历qreg i 经历的所有门
        while(cur >= 0){
            printf("current node id: %d\n", cur);
            g->vertex[cur].print();
            node cur_node = g->vertex[cur];
            int u_prev, v_prev, u_next, v_next;//分别记录匹配块的前后的节点编号
            int another_qid = -1; //和qid并行的qid
            u_prev = g->prev_node(cur, qid);
            printf("cur: %d u_prev: %d\n", cur, u_prev);
            for(int i = 0;i < NPAT;i++){
                bool match = true;
                int matching_id = cur;
                int matching_len = 0;
                printf("try to match pattern %d\n", i);
                for(int k = 0;k < MAXPAT;k++){
                    if(pattern[i][k].tag == 0){
                        break; //模式到头了
                    }
                    if(!(pattern[i][k] == g->vertex[matching_id])){
                        match = false;
                        break;
                    }
                    //cnot 需要判断方向
                    if(pattern[i][k].tag == CX){
                        if(g->vertex[matching_id].arg_int[1] != qid){
                            match = false;
                            break;
                        }
                        another_qid = g->vertex[matching_id].arg_int[0];
                        // 如果是那个特殊的pattern 
                        v_next = g->next_node(matching_id, another_qid);
                        v_prev = g->prev_node(matching_id, another_qid);
                        if(i == 2){
                            if(!(v_next >= 0 && g->vertex[v_next].tag == H && v_prev >= 0 && g->vertex[v_next].tag == H)){
                                match = false;
                                break;
                            }
                            else{
                                v_next = g->next_node(v_next, another_qid);
                                v_prev = g->prev_node(v_prev, another_qid);
                            }
                        }
                    }
                    printf("%d nodes matched\n", k+1);
                    u_next = g->next_node(matching_id, qid);
                    //时刻记录当前匹配节点的下一个节点
                    matching_len ++;
                    matching_id = g->next_node(matching_id, qid);
                }
                //第i个pattern 匹配了
                if(match){
                    printf("pattern %d mathed!wawawawa happy!!!\n", i);
                    // 先删除cur 之后匹配的节点
                    //printf("graph before************\n");
                    //g->printedge();
                    int cur_delete = cur;
                    for(int k = 0;k < matching_len;k++){
                        //printf("remove %d\n", cur_delete);
                        int next = g->next_node(cur_delete, qid);
                        //如果是Pattern 2 那么还需要多删除几个
                        if(i == 2 && g->vertex[cur_delete].tag == CX){
                            int another_qid = g->vertex[cur_delete].arg_int[0];
                            g->removeVertex(g->next_node(cur_delete, another_qid));
                            g->removeVertex(g->prev_node(cur_delete, another_qid));
                        }
                        g->removeVertex(cur_delete);
                        cur_delete = next;
                    }
                    //printf("after removed edge**********\n");
                    //g->printedge();
                    int cur_id, prev_id = u_prev;
                    //printf("u_prev: %d u_next: %d\n", u_prev, u_next);
                    for(int k = 0;k < MAXRES;k++){
                        //可能有的result 没有那么长
                        if(result[i][k].tag == 0) 
                            break;
                        if(result[i][k].tag == CX){
                            //qid 是control bit
                            if(result[i][k].arg_int[0] == 0){
                                cur_id = g->addCXVertexOpt(qid, another_qid);
                            }
                            else
                                cur_id = g->addCXVertexOpt(another_qid, qid);
                            g->addEdge(v_prev, cur_id, another_qid);
                            if(v_next >= 0)
                                g->addEdge(cur_id, v_next, another_qid);
                        }
                        else{
                            cur_id = g->addSingleQubitVertexOpt(&result[i][k], qid);
                        }
                        // 更新cur,因为图变化了，但优化还要继续
                        if(k == 0)
                            cur = cur_id;
                        g->addEdge(prev_id, cur_id, qid);
                        prev_id = cur_id;
                    }
                    //如果不是最后一个点
                    if(u_next >= 0)
                        g->addEdge(cur_id, u_next, qid);
                    //printf("graph after ***************\n");
                    //g->printedge();
                    //printf("graph after end *******\n");
                }
            }
            cur = g->next_node(cur, qid);
        }
    }
}
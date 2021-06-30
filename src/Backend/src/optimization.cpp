#include "graph.h"
#include "tree.h"
#include <string>

node P = node(RZ, (char*)"pi/2"), P_dagger = node(RZ, (char*)"-pi/2");
node RZ_any = node(RZ, (char*)"any");
#define MAXPAT 5
#define MAXRES 3
#define NPAT 5
#define MAXRULE 3

#define NRULES 3 //commute 的模式的个数
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

node single_commute_rules[NRULES][MAXRULE] = {{H, node(CX, 1, 1), H},
{node(CX, 1, 1), RZ_any, node(CX, 1, 1)},
{node(CX, 0, 1), 0, 0}};

void H_reduction(Graph * g){
    for(int qid = 0;qid < qcnt;qid++){
        //每一个都从记录的最早的gate 开始
        int cur = q_first[qid];
        //printf("current qid %d\n",qid);
        // 遍历qreg i 经历的所有门
        while(cur >= 0){
            //printf("current node id: %d\n", cur);
            //g->vertex[cur].print();
            node cur_node = g->vertex[cur];
            int u_prev, v_prev, u_next, v_next;//分别记录匹配块的前后的节点编号
            int another_qid = -1; //和qid并行的qid
            u_prev = g->prev_node(cur, qid);
            //printf("cur: %d u_prev: %d\n", cur, u_prev);
            for(int i = 0;i < NPAT;i++){
                bool match = true;
                int matching_id = cur;
                int matching_len = 0;
                //printf("try to match pattern %d\n", i);
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
                    //printf("%d nodes matched\n", k+1);
                    u_next = g->next_node(matching_id, qid);
                    //时刻记录当前匹配节点的下一个节点
                    matching_len ++;
                    matching_id = g->next_node(matching_id, qid);
                }
                //第i个pattern 匹配了
                if(match){
                    printf("Hardmard reduction: pattern %d mathed!!! wawawawa happy!!!\n", i);
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
bool isDagger(node u, node v){
    if(u.tag == RZ && v.tag == RZ){
        string u_str = string(u.arg_char), v_str = string(v.arg_char);
        return ("-" + u_str == v_str) || ("-" + v_str == u_str);
    }
    else return false;
}
void singleQubitCancellation(Graph *g){
    for(int qid = 0; qid < qcnt;qid++){
        int origin_id = q_first[qid];
        //printf("current qid %d\n",qid);
        while(origin_id >= 0){
            node origin_node = g->vertex[origin_id];
            bool early_update_origin = false;
            if(origin_node.tag == RZ){
                int cur_id = g->next_node(origin_id, qid); // 当前移动到的位置
                if(cur_id < 0)
                    break;
                int elim_id = 0;
                //printf("current origin node id: %d\n", origin_id);
                //g->vertex[origin_id].print();
                bool adj = false;
                if(isDagger(g->vertex[cur_id], g->vertex[origin_id])){
                    //如果直接就能匹配
                    elim_id = cur_id;
                    adj = true;
                }
                while(!adj){
                    bool update = false;
                    for(int rule_id = 0;rule_id < NRULES;rule_id++){
                        bool match = true;
                        //printf("try to match pattern %d\n", rule_id);
                        int matching_id = cur_id;
                        for(int matched_cnt = 0; matched_cnt < MAXRES;matched_cnt++){
                            if(single_commute_rules[rule_id][matched_cnt] == 0){
                                break;
                            }
                            if(!(single_commute_rules[rule_id][matched_cnt] == g->vertex[matching_id])){
                                match = false;
                                break;
                            }
                            if(single_commute_rules[rule_id][matched_cnt].tag == CX){ 
                                //如果 cx 方向不对
                                if(single_commute_rules[rule_id][matched_cnt].arg_int[0] != (g->vertex[matching_id].arg_int[1] == qid)){
                                    match = false;
                                    break;
                                }
                            }
                            if(rule_id == 1 && matched_cnt == 0){
                                //这里需要检查一下两个cx之间没有其他东西
                            }
                            //printf("%d nodes matched \n", matched_cnt+1);
                            matching_id = g->next_node(matching_id, qid);
                        }
                        if(match){
                            printf("Gate cancellation: pattern %d mathed! wawawawa happy!!!\n", rule_id);
                            //这里只要match上就一定选择移动
                            cur_id = matching_id;
                            update = true;
                            break;
                        }
                    }
                    if(update == false)
                        break;
                    else{
                        if(isDagger(origin_node, g->vertex[cur_id])){
                            elim_id = cur_id;
                            break;
                        }
                    }
                }
                //在这里更新orgin_id 因为马上origin node 就要被删除了
                int delete_id = origin_id;
                origin_id = g->next_node(origin_id, qid);
                early_update_origin = true;
                if(elim_id){
                    g->removeVertex(delete_id);
                    int prev = g->prev_node(delete_id, qid), next = g->next_node(delete_id, qid);
                    g->addEdge(prev, next, qid);
                    prev = g->prev_node(elim_id, qid);
                    next = g->next_node(elim_id, qid);
                    if(next >= 0)
                        g->addEdge(prev, next, qid);
                    g->removeVertex(elim_id);
                }
            }
            origin_id = g->next_node(origin_id, qid);
        }
    }
}
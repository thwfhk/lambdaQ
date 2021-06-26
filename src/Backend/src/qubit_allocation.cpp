#include "parser.tab.h"
#include "tree.h"
#include "graph.h"




bool cmp(const Vertex& v1, const Vertex& v2){
    return v1.out_degree > v2.out_degree;
}

int nphy; //物理qubit总数
Vertex vertex_phy[MAXN]; //physical qubits
vector <int> h[MAXN]; //phsical qubit constraint graph
vector <int> all_phy;
int hcnt[MAXN][MAXN];

int gcnt[MAXN][MAXN]; // logic qubit
Vertex vertex_logic[MAXN]; // logic qubits
vector <int> g[MAXN]; //logic qubit constraint graph
queue <int> q; // used by bfs
int visit[MAXN]; // 是否被bfs遍历过，同时也意味着是否被映射到一个物理qubit(因为只有被成功映射的logic qubit才会入队)
int used[MAXN]; // phsical qubit是否被映射过

int h_nd[MAXN][MAXN];//h的无向图版本
int pre[MAXN]; //bfs中记录前驱节点

int l[MAXN]; // allocaltion policy
int l_inv[MAXN]; 

int l_cur[MAXN]; // swap过后当前的l
int l_cur_inv[MAXN];

int freeze[MAXN]; // 为0 说明已经出现过，不能改变initial mapping

// 在vec给定的物理qubit中找到和degree最接近的物理qubit 
int search(int degree, vector <int> &vec){
    int min = nphy, ans;
    int tmp = vec.size();
    for(int i = 0;i < tmp;i++){
        int u = vec[i];
        // 只考虑没有映射过的physical qubits
        if(!used[u] && abs(degree-vertex_phy[u].out_degree) < min){
            ans = u;
            min = abs(degree-vertex_phy[u].out_degree);
        }
    }
    if(min != nphy)
        return ans;
    else
        return -1;
}

void bfs(int root){
    int qubit_mapped = search(vertex_logic[root].out_degree, all_phy);
    if(qubit_mapped == -1){
        fprintf(stderr, "bfs failed on root\n");
        exit(1);
    }
    used[qubit_mapped] = 1; // 物理qubit被标记
    l[root] = qubit_mapped;
    l_inv[qubit_mapped] = root;
    visit[root] = 1;
    q.push(root);
    while(!q.empty()){
        int cur = q.front();
        int phy_cur = l[cur]; //当前节点映射到的物理qubit
        q.pop();
        int tmp = g[cur].size();

        for(int i = 0; i < tmp;i++){
            int v = g[cur][i];
            
            if(!visit[v]){            
                qubit_mapped = search(vertex_logic[v].out_degree, h[phy_cur]); //从phy_cur所有儿子中找到可以被映射的物理qubit
                if(qubit_mapped == -1 ) 
                    continue; //如果没有找到，那么不会继续扩展
                l[v] = qubit_mapped;
                l_inv[qubit_mapped] = v;
                used[qubit_mapped] = 1;
                visit[v] = 1;
                q.push(v);
            }
        }
    }
}
//这个bfs是用来计算最短路径的，使用的图应该是物理qubit的无向图
int bfs_minpath(int s, int t){
    queue<int> q;
    pre[t] = -1;
    for(int i = 0;i < nphy;i++)
        visit[i] = 0;
    visit[s] = 1;
    q.push(s);
    while(!q.empty()){
        int cur = q.front();
        //printf("in bfs_min %d\n", cur);
        q.pop();
        if(hcnt[t][cur]){
            pre[t] = cur;
            break;
        }
        for(int i = 0;i < nphy;i++){
            if(!h_nd[cur][i] || visit[i])
                continue;
            q.push(i);
            visit[i] = 1;
            pre[i] = cur;
        }
    }
    if(pre[t] == -1)
        return -1;
    while(pre[t] != s){
        //printf("min_path: %d\n", t);
        t = pre[t];
    }
    return t;
}

void swap(int & x, int & y){
    int tmp = x;
    x = y;
    y = tmp;
    return ;
}

// 输入是物理 qubit 
void swap_qubit(int phy_u, int phy_v, Graph * g){
    int u = l_cur_inv[phy_u], v = l_cur_inv[phy_v];
    printf("// swap %d %d\n", u, v);                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       
    if(!freeze[u] && !freeze[v]){
        //如果没有冻结的活可以直接更改l
        swap(l[u], l[v]);
        swap(l_inv[phy_u], l_inv[phy_v]);
        swap(l_cur_inv[phy_u], l_cur_inv[phy_v]);
        swap(l_cur[u], l_cur[v]);
    }
    else{
        swap(l_cur_inv[phy_u], l_cur_inv[phy_v]);
        swap(l_cur[u], l_cur[v]);
        if(hcnt[phy_u][phy_v]){
            if(hcnt[phy_v][phy_u]){
                printf("CX q[%d], q[%d];\n", phy_u, phy_v);
                g->addCXVertex(phy_u, phy_v);
                printf("CX q[%d], q[%d];\n", phy_v, phy_u);
                g->addCXVertex(phy_v, phy_u);
                printf("CX q[%d], q[%d];\n", phy_u, phy_v);
                g->addCXVertex(phy_u, phy_v);
            }
            else
                swap(phy_u, phy_v);
        }
        printf("CX q[%d], q[%d];\n", phy_v, phy_u);
        g->addCXVertex(phy_v, phy_u);
        printf("H q[%d];\n", phy_v);
        g->addSingleQubitVertex(H, phy_v);
        printf("H q[%d];\n", phy_u);
        g->addSingleQubitVertex(H, phy_u);
        printf("CX q[%d], q[%d];\n", phy_v, phy_u);
        g->addCXVertex(phy_v, phy_u);
        printf("H q[%d];\n", phy_v);
        g->addSingleQubitVertex(H, phy_v);
        printf("H q[%d];\n", phy_u);
        g->addSingleQubitVertex(H, phy_u);
        printf("CX q[%d], q[%d];\n", phy_v, phy_u);
        g->addCXVertex(phy_v, phy_u);
        freeze[u] = 1;
        freeze[v] = 1;
    }
    return ;
}


int transform(int u, int v, Graph* g){
// 更改ast中的argument 一定要在这里
    int phy_u = l_cur[u], phy_v = l_cur[v];
    if(hcnt[phy_u][phy_v]) {
        freeze[u] = 1;
        freeze[v] = 1;
        printf("CX q[%d], q[%d];\n",phy_u, phy_v);
        g->addCXVertex(phy_u, phy_v);

        return 1; //约束满足
    }
    if(gcnt[u][v] > 1){
        int t = bfs_minpath(phy_v, phy_u);
        if(t == -1){
            fprintf(stderr, "This initial mapping is invalid\n");
            exit(1);
        }
        swap_qubit(phy_v, t, g); //注意需要freeze所有经过的节点，会改变l and l_cur, freeze
        return transform(u, v, g);
    }
    else if(hcnt[phy_v][phy_u]){
        printf("// inverse cnot %d %d\n", phy_u, phy_v);
        printf("H q[%d];\n", phy_v);
        g->addSingleQubitVertex(H, phy_v);
        printf("H q[%d];\n", phy_u);
        g->addSingleQubitVertex(H, phy_u);
        printf("CX q[%d], q[%d];\n", phy_v, phy_u);
        g->addCXVertex(phy_u, phy_v);
        printf("H q[%d];\n", phy_v);
        g->addSingleQubitVertex(H, phy_v);
        printf("H q[%d];\n", phy_u);
        g->addSingleQubitVertex(H, phy_u);
        return 1;
    }
    else {
        int flag = 0;
        for(int i = 0;i < nphy;i++){ 
            if(hcnt[phy_u][i] && hcnt[i][phy_v]){
                printf("// bridge %d %d %d\n", phy_u, i, phy_v);
                printf("CX q[%d], q[%d];\n", i, phy_v);
                g->addCXVertex(i, phy_v);
                printf("CX q[%d], q[%d];\n", phy_u, i);
                g->addCXVertex(phy_u, i);
                printf("CX q[%d], q[%d];\n", i, phy_v);
                g->addCXVertex(i, phy_v);
                printf("CX q[%d], q[%d];\n", phy_u, i);
                g->addCXVertex(phy_u, i);
                flag = 1;
                freeze[l_cur_inv[i]] = 1;
                return 1;
            }
        }
        if(!flag){
            int t = bfs_minpath(phy_v, phy_u);
            if(t == -1){
                fprintf(stderr, "This initial mapping is invalid\n");
                exit(1);
            }
            swap_qubit(phy_v, t, g); //注意需要freeze所有经过的节点，会改变l and l_cur, freeze
            return transform(u, v, g);
        }
    }
    return 0;

    freeze[u] = 1;
    freeze[v] = 1;
}

bool qubit_allocation(vector<Constraint> &Phi){
    //读入计算机的结构

    FILE* fp=fopen("test/structure.txt", "r");
    fscanf(fp, "%d", &nphy);
    int u, v;
    for(int i = 0;i < nphy; i++){
        vertex_phy[i].id = i;
    }
    while(fscanf(fp, "%d %d", &u, &v) != EOF){
        vertex_phy[u].out_degree ++;
        h[u].push_back(v);
        hcnt[u][v] ++;
        h_nd[u][v] = h_nd[v][u] = 1;
        // printf("phy: %d %d\n", u, v);
    }

    //定义一个包含所有物理qubit的vector之后会用到
    for(int i = 0;i < nphy;i++)
        all_phy.push_back(i);


    for(int i = 0;i < qcnt;i++) 
        vertex_logic[i].id = i;
    int n_con = Phi.size();
	for(int i = 0; i < n_con; i++){
        int u = Phi[i].u, v  = Phi[i].v;
		// printf("%d %d\n", Phi[i].u, Phi[i].v);
        if(gcnt[u][v] == 0){
            g[u].push_back(v);
            vertex_logic[u].out_degree ++; //注意degree 不需要重复计数
        }
        gcnt[u][v] ++;
    }
    sort(vertex_logic, vertex_logic+qcnt, cmp);

    for(int i = 0;i < qcnt;i++){
        printf("id: %d degree: %d\n", vertex_logic[i].id, vertex_logic[i].out_degree);
        int v = vertex_logic[i].id;
        if(visit[v] == 0) 
            bfs(v);
    }

    for(int i = 0; i < qcnt;i++){
        printf("logic qubit %d is mapped to physical qubit %d\n", i, l[i]);
        l_cur[i] = l[i];    
    }
    for(int i = 0;i < nphy;i++){
        l_cur_inv[i] = l_inv[i];
       // printf("l inv %d\n", l_inv[i]);
    }

    //printf("%d****", bfs_minpath(0,3));
    /*
    for(int i = 0;i < n_con;i++){
        int u = Phi[i].u, v  = Phi[i].v;
        printf("tranforming %d %d...\n", u, v);
        transform(u, v, g);
    }
    for(int i = 0;i < qcnt;i++){
        printf("logic qubit %d is mapped to physical qubit %d\n", i, l[i]);
    }
    */
   return 1;
}


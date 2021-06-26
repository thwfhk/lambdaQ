#include <iostream>//使用C++库
#include <string>
#include <stdio.h>//printf和FILE要用的
#include <stdarg.h>
#include <map>
#include <vector>
#include <algorithm>
#include <queue>
#include <cmath>

using namespace std;

#define MAXN 100
struct Node
{
    int tag;
    int info; // 附加信息，对于cnot节点就表明它需要的Tranformation
    char* value;
 
    struct Node *cld[10];
    int ncld;
};

struct Constraint
{
    int u,v;
    struct Node* nd;
    Constraint(int u, int v, struct Node* nd): u(u), v(v), nd(nd) {}
};

struct Vertex{
    int id;
    int out_degree;
};
 
struct Node *createLeaf(int tag, char *text);
struct Node *createNode(int tag, int ncld, ...);
// struct Node *createEmpty();
void treePrint(struct Node * nd);
void generate(struct Node * tree);

bool qubit_allocation(vector<Constraint> &Phi);

int get_qid(struct Node *);
int get_cid(struct Node *);


extern map <string, int> qmap;
extern map <string, int> cmap;
extern int qcnt, ccnt; //记录当前使用的bit个数
extern vector <Constraint> Phi;

enum yyNTtype
{
    HEADER = 400,
    STATEMENT = 401,
    PROGRAM_STATEMENT = 402,
    DECL = 403,
    Q_DECL = 404,
    C_DECL = 405,
    NEG = 406,
    EXP_UNARY = 407,
    EXPLIST = 408,
    PAREN = 409,
    ARG_IDINT = 410,
    ID_LIST = 411,
    MIXEDLIST = 412,
    MIXEDLIST1 = 413,
    MIXEDLIST2 = 414,
    MIXEDLIST3 = 415,
    UOP_U = 416,
    UOP_CX = 417,
    UOP_ID = 418,
    GOPLIST_GOPUOP = 419,
    GOPLIST_BARRIER = 420,
    GOPLIST_GOPBARRIER = 421,
    GATEDECL = 422,
    STATEMENT_GATEDECL = 423,
    STATEMENT_GATE_GOPLIST = 424,
    UOP_X = 425,
    UOP_Y = 426,
    UOP_Z = 427,
    UOP_H = 428,
    UOP_RZ = 429,

};
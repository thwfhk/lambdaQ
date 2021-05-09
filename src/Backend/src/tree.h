#include <iostream>//使用C++库
#include <string>
#include <stdio.h>//printf和FILE要用的
#include <stdarg.h>

using namespace std;

struct Node
{
    int tag;
    char* value;
 
    struct Node *cld[10];
    int ncld;
};
 
struct Node *createLeaf(int tag, char *text);
struct Node *createNode(int tag, int ncld, ...);
// struct Node *createEmpty();
void treePrint(struct Node * nd);
void generate(struct Node * tree);
 
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

};
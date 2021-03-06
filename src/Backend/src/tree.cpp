#include "parser.tab.h"
#include "tree.h"
#include <cstring>
#include <cstdlib>
#include <cstdio>
 
map <string, int> qmap;
map <string, int> cmap;
int qcnt = 0, ccnt = 0; //记录当前使用的bit个数
vector <Constraint> Phi;
const char * structure_file;
struct Node *createLeaf(int tag, char *text)
{
	struct Node *nd=(struct Node*)malloc(sizeof(struct Node));
	nd->tag=tag;
	//printf("createleaf %d %s\n", tag, text);

	if(tag==ID || tag==REAL || tag==NNINTEGER)
	{
		nd->value=(char*)malloc(sizeof(char)*strlen(text));
		strcpy(nd->value,text);
	}
	else
		nd->value=NULL;
	return nd;
}
 
struct Node *createNode(int tag, int ncld, ...)
{
	va_list cld;
	int i;
	va_start(cld, ncld);
	struct Node *nd=(struct Node*)malloc(sizeof(struct Node));
	nd->ncld=ncld;
	nd->tag=tag;
	nd->value=NULL;
	for(i=0; i<nd->ncld; i++)
		(nd->cld)[i]= va_arg(cld, struct Node*);
	return nd;
}
/*
struct Node *createEmpty()
{
	struct Node *nd=(struct Node*)malloc(sizeof(struct Node));
	nd->ncld=0;
	nd->tag=EPS;
	nd->value=NULL;
 
	return nd;
}
*/
 
void treePrintLevel(struct Node *nd, int lvl)
{
	int i;
	if(nd!=NULL)
	{
		for(i=0; i<4*lvl; i++)
			printf("-");
		
		if(nd->value==NULL)
			printf("<%d,->\n", nd->tag);
		else 
			printf("<%d,%s>\n", nd->tag, nd->value);
		
		for (i=0; i<nd->ncld; i++) {  
			treePrintLevel((nd->cld)[i], lvl+1);
		}
	}
}
 
void treePrint(struct Node *nd)
{
	treePrintLevel(nd, 0);
}

int get_qid(struct Node * x){
	if(x->tag == ID)
		return qmap[x->value];
	else{
		string id = x->cld[0]->value;
		int n = atoi(x->cld[1]->value);
		return qmap[id] + n;
	}
}
int get_cid(struct Node * x){
	if(x->tag == ID)
		return cmap[x->value];
	else{
		string id = x->cld[0]->value;
		int n = atoi(x->cld[1]->value);
		return cmap[id] + n;
	}
}
int get_cid_from_value(char * s, char * ind){
	string id = s;
	int n = atoi(ind);
	return cmap[id] + n;
}

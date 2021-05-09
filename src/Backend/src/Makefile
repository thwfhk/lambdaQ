LEX=flex
YACC=bison
CC=g++
OBJECT=parser #生成的目标文件


.PHONY:all
all: lex.yy.c parser.tab.h parser.tab.c

parser.tab.c  parser.tab.h: parser.y
# bison使用-d参数编译.y文件
	$(YACC) -d --debug --verbose -Wcounterexamples parser.y
 
lex.yy.c: lex.l
	$(LEX) lex.l

LEX=flex
YACC=bison
CC=g++
OBJECT=parser #生成的目标文件
VPATH = src
SRC = src
SUBDIRS = ./src

$(OBJECT): parser.tab.c parser.tab.h lex.yy.c tree.c tree.h generator.c
	$(CC) $(SRC)/tree.c $(SRC)/lex.yy.c $(SRC)/parser.tab.c $(SRC)/generator.c -o $(OBJECT)
	@./$(OBJECT) #编译后立刻运行

parser.tab.c parser.tab.h lex.yy.c: parser.y lex.l tree.h tree.c
	@list='$(SUBDIRS)';for subdir in $$list; do \
				cd $$subdir && make; \
		done

clean:
	@rm -f $(OBJECT)  *.o
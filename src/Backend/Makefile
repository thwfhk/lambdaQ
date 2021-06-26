LEX=flex
YACC=bison
CC=g++
OBJECT=parser #生成的目标文件
VPATH = src
SRC = src
SUBDIRS = ./src

$(OBJECT): parser.tab.c parser.tab.h lex.yy.c tree.cpp tree.h generator.cpp qubit_allocation.cpp graph.cpp graph.h
	$(CC) $(SRC)/tree.cpp $(SRC)/lex.yy.c $(SRC)/parser.tab.c $(SRC)/generator.cpp $(SRC)/qubit_allocation.cpp $(SRC)/graph.cpp -o $(OBJECT)
	@./$(OBJECT) #编译后立刻运行

parser.tab.c parser.tab.h lex.yy.c: parser.y lex.l tree.h tree.cpp
	@list='$(SUBDIRS)';for subdir in $$list; do \
				cd $$subdir && make; \
		done

clean:
	@rm -f $(OBJECT)  *.o
# 定义预处理查找的目录
# 定义编译器的安装目录
INCDIR=/tmp/include
BINDIR=/tmp

HSRCS= data.h decl.h defs.h incdir.h
SRCS= cg.c decl.c expr.c gen.c main.c misc.c \
	opt.c scan.c stmt.c sym.c tree.c types.c

mycc: $(SRCS) $(HSRCS)
	cc -o mycc -g -Wall $(SRCS)


incdir.h:
	echo "#define INCDIR \"$(INCDIR)\"" > incdir.h

install: mycc
	mkdir -p $(INCDIR)
	cp -a include/. $(INCDIR)
	cp mycc $(BINDIR)
	chmod +x $(BINDIR)/mycc

clean:
	rm -f mycc mycc[0-9] *.o *.s out a.out incdir.h

test: install tests/runtests
	(cd tests; chmod +x runtests; ./runtests)

# 用自己编译自己的编译器来跑测试
test0: install tests/runtests0 mycc0
	(cd tests; chmod +x runtests0; ./runtests0)


# 继续编译自己
triple: mycc1
	size mycc[01]

# 编译自己上瘾了
quad: mycc2
	size mycc[012]

mycc2: mycc1 $(SRCS) $(HSRCS)
	./mycc1 -o mycc2 $(SRCS)

mycc1: mycc0 $(SRCS) $(HSRCS)
	./mycc0 -o mycc1 $(SRCS)

mycc0: install $(SRCS) $(HSRCS)
	./mycc  -o mycc0 $(SRCS)

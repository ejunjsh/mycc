# Define the location of the include directory
# and the location to install the compiler binary
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

# Run the tests with the
# compiler that compiled itself
test0: install tests/runtests0 mycc0
	(cd tests; chmod +x runtests0; ./runtests0)


# Try to do the triple test
triple: mycc1
	size mycc[01]

# Paranoid: quadruple test
quad: mycc2
	size mycc[012]

mycc2: mycc1 $(SRCS) $(HSRCS)
	./mycc1 -o mycc2 $(SRCS)

mycc1: mycc0 $(SRCS) $(HSRCS)
	./mycc0 -o mycc1 $(SRCS)

mycc0: install $(SRCS) $(HSRCS)
	./mycc  -o mycc0 $(SRCS)

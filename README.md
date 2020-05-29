# mycc

一个c的编译器, 基本代码来自参考里面的代码，修复部分bug，还有加上中文注释😄

基本实现了c的所有语法，应该说是c的一个子集,而且能够自举（自己编译自己）

这个编译器目前只支持linux（有时间看看能不能跑在macos）

## 编译

linux平台

    make install

非linux平台

    docker run -it -v $(pwd):/opt/tmp/ --rm -w /opt/tmp gcc make install

详细make命令，可以看[Makefile](https://github.com/ejunjsh/mycc/blob/master/Makefile)


## 使用帮助

````shell
# ./mycc
Usage: ./mycc [-vcSTM] [-o outfile] file [file ...]
       -v give verbose output of the compilation stages
       -c generate object files but don't link them
       -S generate assembly files but don't link them
       -T dump the AST trees for each input file
       -M dump the symbol table for each input file
       -o outfile, produce the outfile executable file
````

## 例子

````c
#include <stdio.h>
#include <stdlib.h>

struct a{
    union {
        int b
    } c;
    long k
};

struct a xx;

int kk=1;

void  main(){
    int* kkp=&kk;
    *kkp=*kkp+1;
    xx.c.b=1;
    xx.c.b=xx.c.b+1;
    struct a* sp=&xx;
    sp->k=sp->k+2;
    printf("hello world mycc! test output %d %d %d \n",*kkp,xx.c.b,sp->k);
}
````

编译

    ./mycc -o hello examples/helloworld.c # 编译
    ./hello # 执行
    hello world mycc! test output 2 2 2 # 输出    

## 自举测试

````shell
# make clean quad
rm -f mycc mycc[0-9] *.o *.s out a.out incdir.h
echo "#define INCDIR \"/tmp/include\"" > incdir.h
cc -o mycc -g -Wall cg.c decl.c expr.c gen.c main.c misc.c opt.c scan.c stmt.c sym.c tree.c types.c
mkdir -p /tmp/include
cp -a include/. /tmp/include
cp mycc /tmp
chmod +x /tmp/mycc
./mycc  -o mycc0 cg.c decl.c expr.c gen.c main.c misc.c opt.c scan.c stmt.c sym.c tree.c types.c
./mycc0 -o mycc1 cg.c decl.c expr.c gen.c main.c misc.c opt.c scan.c stmt.c sym.c tree.c types.c
./mycc1 -o mycc2 cg.c decl.c expr.c gen.c main.c misc.c opt.c scan.c stmt.c sym.c tree.c types.c
size mycc[012]
   text    data     bss     dec     hex filename
  97560    2972      48  100580   188e4 mycc0 
  97560    2972      48  100580   188e4 mycc1
  97560    2972      48  100580   188e4 mycc2
````

可以看到自己编译自己的mycc0，mycc1，mycc2大小一致，证明成功

````shell
# make clean test 
rm -f mycc mycc[0-9] *.o *.s out a.out incdir.h
echo "#define INCDIR \"/tmp/include\"" > incdir.h
cc -o mycc -g -Wall cg.c decl.c expr.c gen.c main.c misc.c opt.c scan.c stmt.c sym.c tree.c types.c
mkdir -p /tmp/include
cp -a include/. /tmp/include
cp mycc /tmp
chmod +x /tmp/mycc
(cd tests; chmod +x runtests; ./runtests)
input001.c: OK
input002.c: OK
input003.c: OK
# 省略
input149.c: OK
input150.c: OK
````
这个是gnu c 编译器编译出来的mycc的test case成功

````shell
# make clean test0
rm -f mycc mycc[0-9] *.o *.s out a.out incdir.h
echo "#define INCDIR \"/tmp/include\"" > incdir.h
cc -o mycc -g -Wall cg.c decl.c expr.c gen.c main.c misc.c opt.c scan.c stmt.c sym.c tree.c types.c
mkdir -p /tmp/include
cp -a include/. /tmp/include
cp mycc /tmp
chmod +x /tmp/mycc
./mycc  -o mycc0 cg.c decl.c expr.c gen.c main.c misc.c opt.c scan.c stmt.c sym.c tree.c types.c
(cd tests; chmod +x runtests0; ./runtests0)
input001.c: OK
input002.c: OK
input003.c: OK
# 省略
input149.c: OK
input150.c: OK
````
这个是mycc编译器编译出来的mycc0的test case成功

## 参考

https://github.com/DoctorWkt/acwj


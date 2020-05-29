# mycc

一个c的编译器, 基本代码来自参考里面的代码，修复部分bug，还有加上中文注释😄

基本实现了c的所有语法，应该说是c的一个子集

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

## 参考

https://github.com/DoctorWkt/acwj


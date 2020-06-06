#include <stdio.h>
#include <stdlib.h>

struct a{
    union {
        int b
    } c; // 目前只支持嵌套一个struct/union声明，而且只能是第一个成员😭，或者在外面声明好，再到里面用吧😭
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
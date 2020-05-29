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
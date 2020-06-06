#include <stdio.h>
struct foo { int x; char y; long z; }; 
typedef struct foo blah;

union bar { int x; char y; long z; }; 
typedef union bar haha;

// 符号表结构
struct symtable {
  union {
    int st_endlabel; // 对于函数，表示为结束标签号
    int st_posn; // 对于本地变量，表现相对于栈底的偏移量，是个负数
  } stu;
  char *name;			// 符号表名称
  int type;			// 当前符号表的类型
  struct symtable *ctype;	// 如果是struct/union, 指向他们的类型
  int stype;			// 结构化类型
  int class;			// 存储类
  int size;			// 当前符号表大小
  int nelems;			// 函数: # 参数个数. 数组: # 元素个数
  int *initlist;		// 初始值列表
  struct symtable *next;	// 列表的下个符号表
  struct symtable *member;	// 函数，结构体，联合体或者枚举的第一个成员
};				

// 抽象语法树结构
struct ASTnode {
  union{
    int a_intvalue; // 对于A_INTLIT，表示整型的值
    int a_size; //  对于A_SCALE，表示倍数
  } atu;
  int op;			// 树的操作
  int type;			// 树生成的表达式类型
  struct symtable *ctype;	// 如果是struct/union, 指向他们的类型
  int rvalue;			// 如果是右值，就设置为1
  struct ASTnode *left;		// 左中右子树
  struct ASTnode *mid;
  struct ASTnode *right;
  struct symtable *sym;		// 指向符号表
  int linenum;			//  这个节点所在的行号
};

int main() {
  printf("%ld\n", sizeof(char));
  printf("%ld\n", sizeof(int));
  printf("%ld\n", sizeof(long));
  printf("%ld\n", sizeof(char *));
  printf("%ld\n", sizeof(blah));
  printf("%ld\n", sizeof(haha));
  printf("%ld\n", sizeof(struct symtable));
  printf("%ld\n", sizeof(struct ASTnode));
  return(0);
}

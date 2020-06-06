#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include "incdir.h"

// 结构和枚举定义
// Copyright (c) 2019 Warren Toomey, GPL3

enum {
  TEXTLEN = 512			// 变量标识符的最大长度
};

// 命令和默认文件名
#define AOUT "a.out"
#define ASCMD "as -g -o "
#define LDCMD "cc -g -o "
#define CPPCMD "cpp -nostdinc -isystem "

// Token类型
enum {
  T_EOF,

  // 二元操作符
  T_ASSIGN, T_ASPLUS, T_ASMINUS,
  T_ASSTAR, T_ASSLASH, T_ASMOD,
  T_QUESTION, T_LOGOR, T_LOGAND,
  T_OR, T_XOR, T_AMPER,
  T_EQ, T_NE,
  T_LT, T_GT, T_LE, T_GE,
  T_LSHIFT, T_RSHIFT,
  T_PLUS, T_MINUS, T_STAR, T_SLASH, T_MOD,

  // 其他操作符
  T_INC, T_DEC, T_INVERT, T_LOGNOT,

  // 类型关键字
  T_VOID, T_CHAR, T_INT, T_LONG,

  // 其他关键字
  T_IF, T_ELSE, T_WHILE, T_FOR, T_RETURN,
  T_STRUCT, T_UNION, T_ENUM, T_TYPEDEF,
  T_EXTERN, T_BREAK, T_CONTINUE, T_SWITCH,
  T_CASE, T_DEFAULT, T_SIZEOF, T_STATIC,

  // 结构化相关Token
  T_INTLIT, T_STRLIT, T_SEMI, T_IDENT,
  T_LBRACE, T_RBRACE, T_LPAREN, T_RPAREN,
  T_LBRACKET, T_RBRACKET, T_COMMA, T_DOT,
  T_ARROW, T_COLON
};

// Token结构
struct token {
  int token;			// Token类型，来自上面枚举列表
  char *tokstr;			// Token对应的字符名称
  int intvalue;			// 对于类型T_INTLIT的整型值
};

// 抽象语法树节点类型，前面几行是跟token类型前几行一一对应
enum {
  A_ASSIGN = 1, A_ASPLUS, A_ASMINUS, A_ASSTAR,			//  1
  A_ASSLASH, A_ASMOD, A_TERNARY, A_LOGOR,			//  5
  A_LOGAND, A_OR, A_XOR, A_AND, A_EQ, A_NE, A_LT,		//  9
  A_GT, A_LE, A_GE, A_LSHIFT, A_RSHIFT,				// 16
  A_ADD, A_SUBTRACT, A_MULTIPLY, A_DIVIDE, A_MOD,		// 21
  A_INTLIT, A_STRLIT, A_IDENT, A_GLUE,				// 26
  A_IF, A_WHILE, A_FUNCTION, A_WIDEN, A_RETURN,			// 30
  A_FUNCCALL, A_DEREF, A_ADDR, A_SCALE,				// 35
  A_PREINC, A_PREDEC, A_POSTINC, A_POSTDEC,			// 39
  A_NEGATE, A_INVERT, A_LOGNOT, A_TOBOOL, A_BREAK,		// 43
  A_CONTINUE, A_SWITCH, A_CASE, A_DEFAULT, A_CAST		// 48
};

// 原始类型，低四位表示间接的程度
// 例如整型 int的类型值是48，其二进制是100 0000，int* 为100 0001，int** 为100 0002，等等
enum {
  P_NONE, P_VOID = 16, P_CHAR = 32, P_INT = 48, P_LONG = 64,
  P_STRUCT=80, P_UNION=96
};

// 结构化类型
enum {
  S_VARIABLE, S_FUNCTION, S_ARRAY
};

// 存储类
enum {
  C_GLOBAL = 1,			//全局可见符号
  C_LOCAL,			// 本地可见符号
  C_PARAM,			// 本地可见函数参数
  C_EXTERN,			// 外部全局可见符号
  C_STATIC,			// 静态符号，当前文件可见
  C_STRUCT,			// 表示结构体（struct）
  C_UNION,			// 表示联合体（union）
  C_MEMBER,			// 结构体或联合体成员
  C_ENUMTYPE,			// 枚举类型的名字
  C_ENUMVAL,			// 枚举值的名字
  C_TYPEDEF			// typedef的名字
};

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

enum {
  NOREG = -1,			// 表示抽象语法生成的函数没有寄存器返回
  NOLABEL = 0			// 表示没有标签号要传给genAST()			
};
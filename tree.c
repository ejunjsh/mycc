#include "defs.h"
#include "data.h"
#include "decl.h"

// 创建并返回一个抽象语法树节点
struct ASTnode *mkastnode(int op, int type,
			  struct symtable *ctype,
			  struct ASTnode *left,
			  struct ASTnode *mid,
			  struct ASTnode *right,
			  struct symtable *sym, int intvalue) {
  struct ASTnode *n;

  // 给新节点分配内存
  n = (struct ASTnode *) malloc(sizeof(struct ASTnode));
  if (n == NULL)
    fatal("Unable to malloc in mkastnode()");

  // 复制数据到这个新节点并返回
  n->op = op;
  n->type = type;
  n->ctype = ctype;
  n->left = left;
  n->mid = mid;
  n->right = right;
  n->sym = sym;
  n->a_intvalue = intvalue;
  n->linenum= 0;
  return (n);
}
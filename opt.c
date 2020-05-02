#include "defs.h"
#include "data.h"
#include "decl.h"

// 抽象语法树优化代码
// Copyright (c) 2019 Warren Toomey, GPL3

// 折叠一颗抽象语法树，把一个二元操作符和它两个A_INTLIT小孩去掉
// 返回原来的树，或者只是一个新的叶子节点
static struct ASTnode *fold2(struct ASTnode *n) {
  int val, leftval, rightval;

  // 从每个小孩那里获得值
  leftval = n->left->a_intvalue;
  rightval = n->right->a_intvalue;

  // 执行二元操作
  // 对于其他不支持的操作
  // 返回原来的树
  switch (n->op) {
    case A_ADD:
      val = leftval + rightval;
      break;
    case A_SUBTRACT:
      val = leftval - rightval;
      break;
    case A_MULTIPLY:
      val = leftval * rightval;
      break;
    case A_DIVIDE:
      // 不会去除0
      if (rightval == 0)
	return (n);
      val = leftval / rightval;
      break;
    default:
      return (n);
  }

    // 返回一个含新值的叶子节点
  return (mkastleaf(A_INTLIT, n->type, NULL, NULL, val));
}

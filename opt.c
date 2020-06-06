#include "defs.h"
#include "data.h"
#include "decl.h"

// 抽象语法树优化代码
// Copyright (c) 2019 Warren Toomey, GPL3

// 折叠一颗抽象语法树，把一个二元操作符和它两个A_INTLIT子节点折叠
// 返回原来的树，或者只是一个新的叶子节点
static struct ASTnode *fold2(struct ASTnode *n) {
  int val, leftval, rightval;

  // 从每个子节点那里获得值
  leftval = n->left->atu.a_intvalue;
  rightval = n->right->atu.a_intvalue;

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

// 折叠一颗抽象语法树，把一个一元操作符和它的一个A_INTLIT子节点折叠
// 返回原来的树，或者只是一个新的叶子节点
static struct ASTnode *fold1(struct ASTnode *n) {
  int val;

  // 对孩子对值进行操作，前提是支持的操作
  // 返回一个新的叶子，对于不支持的操作，返回原树
  val = n->left->atu.a_intvalue;
  switch (n->op) {
    case A_WIDEN:
      break;
    case A_INVERT:
      val = ~val;
      break;
    case A_LOGNOT:
      val = !val;
      break;
    default:
      return (n);
  }

  // 返回一个含新值的叶子节点
  return (mkastleaf(A_INTLIT, n->type, NULL, NULL, val));
}

// 尝试把树n常量折叠
static struct ASTnode *fold(struct ASTnode *n) {

  if (n == NULL)
    return (NULL);

  // 折叠左子树和右子树
  n->left = fold(n->left);
  n->right = fold(n->right);

  // 如果两个子树都是A_INTLITs，使用fold2()折叠
  if (n->left && n->left->op == A_INTLIT) {
    if (n->right && n->right->op == A_INTLIT)
      n = fold2(n);
    else
      // 如果只是左子树是A_INTLIT,使用fold1()折叠
      n = fold1(n);
  }

  // 返回可能折叠过的树
  return (n);
}

// 通过常量折叠的方式优化抽象语法树
struct ASTnode *optimise(struct ASTnode *n) {
  n = fold(n);
  return (n);
}
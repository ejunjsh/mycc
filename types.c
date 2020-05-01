#include "defs.h"
#include "data.h"
#include "decl.h"

// 类型和类型处理
// Copyright (c) 2019 Warren Toomey, GPL3

// 如果type是一个任意大小的整型（这里还包括字符和长整型），就返回true
int inttype(int type) {
  return (((type & 0xf) == 0) && (type >= P_CHAR && type <= P_LONG));
}

// 如果类型type的指针类型，返回true
int ptrtype(int type) {
  return ((type & 0xf) != 0);
}

// 给定一个类型type，返回这个类型的指针类型
int pointer_to(int type) {
  if ((type & 0xf) == 0xf)
    fatald("Unrecognised in pointer_to: type", type);
  return (type + 1);
}

// 给定一个指针类型type，返回它指针指向的类型
int value_at(int type) {
  if ((type & 0xf) == 0x0)
    fatald("Unrecognised in value_at: type", type);
  return (type - 1);
}

// 给定一个类型和复合类型的指针，返回这个类型的字节大小
int typesize(int type, struct symtable *ctype) {
  if (type == P_STRUCT || type == P_UNION)
    return (ctype->size);
  return (genprimsize(type));
}

// 给定一个抽象语法树和一个我们想要比较的类型rtype，
// 可能会修改这个语法树。
// 通过扩展大小来兼容rtype，或者改动倍数
// 如果跟rtype没什么差别，就不会改动这树，原值返回
// 如果不兼容rtype的话，会返回NULL
// 如果是二元操作，op参数不为零
struct ASTnode *modify_type(struct ASTnode *tree, int rtype,
			    struct symtable *rctype, int op) {
  int ltype;
  int lsize, rsize;

  ltype = tree->type;

  // 对于A_LOGOR 和 A_LOGAND， 这两个类型必须是整型或者指针类型
  if (op==A_LOGOR || op==A_LOGAND) {
    if (!inttype(ltype) && !ptrtype(ltype))
      return(NULL);
      //TODO:？？
    if (!inttype(ltype) && !ptrtype(rtype))
      return(NULL);
    return (tree);
  }

  // TODO:还没有实现复合结构
  if (ltype == P_STRUCT || ltype == P_UNION)
    fatal("Don't know how to do this yet");
  if (rtype == P_STRUCT || rtype == P_UNION)
    fatal("Don't know how to do this yet");

  // 都是整型
  if (inttype(ltype) && inttype(rtype)) {

    // 类型一样，直接返回
    if (ltype == rtype)
      return (tree);

    // 获得类型大小
    lsize = typesize(ltype, NULL);
    rsize = typesize(rtype, NULL);

    // 当前类型大小大于rtype类型，不兼容，我们不能截断
    if (lsize > rsize)
      return (NULL);

    // 如果当前小于rtype，增容
    if (rsize > lsize)
      return (mkastunary(A_WIDEN, rtype, NULL, tree, NULL, 0));
  }

  // 对于指针
  if (ptrtype(ltype) && ptrtype(rtype)) {
    // 指针可以比较
    if (op >= A_EQ && op <= A_GE)
      return (tree);

    // 对于非二元操作，相同类型，可以直接返回
    // 或者当前类型是一个`void *`类型
    if (op == 0 && (ltype == rtype || ltype == pointer_to(P_VOID)))
      return (tree);
  }

  // 只有在加减操作才进行倍数操作
  // 例如：int* p; int* a=p+1,实际上a的值是p的值加上4，4是整型类型大小，所以4是要作为倍数来修改原来的树
  if (op == A_ADD || op == A_SUBTRACT ||
      op == A_ASPLUS || op == A_ASMINUS) {

    // 当前是整型，rtype是指针类型，同时它的原来类型大小是大于一
    // 就要把当前类型倍数改成rtype的原来类型的大小
    if (inttype(ltype) && ptrtype(rtype)) {
      rsize = genprimsize(value_at(rtype));
      if (rsize > 1)
	    return (mkastunary(A_SCALE, rtype, rctype, tree, NULL, rsize)); // 包装原来那棵tree
      else
	    return (tree);		// 大小为一，不用修改倍数，直接返回
    }
  }

  // 到这里，代表类型不兼容
  return (NULL);
}

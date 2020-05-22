#include "defs.h"
#include "data.h"
#include "decl.h"

// 通用代码生成器
// Copyright (c) 2019 Warren Toomey, GPL3

// 生成并返回一个新的标签号
static int labelid = 1;
int genlabel(void) {
  return (labelid++);
}

static void update_line(struct ASTnode *n) {
  // 如果我们在语法树节点上改变了行号, 输出行号到汇编
  if (n->linenum != 0 && Line != n->linenum) {
    Line = n->linenum;
    cglinenum(Line);
  }
}

// 为IF语句生成汇编代码,包括可选的ELSE语句
static int genIF(struct ASTnode *n, int looptoplabel, int loopendlabel) {
  int Lfalse, Lend;

  // 生成两个标签: 一个是给false时的组合语句,一个是整个IF的结束标签
  // 当没有ELSE语句的情况,Lfalse相当于结束标签了
  Lfalse = genlabel();
  if (n->right)
    Lend = genlabel();

  // 生成条件的汇编代码,传Lfalse作为参数是方便跳到Lfalse
  genAST(n->left, Lfalse, NOLABEL, NOLABEL, n->op);
  genfreeregs(NOREG);

  // 生成true时的组合语句的汇编
  genAST(n->mid, NOLABEL, looptoplabel, loopendlabel, n->op);
  genfreeregs(NOREG);

  // 如果有ELSE, 生成跳转指令,跳到最后
  if (n->right)
    cgjump(Lend);

  // 生成false时的标签
  cglabel(Lfalse);

  // 如果有ELSE,生成false时的组合语句的汇编和结束标签
  if (n->right) {
    genAST(n->right, NOLABEL, NOLABEL, loopendlabel, n->op);
    genfreeregs(NOREG);
    cglabel(Lend);
  }

  return (NOREG);
}

// 生成WHILE语句的汇编代码
static int genWHILE(struct ASTnode *n) {
  int Lstart, Lend;

  // 生成开始和结束标签
  // 输出开始标签
  Lstart = genlabel();
  Lend = genlabel();
  cglabel(Lstart);

  // 生成条件的汇编代码,传Lend作为参数是方便跳出循环
  genAST(n->left, Lend, Lstart, Lend, n->op);
  genfreeregs(NOREG);

  // 生成循环体的汇编
  genAST(n->right, NOLABEL, Lstart, Lend, n->op);
  genfreeregs(NOREG);

  // 最后输出跳回开始的指令和结束标签
  cgjump(Lstart);
  cglabel(Lend);
  return (NOREG);
}

// 生成SWITCH语句的汇编
static int genSWITCH(struct ASTnode *n) {
  int *caseval, *caselabel;
  int Ljumptop, Lend;
  int i, reg, defaultlabel = 0, casecount = 0;
  struct ASTnode *c;

  // 分别创建case值和相关联的标签的数组
  // 保证数组至少有一个元素
  caseval = (int *) malloc((n->a_intvalue + 1) * sizeof(int));
  caselabel = (int *) malloc((n->a_intvalue + 1) * sizeof(int));

  // 生成跳表的标签和整个SWITCH语句的结束标签
  // 设置默认标签为结束标签
  Ljumptop = genlabel();
  Lend = genlabel();
  defaultlabel = Lend;

  // 输出switch条件判断的汇编
  reg = genAST(n->left, NOLABEL, NOLABEL, NOLABEL, 0);
  cgjump(Ljumptop);
  genfreeregs(reg);

  // 沿着右子树去生成每个case的汇编
  for (i = 0, c = n->right; c != NULL; i++, c = c->right) {

    // 获得一个case的标签号,保存它和值到数组里
    // 如果有default case,就存到defaultlabel
    caselabel[i] = genlabel();
    caseval[i] = c->a_intvalue;
    cglabel(caselabel[i]);
    if (c->op == A_DEFAULT)
      defaultlabel = caselabel[i];
    else
      casecount++;

    // 生成case body的汇编代码,传Lend是为了处理break的情况
    // 如果case没有body,直接到下个case
    if (c->left)
      genAST(c->left, NOLABEL, NOLABEL, Lend, 0);
    genfreeregs(NOREG);
  }

  // 保证最后个case跳过switch表
  cgjump(Lend);

  // 输出switch表和结束标签
  cgswitch(reg, casecount, Ljumptop, caselabel, caseval, defaultlabel);
  cglabel(Lend);
  return (NOREG);
}

// 为A_LOGAND 或 A_LOGOR操作生成汇编代码
static int gen_logandor(struct ASTnode *n) {
  // 生成两个标签
  int Lfalse = genlabel();
  int Lend = genlabel();
  int reg;

  // 为左表达式生成代码, 如果表达式满足条件,则跳到false标签
  // 例如 1&&1+1 这个表达式,由于左边为true,那就不用执行右边,直接跳转到Lfalse
  reg = genAST(n->left, NOLABEL, NOLABEL, NOLABEL, 0);
  cgboolean(reg, n->op, Lfalse);
  genfreeregs(NOREG);

  // 为右表达式生成代码, 如果表达式满足条件,则跳到false标签
  reg = genAST(n->right, NOLABEL, NOLABEL, NOLABEL, 0);
  cgboolean(reg, n->op, Lfalse);
  genfreeregs(reg);

  // 设置寄存器的值
  // 这里很精彩,好好品品:)
  if (n->op == A_LOGAND) {
    cgloadboolean(reg, 1);
    cgjump(Lend);
    cglabel(Lfalse);
    cgloadboolean(reg, 0);
  } else {
    cgloadboolean(reg, 0);
    cgjump(Lend);
    cglabel(Lfalse);
    cgloadboolean(reg, 1);
  }
  cglabel(Lend);
  return (reg);
}

// 生成拷贝实参到形参的汇编代码,然后调用函数自己
// 返回寄存器,它的值是函数的返回值
static int gen_funccall(struct ASTnode *n) {
  struct ASTnode *gluetree = n->left;
  int reg;
  int numargs = 0;

  // 拷贝实参之前,先保存寄存器
  cgspillregs();

  // 如果有一列表实参,从最后一个开始遍历到第一个
  while (gluetree) {
    // 生成表达式,并返回表达式值的寄存器
    reg = genAST(gluetree->right, NOLABEL, NOLABEL, NOLABEL, gluetree->op);
    // 拷贝实参到形参
    cgcopyarg(reg, gluetree->a_size);
    // 保留函数参数数量
    if (numargs == 0)
      numargs = gluetree->a_size;
    gluetree = gluetree->left;
  }

  // 调用函数,清理栈(根据参数数量),并返回函数返回值
  return (cgcall(n->sym, numargs));
}

// 为三元表达式生成汇编代码
static int gen_ternary(struct ASTnode *n) {
  int Lfalse, Lend;
  int reg, expreg;

  // Generate two labels: one for the
  // false expression, and one for the
  // end of the overall expression
  Lfalse = genlabel();
  Lend = genlabel();

  // Generate the condition code followed
  // by a jump to the false label.
  genAST(n->left, Lfalse, NOLABEL, NOLABEL, n->op);
  // genfreeregs(NOREG);

  // Get a register to hold the result of the two expressions
  reg = cgallocreg();

  // Generate the true expression and the false label.
  // Move the expression result into the known register.
  expreg = genAST(n->mid, NOLABEL, NOLABEL, NOLABEL, n->op);
  cgmove(expreg, reg);
  cgfreereg(expreg);
  cgjump(Lend);
  cglabel(Lfalse);

  // Generate the false expression and the end label.
  // Move the expression result into the known register.
  expreg = genAST(n->right, NOLABEL, NOLABEL, NOLABEL, n->op);
  cgmove(expreg, reg);
  cgfreereg(expreg);
  cglabel(Lend);
  return (reg);
}

// Given an AST, an optional label, and the AST op
// of the parent, generate assembly code recursively.
// Return the register id with the tree's final value.
int genAST(struct ASTnode *n, int iflabel, int looptoplabel,
	   int loopendlabel, int parentASTop) {
  int leftreg = NOREG, rightreg = NOREG;

  // Empty tree, do nothing
  if (n == NULL)
    return (NOREG);

  // Update the line number in the output
  update_line(n);

  // We have some specific AST node handling at the top
  // so that we don't evaluate the child sub-trees immediately
  switch (n->op) {
  case A_IF:
    return (genIF(n, looptoplabel, loopendlabel));
  case A_WHILE:
    return (genWHILE(n));
  case A_SWITCH:
    return (genSWITCH(n));
  case A_FUNCCALL:
    return (gen_funccall(n));
  case A_TERNARY:
    return (gen_ternary(n));
  case A_LOGOR:
    return (gen_logandor(n));
  case A_LOGAND:
    return (gen_logandor(n));
  case A_GLUE:
    // Do each child statement, and free the
    // registers after each child
    if (n->left != NULL)
      genAST(n->left, iflabel, looptoplabel, loopendlabel, n->op);
    genfreeregs(NOREG);
    if (n->right != NULL)
      genAST(n->right, iflabel, looptoplabel, loopendlabel, n->op);
    genfreeregs(NOREG);
    return (NOREG);
  case A_FUNCTION:
    // Generate the function's preamble before the code
    // in the child sub-tree
    cgfuncpreamble(n->sym);
    genAST(n->left, NOLABEL, NOLABEL, NOLABEL, n->op);
    cgfuncpostamble(n->sym);
    return (NOREG);
  }

  // General AST node handling below

  // Get the left and right sub-tree values
  if (n->left)
    leftreg = genAST(n->left, NOLABEL, NOLABEL, NOLABEL, n->op);
  if (n->right)
    rightreg = genAST(n->right, NOLABEL, NOLABEL, NOLABEL, n->op);

  switch (n->op) {
  case A_ADD:
    return (cgadd(leftreg, rightreg));
  case A_SUBTRACT:
    return (cgsub(leftreg, rightreg));
  case A_MULTIPLY:
    return (cgmul(leftreg, rightreg));
  case A_DIVIDE:
    return (cgdivmod(leftreg, rightreg, A_DIVIDE));
  case A_MOD:
    return (cgdivmod(leftreg, rightreg, A_MOD));
  case A_AND:
    return (cgand(leftreg, rightreg));
  case A_OR:
    return (cgor(leftreg, rightreg));
  case A_XOR:
    return (cgxor(leftreg, rightreg));
  case A_LSHIFT:
    return (cgshl(leftreg, rightreg));
  case A_RSHIFT:
    return (cgshr(leftreg, rightreg));
  case A_EQ:
  case A_NE:
  case A_LT:
  case A_GT:
  case A_LE:
  case A_GE:
    // If the parent AST node is an A_IF, A_WHILE or A_TERNARY,
    // generate a compare followed by a jump. Otherwise, compare
    // registers and set one to 1 or 0 based on the comparison.
    if (parentASTop == A_IF || parentASTop == A_WHILE ||
	parentASTop == A_TERNARY)
      return (cgcompare_and_jump
	      (n->op, leftreg, rightreg, iflabel, n->left->type));
    else
      return (cgcompare_and_set(n->op, leftreg, rightreg, n->left->type));
  case A_INTLIT:
    return (cgloadint(n->a_intvalue, n->type));
  case A_STRLIT:
    return (cgloadglobstr(n->a_intvalue));
  case A_IDENT:
    // Load our value if we are an rvalue
    // or we are being dereferenced
    if (n->rvalue || parentASTop == A_DEREF) {
      return (cgloadvar(n->sym, n->op));
    } else
      return (NOREG);
  case A_ASPLUS:
  case A_ASMINUS:
  case A_ASSTAR:
  case A_ASSLASH:
  case A_ASMOD:
  case A_ASSIGN:

    // For the '+=' and friends operators, generate suitable code
    // and get the register with the result. Then take the left child,
    // make it the right child so that we can fall into the assignment code.
    switch (n->op) {
    case A_ASPLUS:
      leftreg = cgadd(leftreg, rightreg);
      n->right = n->left;
      break;
    case A_ASMINUS:
      leftreg = cgsub(leftreg, rightreg);
      n->right = n->left;
      break;
    case A_ASSTAR:
      leftreg = cgmul(leftreg, rightreg);
      n->right = n->left;
      break;
    case A_ASSLASH:
      leftreg = cgdivmod(leftreg, rightreg, A_DIVIDE);
      n->right = n->left;
      break;
    case A_ASMOD:
      leftreg = cgdivmod(leftreg, rightreg, A_MOD);
      n->right = n->left;
      break;
    }

    // Now into the assignment code
    // Are we assigning to an identifier or through a pointer?
    switch (n->right->op) {
    case A_IDENT:
      if (n->right->sym->class == C_GLOBAL ||
	  n->right->sym->class == C_EXTERN ||
	  n->right->sym->class == C_STATIC)
	return (cgstorglob(leftreg, n->right->sym));
      else
	return (cgstorlocal(leftreg, n->right->sym));
    case A_DEREF:
      return (cgstorderef(leftreg, rightreg, n->right->type));
    default:
      fatald("Can't A_ASSIGN in genAST(), op", n->op);
    }
  case A_WIDEN:
    // Widen the child's type to the parent's type
    return (cgwiden(leftreg, n->left->type, n->type));
  case A_RETURN:
    cgreturn(leftreg, Functionid);
    return (NOREG);
  case A_ADDR:
    // If we have a symbol, get its address. Otherwise,
    // the left register already has the address because
    // it's a member access
    if (n->sym != NULL)
      return (cgaddress(n->sym));
    else
      return (leftreg);
  case A_DEREF:
    // If we are an rvalue, dereference to get the value we point at,
    // otherwise leave it for A_ASSIGN to store through the pointer
    if (n->rvalue)
      return (cgderef(leftreg, n->left->type));
    else
      return (leftreg);
  case A_SCALE:
    // Small optimisation: use shift if the
    // scale value is a known power of two
    switch (n->a_size) {
    case 2:
      return (cgshlconst(leftreg, 1));
    case 4:
      return (cgshlconst(leftreg, 2));
    case 8:
      return (cgshlconst(leftreg, 3));
    default:
      // Load a register with the size and
      // multiply the leftreg by this size
      rightreg = cgloadint(n->a_size, P_INT);
      return (cgmul(leftreg, rightreg));
    }
  case A_POSTINC:
  case A_POSTDEC:
    // Load and decrement the variable's value into a register
    // and post increment/decrement it
    return (cgloadvar(n->sym, n->op));
  case A_PREINC:
  case A_PREDEC:
    // Load and decrement the variable's value into a register
    // and pre increment/decrement it
    return (cgloadvar(n->left->sym, n->op));
  case A_NEGATE:
    return (cgnegate(leftreg));
  case A_INVERT:
    return (cginvert(leftreg));
  case A_LOGNOT:
    return (cglognot(leftreg));
  case A_TOBOOL:
    // If the parent AST node is an A_IF or A_WHILE, generate
    // a compare followed by a jump. Otherwise, set the register
    // to 0 or 1 based on it's zeroeness or non-zeroeness
    return (cgboolean(leftreg, parentASTop, iflabel));
  case A_BREAK:
    cgjump(loopendlabel);
    return (NOREG);
  case A_CONTINUE:
    cgjump(looptoplabel);
    return (NOREG);
  case A_CAST:
    return (leftreg);		// Not much to do
  default:
    fatald("Unknown AST operator", n->op);
  }
  return (NOREG);		// Keep -Wall happy
}

void genpreamble(char *filename) {
  cgpreamble(filename);
}
void genpostamble() {
  cgpostamble();
}
void genfreeregs(int keepreg) {
  cgfreeallregs(keepreg);
}
void genglobsym(struct symtable *node) {
  cgglobsym(node);
}

// Generate a global string.
// If append is true, append to
// previous genglobstr() call.
int genglobstr(char *strvalue, int append) {
  int l = genlabel();
  cgglobstr(l, strvalue, append);
  return (l);
}
void genglobstrend(void) {
  cgglobstrend();
}
int genprimsize(int type) {
  return (cgprimsize(type));
}
int genalign(int type, int offset, int direction) {
  return (cgalign(type, offset, direction));
}
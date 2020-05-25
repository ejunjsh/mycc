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

  // 生成两个标签：一个是给false表达式，一个是给整个表达式
  Lfalse = genlabel();
  Lend = genlabel();

  // 生成条件的汇编代码，并传递Lfalse作为参数
  genAST(n->left, Lfalse, NOLABEL, NOLABEL, n->op);
  // genfreeregs(NOREG);

  // 分配一个寄存器用来存储后面两个表达式的结果
  reg = cgallocreg();

  // 生成true表达式和false标签
  // 把表达式的结果拷贝到上面那个已知的寄存器
  expreg = genAST(n->mid, NOLABEL, NOLABEL, NOLABEL, n->op);
  cgmove(expreg, reg);
  cgfreereg(expreg);
  cgjump(Lend);
  cglabel(Lfalse);

  // 生成false表达式和结束标签
  // 把表达式的结果拷贝到上面那个已知的寄存器
  expreg = genAST(n->right, NOLABEL, NOLABEL, NOLABEL, n->op);
  cgmove(expreg, reg);
  cgfreereg(expreg);
  cglabel(Lend);
  return (reg);
}

// 给定一个语法树，三个可选的标签，和一个父节点的操作
// 递归生成汇编代码
// 返回寄存器，寄存器里面有这棵树的最终值
int genAST(struct ASTnode *n, int iflabel, int looptoplabel,
	   int loopendlabel, int parentASTop) {
  int leftreg = NOREG, rightreg = NOREG;

  // 空树，什么都不做
  if (n == NULL)
    return (NOREG);

  // 在输出中更新行号
  update_line(n);

  // 上面我们定义了一些语法树节点处理的函数，所以，如果遇到某些操作，就直接扔给相关函数处理即可
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
    // 执行每个子语句，然后释放所有寄存器
    if (n->left != NULL)
      genAST(n->left, iflabel, looptoplabel, loopendlabel, n->op);
    genfreeregs(NOREG);
    if (n->right != NULL)
      genAST(n->right, iflabel, looptoplabel, loopendlabel, n->op);
    genfreeregs(NOREG);
    return (NOREG);
  case A_FUNCTION:
    // 生成函数的前置汇编，然会再到子树的汇编
    cgfuncpreamble(n->sym);
    genAST(n->left, NOLABEL, NOLABEL, NOLABEL, n->op);
    cgfuncpostamble(n->sym);
    return (NOREG);
  }

  // 下面是通用语法树处理

  // 获得左树和右数的值
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
    // 如果父节点是A_IF, A_WHILE 或者 A_TERNARY，
    // 生成比较和跳转的代码，否则比较寄存器，然后根据比较结果设置其中一个寄存器的值为1或0
    if (parentASTop == A_IF || parentASTop == A_WHILE || parentASTop == A_TERNARY)
      return (cgcompare_and_jump(n->op, leftreg, rightreg, iflabel, n->left->type));
    else
      return (cgcompare_and_set(n->op, leftreg, rightreg, n->left->type));
  case A_INTLIT:
    return (cgloadint(n->a_intvalue, n->type));
  case A_STRLIT:
    return (cgloadglobstr(n->a_intvalue));
  case A_IDENT:
    // 如果是右值或者是解引用，则加载值
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

    // 对于'+='和相关操作，生成合适的汇编代码，同时获取结果寄存器
    // 然后令右节点等于左节点，这样就变成赋值操作了
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

    // 赋值代码
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
    // 加宽子节点的类型到父节点的类型
    return (cgwiden(leftreg, n->left->type, n->type));
  case A_RETURN:
    cgreturn(leftreg, Functionid);
    return (NOREG);
  case A_ADDR:
    // 如果有符号，直接获取它的地址，否则左寄存器已经有地址，因为这是个成员访问
    if (n->sym != NULL)
      return (cgaddress(n->sym));
    else
      return (leftreg);
  case A_DEREF:
    // 如果是右值，解引用来获得值
    // 否则直接返回，留给A_ASSIGN来处理
    if (n->rvalue)
      return (cgderef(leftreg, n->left->type));
    else
      return (leftreg);
  case A_SCALE:
    // 小优化：如果大小是2的幂次方，就使用位移来处理
    switch (n->a_size) {
    case 2:
      return (cgshlconst(leftreg, 1));
    case 4:
      return (cgshlconst(leftreg, 2));
    case 8:
      return (cgshlconst(leftreg, 3));
    default:
      // 加载大小到寄存器，然后用左寄存器相乘
      rightreg = cgloadint(n->a_size, P_INT);
      return (cgmul(leftreg, rightreg));
    }
  case A_POSTINC:
  case A_POSTDEC:
    // 例如：i++
    // 先返回i，再++
    return (cgloadvar(n->sym, n->op));
  case A_PREINC:
  case A_PREDEC:
    // 例如：++i
    // 先++，再返回i
    return (cgloadvar(n->left->sym, n->op));
  case A_NEGATE:
    return (cgnegate(leftreg));
  case A_INVERT:
    return (cginvert(leftreg));
  case A_LOGNOT:
    return (cglognot(leftreg));
  case A_TOBOOL:
    // 如果父节点是A_IF, A_WHILE，
    // 生成比较和跳转的代码，否则根据是零还是非零来设置寄存器的值为0或1
    return (cgboolean(leftreg, parentASTop, iflabel));
  case A_BREAK:
    cgjump(loopendlabel);
    return (NOREG);
  case A_CONTINUE:
    cgjump(looptoplabel);
    return (NOREG);
  case A_CAST:
    return (leftreg);		// 什么都不用做
  default:
    fatald("Unknown AST operator", n->op);
  }
  return (NOREG);	
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

// 生成一个全局字符串，如果append为true，就加到之前这个函数调用生成的全局字符串之后
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
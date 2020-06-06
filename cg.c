#include "defs.h"
#include "data.h"
#include "decl.h"

// x86-64汇编生成器
// Copyright (c) 2019 Warren Toomey, GPL3

// 标记我们正在输出的汇编在那个区域
enum { no_seg, text_seg, data_seg } currSeg = no_seg;

// 切换到指令（text）区域
void cgtextseg() {
  if (currSeg != text_seg) {
    fputs("\t.text\n", Outfile);
    currSeg = text_seg;
  }
}

// 切换到数据（data）区域
void cgdataseg() {
  if (currSeg != data_seg) {
    fputs("\t.data\n", Outfile);
    currSeg = data_seg;
  }
}

// 给定一个标量类型，返回该类型的字节大小
int cgprimsize(int type) {
  if (ptrtype(type))
    return (8);
  switch (type) {
  case P_CHAR:
    return (1);
  case P_INT:
    return (4);
  case P_LONG:
    return (8);
  default:
    fatald("Bad type in cgprimsize:", type);
  }
  return (0);	
}

// 给定一个类型，一个内存位移（这个还没有分配过）
// 和一个方向（1为上，-1为下），返回一个合适的对齐位移
// 它有可能跟原来的位移一样，或者比原来少，或者多
int cgalign(int type, int offset, int direction) {
  int alignment;

  // 虽然x86-64不要求对齐，但是也尝试下把
  // 字符的不用对情，其他类型就按四字节对齐
  switch (type) {
  case P_CHAR:
    break;
  default:
    // 四字节对齐
    alignment = 4;
    offset = (offset + direction * (alignment - 1)) & ~(alignment - 1);
  }
  return (offset);
}

// 下一个本地变量的相对于栈底的位置
// 这个是个正数，这样可以方便对齐栈指针
static int localOffset;

// 相对于栈低的栈指针位移
// 用这个来保证16字节对齐 
static int stackOffset;

// 为一个新的本地变量在栈中开辟一个位置
static int newlocaloffset(int size) {
  // 减去一个至少为4字节的位移来在栈中分配空间
  localOffset += (size > 4) ? size : 4;
  return (-localOffset);
}

// 所有可用寄存器列表
// 这个列表还包含字节和双字寄存器
// 这个列表还包括用在函数参数的寄存器
#define NUMFREEREGS 4
#define FIRSTPARAMREG 9		// 第一个参数寄存器的位置
static int freereg[NUMFREEREGS];
static char *reglist[] =
  { "%r10", "%r11", "%r12", "%r13", "%r9", "%r8", "%rcx", "%rdx", "%rsi",
  "%rdi"
};

// 8位和32位寄存器
static char *breglist[] =
  { "%r10b", "%r11b", "%r12b", "%r13b", "%r9b", "%r8b", "%cl", "%dl", "%sil",
  "%dil"
};

static char *dreglist[] =
  { "%r10d", "%r11d", "%r12d", "%r13d", "%r9d", "%r8d", "%ecx", "%edx",
  "%esi", "%edi"
};

// 压入一个寄存器到栈
static void pushreg(int r) {
  fprintf(Outfile, "\tpushq\t%s\n", reglist[r]);
}

// 从栈中弹出到寄存器
static void popreg(int r) {
  fprintf(Outfile, "\tpopq\t%s\n", reglist[r]);
}


// 释放所有寄存器，除了参数那个，如果参数是-1，则释放所有
void cgfreeallregs(int keepreg) {
  int i;
  // fprintf(Outfile, "# freeing all registers\n");
  for (i = 0; i < NUMFREEREGS; i++)
    if (i != keepreg)
      freereg[i] = 1;
}

// 当需要溢出一个寄存器，我们选择这个变量指定的寄存器溢出，然后下次继续循环取剩下的寄存器溢出
// spillreg是一直递增的，所以需要取模
static int spillreg = 0;

// 分配一个空闲的寄存器,并返回这个寄存器号
// 如果没有可用的话,就溢出一个寄存器到栈,然后返回这个寄存器
int cgallocreg(void) {
  int reg;

  for (reg = 0; reg < NUMFREEREGS; reg++) {
    if (freereg[reg]) {
      freereg[reg] = 0;
      // fprintf(Outfile, "# allocated register %s\n", reglist[reg]);
      return (reg);
    }
  }

  // 没有可用寄存器,溢出一个
  reg = (spillreg % NUMFREEREGS);
  spillreg++;
  // fprintf(Outfile, "# spilling reg %s\n", reglist[reg]);
  pushreg(reg);
  return (reg);
}

// 释放一个寄存器到可用列表中
void cgfreereg(int reg) {
  if (freereg[reg] != 0) {
    // fprintf(Outfile, "# error trying to free register %s\n", reglist[reg]);
    fatald("Error trying to free register", reg);
  }
  // 如果这个是溢出来的, 从栈中恢复
  if (spillreg > 0) {
    spillreg--;
    reg = (spillreg % NUMFREEREGS);
    // fprintf(Outfile, "# unspilling reg %s\n", reglist[reg]);
    popreg(reg);
  } else {
    // fprintf(Outfile, "# freeing reg %s\n", reglist[reg]);
    freereg[reg] = 1;
  }
}

// 溢出所有寄存器到栈中
void cgspillregs(void) {
  int i;

  for (i = 0; i < NUMFREEREGS; i++)
    pushreg(i);
}

// 从栈恢复所有寄存器
static void cgunspillregs(void) {
  int i;

  for (i = NUMFREEREGS - 1; i >= 0; i--)
    popreg(i);
}

// 打印前置汇编到输出文件
void cgpreamble(char *filename) {
  cgfreeallregs(NOREG);
  cgtextseg();
  fprintf(Outfile, "\t.file 1 ");
  fputc('"', Outfile);
  fprintf(Outfile, "%s", filename);
  fputc('"', Outfile);
  fputc('\n', Outfile);
  fprintf(Outfile,
	  "# internal switch(expr) routine\n"
	  "# %%rsi = switch table, %%rax = expr\n"
	  "# from SubC: http://www.t3x.org/subc/\n"
	  "\n"
	  "__switch:\n"
	  "        pushq   %%rsi\n"
	  "        movq    %%rdx, %%rsi\n"
	  "        movq    %%rax, %%rbx\n"
	  "        cld\n"
	  "        lodsq\n"
	  "        movq    %%rax, %%rcx\n"
	  "__next:\n"
	  "        lodsq\n"
	  "        movq    %%rax, %%rdx\n"
	  "        lodsq\n"
	  "        cmpq    %%rdx, %%rbx\n"
	  "        jnz     __no\n"
	  "        popq    %%rsi\n"
	  "        jmp     *%%rax\n"
	  "__no:\n"
	  "        loop    __next\n"
	  "        lodsq\n"
	  "        popq    %%rsi\n" "        jmp     *%%rax\n\n");
}

// 对于后缀汇编，没有什么要做的
void cgpostamble() {
}

// 生成函数的前缀汇编
void cgfuncpreamble(struct symtable *sym) {
  char *name = sym->name;
  struct symtable *parm, *locvar;
  int cnt;
  int paramOffset = 16;		// 超过6个的那部分参数所在栈的开始位置
  int paramReg = FIRSTPARAMREG;	// 上面参数寄存器列表的索引

  // 输出text段，重设本地变量位移
  cgtextseg();
  localOffset = 0;

  // 输出函数开始标签，保存%rbp 和 %rsp
  if (sym->class == C_GLOBAL)
    fprintf(Outfile, "\t.globl\t%s\n" "\t.type\t%s, @function\n", name, name);
  fprintf(Outfile, "%s:\n" "\tpushq\t%%rbp\n" "\tmovq\t%%rsp, %%rbp\n", name);

  // 拷贝在寄存器里面的参数到栈
  // 如果大于6个参数都已经在栈了
  for (parm = sym->member, cnt = 1; parm != NULL; parm = parm->next, cnt++) {
    if (cnt > 6) {
      parm->stu.st_posn = paramOffset;
      paramOffset += 8;
    } else {
      parm->stu.st_posn = newlocaloffset(parm->size);
      cgstorlocal(paramReg--, parm);
    }
  }

  // 计算本地变量的栈位置
  for (locvar = Loclhead; locvar != NULL; locvar = locvar->next) {
    locvar->stu.st_posn = newlocaloffset(locvar->size);
  }

  // 对齐栈指针到16的倍数
  stackOffset = (localOffset + 15) & ~15;
  fprintf(Outfile, "\taddq\t$%d,%%rsp\n", -stackOffset);
}

// 生成函数后缀汇编
void cgfuncpostamble(struct symtable *sym) {
  cglabel(sym->stu.st_endlabel);
  fprintf(Outfile, "\taddq\t$%d,%%rsp\n", stackOffset);
  fputs("\tpopq	%rbp\n" "\tret\n", Outfile);
  cgfreeallregs(NOREG);
}

// 加载一个整型字面量到一个寄存器
// 返回当前寄存器
// 对于x86-64，不用担心类型
int cgloadint(int value, int type) {
  // 分配一个新的寄存器
  int r = cgallocreg();

  fprintf(Outfile, "\tmovq\t$%d, %s\n", value, reglist[r]);
  return (r);
}

// 从一个变量加载值到寄存器
// 返回当前寄存器
// 如果有i++或++i这些操作，也是在这个函数里面处理
int cgloadvar(struct symtable *sym, int op) {
  int r, postreg, offset = 1;

  // 分配一个新的寄存器
  r = cgallocreg();

  // 如果是指针，用指针指向的类型大小作为++或--的大小
  // 如果不是，那大小就是1
  if (ptrtype(sym->type))
    offset = typesize(value_at(sym->type), sym->ctype);

  // 如果是--，就把它变成负数
  if (op == A_PREDEC || op == A_POSTDEC)
    offset = -offset;

  // 如果是++i
  if (op == A_PREINC || op == A_PREDEC) {
    // 获取变量地址
    if (sym->class == C_LOCAL || sym->class == C_PARAM)
      fprintf(Outfile, "\tleaq\t%d(%%rbp), %s\n", sym->stu.st_posn, reglist[r]);
    else
      fprintf(Outfile, "\tleaq\t%s(%%rip), %s\n", sym->name, reglist[r]);

    // 改变那个地址里面的值
    switch (sym->size) {
    case 1:
      fprintf(Outfile, "\taddb\t$%d,(%s)\n", offset, reglist[r]);
      break;
    case 4:
      fprintf(Outfile, "\taddl\t$%d,(%s)\n", offset, reglist[r]);
      break;
    case 8:
      fprintf(Outfile, "\taddq\t$%d,(%s)\n", offset, reglist[r]);
      break;
    }
  }

  // 加载变量值到寄存器
  if (sym->class == C_LOCAL || sym->class == C_PARAM) {
    switch (sym->size) {
    case 1:
      fprintf(Outfile, "\tmovzbq\t%d(%%rbp), %s\n", sym->stu.st_posn, reglist[r]);
      break;
    case 4:
      fprintf(Outfile, "\tmovslq\t%d(%%rbp), %s\n", sym->stu.st_posn, reglist[r]);
      break;
    case 8:
      fprintf(Outfile, "\tmovq\t%d(%%rbp), %s\n", sym->stu.st_posn, reglist[r]);
    }
  } else {
    switch (sym->size) {
    case 1:
      fprintf(Outfile, "\tmovzbq\t%s(%%rip), %s\n", sym->name, reglist[r]);
      break;
    case 4:
      fprintf(Outfile, "\tmovslq\t%s(%%rip), %s\n", sym->name, reglist[r]);
      break;
    case 8:
      fprintf(Outfile, "\tmovq\t%s(%%rip), %s\n", sym->name, reglist[r]);
    }
  }

  // 如果是i++
  if (op == A_POSTINC || op == A_POSTDEC) {
    postreg = cgallocreg(); // 分配一个新寄存器

    // 获取变量地址
    if (sym->class == C_LOCAL || sym->class == C_PARAM)
      fprintf(Outfile, "\tleaq\t%d(%%rbp), %s\n", sym->stu.st_posn,
	      reglist[postreg]);
    else
      fprintf(Outfile, "\tleaq\t%s(%%rip), %s\n", sym->name,
	      reglist[postreg]);

    // 改变那个地址里面的值
    switch (sym->size) {
    case 1:
      fprintf(Outfile, "\taddb\t$%d,(%s)\n", offset, reglist[postreg]);
      break;
    case 4:
      fprintf(Outfile, "\taddl\t$%d,(%s)\n", offset, reglist[postreg]);
      break;
    case 8:
      fprintf(Outfile, "\taddq\t$%d,(%s)\n", offset, reglist[postreg]);
      break;
    }

    // 最后释放这个寄存器
    cgfreereg(postreg);
  }

  // 返回带有值的寄存器
  return (r);
}

// 给定一个全局字符串的标签号，加载它的地址到一个新的寄存器
int cgloadglobstr(int label) {
  // 分配一个新的寄存器
  int r = cgallocreg();
  fprintf(Outfile, "\tleaq\tL%d(%%rip), %s\n", label, reglist[r]);
  return (r);
}

// 相加两个寄存器，并返回装有结果的那个寄存器
int cgadd(int r1, int r2) {
  fprintf(Outfile, "\taddq\t%s, %s\n", reglist[r2], reglist[r1]);
  cgfreereg(r2);
  return (r1);
}

// 从r1中减去r2,并返回装有结果的那个寄存器
int cgsub(int r1, int r2) {
  fprintf(Outfile, "\tsubq\t%s, %s\n", reglist[r2], reglist[r1]);
  cgfreereg(r2);
  return (r1);
}

// 相乘两个寄存器，并返回装有结果的那个寄存器
int cgmul(int r1, int r2) {
  fprintf(Outfile, "\timulq\t%s, %s\n", reglist[r2], reglist[r1]);
  cgfreereg(r2);
  return (r1);
}

// 相除或取余两个寄存器，并返回装有结果的那个寄存器
int cgdivmod(int r1, int r2, int op) {
  fprintf(Outfile, "\tmovq\t%s,%%rax\n", reglist[r1]);
  fprintf(Outfile, "\tcqo\n");
  fprintf(Outfile, "\tidivq\t%s\n", reglist[r2]);
  if (op == A_DIVIDE)
    fprintf(Outfile, "\tmovq\t%%rax,%s\n", reglist[r1]);
  else
    fprintf(Outfile, "\tmovq\t%%rdx,%s\n", reglist[r1]);
  cgfreereg(r2);
  return (r1);
}

// 与
int cgand(int r1, int r2) {
  fprintf(Outfile, "\tandq\t%s, %s\n", reglist[r2], reglist[r1]);
  cgfreereg(r2);
  return (r1);
}

// 或
int cgor(int r1, int r2) {
  fprintf(Outfile, "\torq\t%s, %s\n", reglist[r2], reglist[r1]);
  cgfreereg(r2);
  return (r1);
}

// 异或
int cgxor(int r1, int r2) {
  fprintf(Outfile, "\txorq\t%s, %s\n", reglist[r2], reglist[r1]);
  cgfreereg(r2);
  return (r1);
}

// 左移r1 r2位
int cgshl(int r1, int r2) {
  fprintf(Outfile, "\tmovb\t%s, %%cl\n", breglist[r2]);
  fprintf(Outfile, "\tshlq\t%%cl, %s\n", reglist[r1]);
  cgfreereg(r2);
  return (r1);
}

// 右移r1 r2位
int cgshr(int r1, int r2) {
  fprintf(Outfile, "\tmovb\t%s, %%cl\n", breglist[r2]);
  fprintf(Outfile, "\tshrq\t%%cl, %s\n", reglist[r1]);
  cgfreereg(r2);
  return (r1);
}

// 变成负数
int cgnegate(int r) {
  fprintf(Outfile, "\tnegq\t%s\n", reglist[r]);
  return (r);
}

// 按位取反一个寄存器的值
int cginvert(int r) {
  fprintf(Outfile, "\tnotq\t%s\n", reglist[r]);
  return (r);
}

// 逻辑取反一个寄存器的值
int cglognot(int r) {
  fprintf(Outfile, "\ttest\t%s, %s\n", reglist[r], reglist[r]);
  fprintf(Outfile, "\tsete\t%s\n", breglist[r]);
  fprintf(Outfile, "\tmovzbq\t%s, %s\n", breglist[r], reglist[r]);
  return (r);
}

// 加载一个布尔值(0或1)到一个给定的寄存器
void cgloadboolean(int r, int val) {
  fprintf(Outfile, "\tmovq\t$%d, %s\n", val, reglist[r]);
}

// 转换一个整型值为一个布尔值
// 如果是IF,WHILE,LOGAND或者LOGOR操作,就是要跳转到label
int cgboolean(int r, int op, int label) {
  fprintf(Outfile, "\ttest\t%s, %s\n", reglist[r], reglist[r]);
  switch (op) {
  case A_IF:
  case A_WHILE:
  case A_LOGAND:
    fprintf(Outfile, "\tje\tL%d\n", label);
    break;
  case A_LOGOR:
    fprintf(Outfile, "\tjne\tL%d\n", label);
    break;
  default:
    fprintf(Outfile, "\tsetnz\t%s\n", breglist[r]);
    fprintf(Outfile, "\tmovzbq\t%s, %s\n", breglist[r], reglist[r]);
  }
  return (r);
}

// 用给定的符号表来调用函数
// 弹出之前传参时入栈的实参
// 返回含有返回值的寄存器
int cgcall(struct symtable *sym, int numargs) {
  int outr;

  // 调用函数
  fprintf(Outfile, "\tcall\t%s@PLT\n", sym->name);

  // 删除之前入栈的实参
  if (numargs > 6)
    fprintf(Outfile, "\taddq\t$%d, %%rsp\n", 8 * (numargs - 6));

  // 恢复之前入栈的寄存器
  cgunspillregs();

  // 获取一个新的寄存器,拷贝返回值给他
  outr = cgallocreg();
  fprintf(Outfile, "\tmovq\t%%rax, %s\n", reglist[outr]);
  return (outr);
}

// 把r的值考到位置为argposn的形参上,为以后函数调用做准备
// argposn是从1开始,不是从0
void cgcopyarg(int r, int argposn) {

  // 如果实参数量大于6个,那就把大于的都压入栈中
  if (argposn > 6) {
    fprintf(Outfile, "\tpushq\t%s\n", reglist[r]);
  } else {
    // 否则把实参值拷贝到指定的寄存器
    fprintf(Outfile, "\tmovq\t%s, %s\n", reglist[r],
	    reglist[FIRSTPARAMREG - argposn + 1]);
  }
  cgfreereg(r);
}

// 用常量左移一个寄存器
int cgshlconst(int r, int val) {
  fprintf(Outfile, "\tsalq\t$%d, %s\n", val, reglist[r]);
  return (r);
}

// 存储一个寄存器值到一个全局变量
int cgstorglob(int r, struct symtable *sym) {

  if (cgprimsize(sym->type) == 8) {
    fprintf(Outfile, "\tmovq\t%s, %s(%%rip)\n", reglist[r], sym->name);
  } else
    switch (sym->type) {
    case P_CHAR:
      fprintf(Outfile, "\tmovb\t%s, %s(%%rip)\n", breglist[r], sym->name);
      break;
    case P_INT:
      fprintf(Outfile, "\tmovl\t%s, %s(%%rip)\n", dreglist[r], sym->name);
      break;
    default:
      fatald("Bad type in cgstorglob:", sym->type);
    }
  return (r);
}

// 存储一个寄存器值到一个本地变量
int cgstorlocal(int r, struct symtable *sym) {

  if (cgprimsize(sym->type) == 8) {
    fprintf(Outfile, "\tmovq\t%s, %d(%%rbp)\n", reglist[r], sym->stu.st_posn);
  } else
    switch (sym->type) {
    case P_CHAR:
      fprintf(Outfile, "\tmovb\t%s, %d(%%rbp)\n", breglist[r], sym->stu.st_posn);
      break;
    case P_INT:
      fprintf(Outfile, "\tmovl\t%s, %d(%%rbp)\n", dreglist[r], sym->stu.st_posn);
      break;
    default:
      fatald("Bad type in cgstorlocal:", sym->type);
    }
  return (r);
}

// 生成全局符号，除了函数
void cgglobsym(struct symtable *node) {
  int size, type;
  int initvalue;
  int i;

  if (node == NULL)
    return;
  if (node->stype == S_FUNCTION)
    return;

  // 获取变量（数组元素）的大小
  // 和变量的类型
  if (node->stype == S_ARRAY) {
    size = typesize(value_at(node->type), node->ctype);
    type = value_at(node->type);
  } else {
    size = node->size;
    type = node->type;
  }

  // 生成全局标识和标签
  cgdataseg();
  if (node->class == C_GLOBAL)
    fprintf(Outfile, "\t.globl\t%s\n", node->name);
  fprintf(Outfile, "%s:\n", node->name);

  // 输出一个或者更多元素的空间
  for (i = 0; i < node->nelems; i++) {

    // 初始值
    initvalue = 0;
    if (node->initlist != NULL)
      initvalue = node->initlist[i];

    // 生成这个类型的空间
    switch (size) {
    case 1:
      fprintf(Outfile, "\t.byte\t%d\n", initvalue);
      break;
    case 4:
      fprintf(Outfile, "\t.long\t%d\n", initvalue);
      break;
    case 8:
      // 生成指向字符串字面量的指针，对于0值，就是真的0，而不是标签L0
      if (node->initlist != NULL && type == pointer_to(P_CHAR) && initvalue != 0)
	      fprintf(Outfile, "\t.quad\tL%d\n", initvalue);
      else
      	fprintf(Outfile, "\t.quad\t%d\n", initvalue);
      break;
    default:
      for (i = 0; i < size; i++)
	      fprintf(Outfile, "\t.byte\t0\n");
    }
  }
}

// 生成一个全局字符串和它的开始标签
// 如果append是true，则不要输出标签
void cgglobstr(int l, char *strvalue, int append) {
  char *cptr;
  if (!append)
    cglabel(l);
  for (cptr = strvalue; *cptr; cptr++) {
    fprintf(Outfile, "\t.byte\t%d\n", *cptr);
  }
}

// 结束一个全局字符串，就是加一个0在后面
void cgglobstrend(void) {
  fprintf(Outfile, "\t.byte\t0\n");
}

// 一堆比较指令
// 顺序跟A_EQ, A_NE, A_LT, A_GT, A_LE, A_GE 一致
static char *cmplist[] =
  { "sete", "setne", "setl", "setg", "setle", "setge" };

// 比较两个寄存器
int cgcompare_and_set(int ASTop, int r1, int r2, int type) {
  int size = cgprimsize(type);


  // 检查语法树操作的范围
  if (ASTop < A_EQ || ASTop > A_GE)
    fatal("Bad ASTop in cgcompare_and_set()");

  switch (size) {
  case 1:
    fprintf(Outfile, "\tcmpb\t%s, %s\n", breglist[r2], breglist[r1]);
    break;
  case 4:
    fprintf(Outfile, "\tcmpl\t%s, %s\n", dreglist[r2], dreglist[r1]);
    break;
  default:
    fprintf(Outfile, "\tcmpq\t%s, %s\n", reglist[r2], reglist[r1]);
  }

  fprintf(Outfile, "\t%s\t%s\n", cmplist[ASTop - A_EQ], breglist[r2]);
  fprintf(Outfile, "\tmovzbq\t%s, %s\n", breglist[r2], reglist[r2]);
  cgfreereg(r1);
  return (r2);
}

// 生成一个标签（这个就是直接打印汇编代码了）
void cglabel(int l) {
  fprintf(Outfile, "L%d:\n", l);
}

// 生成一个jmp汇编指令
void cgjump(int l) {
  fprintf(Outfile, "\tjmp\tL%d\n", l);
}

// 一堆反跳转指令
// 顺序跟 A_EQ, A_NE, A_LT, A_GT, A_LE, A_GE 一致
static char *invcmplist[] = { "jne", "je", "jge", "jle", "jg", "jl" };

// 比较两个寄存器，如果结果是false就跳转
int cgcompare_and_jump(int ASTop, int r1, int r2, int label, int type) {
  int size = cgprimsize(type);

  // 检查语法树操作范围
  if (ASTop < A_EQ || ASTop > A_GE)
    fatal("Bad ASTop in cgcompare_and_set()");

  switch (size) {
  case 1:
    fprintf(Outfile, "\tcmpb\t%s, %s\n", breglist[r2], breglist[r1]);
    break;
  case 4:
    fprintf(Outfile, "\tcmpl\t%s, %s\n", dreglist[r2], dreglist[r1]);
    break;
  default:
    fprintf(Outfile, "\tcmpq\t%s, %s\n", reglist[r2], reglist[r1]);
  }

  fprintf(Outfile, "\t%s\tL%d\n", invcmplist[ASTop - A_EQ], label);
  cgfreereg(r1);
  cgfreereg(r2);
  return (NOREG);
}

// 从旧的类型去加宽寄存器里面的值来适应新的类型，返回一个新值的寄存器
int cgwiden(int r, int oldtype, int newtype) {
  // 什么都不需要做
  return (r);
}

// 生成函数返回一个值的汇编代码
void cgreturn(int reg, struct symtable *sym) {

  // 处理有返回值的
  if (reg != NOREG) {
    // 处理指针，不能把它放在下面的switch语句里面一起处理
    if (ptrtype(sym->type))
      fprintf(Outfile, "\tmovq\t%s, %%rax\n", reglist[reg]);
    else {
      // 根据函数类型生成相应汇编代码
      switch (sym->type) {
      case P_CHAR:
        fprintf(Outfile, "\tmovzbl\t%s, %%eax\n", breglist[reg]);
        break;
      case P_INT:
        fprintf(Outfile, "\tmovl\t%s, %%eax\n", dreglist[reg]);
        break;
      case P_LONG:
        fprintf(Outfile, "\tmovq\t%s, %%rax\n", reglist[reg]);
        break;
      default:
	      fatald("Bad function type in cgreturn:", sym->type);
      }
    }
  }

  cgjump(sym->stu.st_endlabel);
}

// 生成代码来加载一个标识符的地址到一个变量，返回装有这个地址的寄存器
int cgaddress(struct symtable *sym) {
  int r = cgallocreg();

  if (sym->class == C_GLOBAL ||
      sym->class == C_EXTERN || sym->class == C_STATIC)
    fprintf(Outfile, "\tleaq\t%s(%%rip), %s\n", sym->name, reglist[r]);
  else
    fprintf(Outfile, "\tleaq\t%d(%%rbp), %s\n", sym->stu.st_posn, reglist[r]);
  return (r);
}

// 解引用一个指针来获得值，放进同一个寄存器
int cgderef(int r, int type) {
  // 获得指针指向的类型
  int newtype = value_at(type);
  // 获得类型大小
  int size = cgprimsize(newtype);

  switch (size) {
  case 1:
    fprintf(Outfile, "\tmovzbq\t(%s), %s\n", reglist[r], reglist[r]);
    break;
  case 4:
    fprintf(Outfile, "\tmovslq\t(%s), %s\n", reglist[r], reglist[r]);
    break;
  case 8:
    fprintf(Outfile, "\tmovq\t(%s), %s\n", reglist[r], reglist[r]);
    break;
  default:
    fatald("Can't cgderef on type:", type);
  }
  return (r);
}

// 通过解引用来存数据
int cgstorderef(int r1, int r2, int type) {
  // 获得类型大小
  int size = cgprimsize(type);

  switch (size) {
  case 1:
    fprintf(Outfile, "\tmovb\t%s, (%s)\n", breglist[r1], reglist[r2]);
    break;
  case 4:
    fprintf(Outfile, "\tmovl\t%s, (%s)\n", dreglist[r1], reglist[r2]);
    break;
  case 8:
    fprintf(Outfile, "\tmovq\t%s, (%s)\n", reglist[r1], reglist[r2]);
    break;
  default:
    fatald("Can't cgstoderef on type:", type);
  }
  return (r1);
}

// 生成一个switch跳表和代码去加载寄存器，还有跳到__switch代码
void cgswitch(int reg, int casecount, int toplabel,
	      int *caselabel, int *caseval, int defaultlabel) {
  int i, label;

  // 为switch表，获得和生成一个标签
  label = genlabel();
  cglabel(label);

  // 如果没有case，就创建一个case指向默认case
  if (casecount == 0) {
    caseval[0] = 0;
    caselabel[0] = defaultlabel;
    casecount = 1;
  }
  // 生成switch跳表
  fprintf(Outfile, "\t.quad\t%d\n", casecount);
  for (i = 0; i < casecount; i++)
    fprintf(Outfile, "\t.quad\t%d, L%d\n", caseval[i], caselabel[i]);
  fprintf(Outfile, "\t.quad\tL%d\n", defaultlabel);

  // 加载指定寄存器
  cglabel(toplabel);
  fprintf(Outfile, "\tmovq\t%s, %%rax\n", reglist[reg]);
  fprintf(Outfile, "\tleaq\tL%d(%%rip), %%rdx\n", label);
  fprintf(Outfile, "\tjmp\t__switch\n");
}

// r1的值移动到r2
void cgmove(int r1, int r2) {
  fprintf(Outfile, "\tmovq\t%s, %s\n", reglist[r1], reglist[r2]);
}

// 输出gdb指令来方便debug，知道当前的汇编是来自于源码的哪一行
void cglinenum(int line) {
  // fprintf(Outfile, "\t.loc 1 %d 0\n", line);
}
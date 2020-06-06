#include "defs.h"
#include "data.h"
#include "decl.h"

// 符号表函数
// Copyright (c) 2019 Warren Toomey, GPL3

// 添加一个节点到由head和tail指向的单链表中
void appendsym(struct symtable **head, struct symtable **tail,
	       struct symtable *node) {

  // 检查指针的有效性
  if (head == NULL || tail == NULL || node == NULL)
    fatal("Either head, tail or node is NULL in appendsym");

  // 加到列表
  if (*tail) {
    (*tail)->next = node;
    *tail = node;
  } else
    *head = *tail = node;
  node->next = NULL;
}

// 创建一个符号节点加入到符号表列表
// 设置这个节点：
// + type:  字符类型，整型等
// + ctype: struct/union组合类型指针 
// + stype: 结构化类型，表示变量，函数，数组等等
// + class: 存储类
// + nelems: 数组元素个数或函数的参数个数
// + posn: 本地符号的位置信息
// 返回一个指向新节点的指针
struct symtable *newsym(char *name, int type, struct symtable *ctype,
			int stype, int class, int nelems, int posn) {

  // 分配内存，创建节点
  struct symtable *node = (struct symtable *) malloc(sizeof(struct symtable));
  if (node == NULL)
    fatal("Unable to malloc a symbol table node in newsym");

  // 填充值
  if (name == NULL)
    node->name = NULL;
  else
    node->name = strdup(name);
  node->type = type;
  node->ctype = ctype;
  node->stype = stype;
  node->class = class;
  node->nelems = nelems;

  // 对于指针和整型，直接设置大小
  // 对于structs/union，后面再设置
  if (ptrtype(type) || inttype(type))
    node->size = nelems * typesize(type, ctype);

  node->stu.st_posn = posn;
  node->next = NULL;
  node->member = NULL;
  node->initlist = NULL;
  return (node);
}

// 加一个符号表到全局符号表
struct symtable *addglob(char *name, int type, struct symtable *ctype,
			 int stype, int class, int nelems, int posn) {
  struct symtable *sym =
    newsym(name, type, ctype, stype, class, nelems, posn);
  // 对于structs/union，从它的类型节点那里拷贝大小
  if (type == P_STRUCT || type == P_UNION)
    sym->size = ctype->size;
  appendsym(&Globhead, &Globtail, sym);
  return (sym);
}

// 加一个符号表到本地符号表
struct symtable *addlocl(char *name, int type, struct symtable *ctype,
			 int stype, int nelems) {
  struct symtable *sym = newsym(name, type, ctype, stype, C_LOCAL, nelems, 0);

  // 对于structs/union，从它的类型节点那里拷贝大小
  if (type == P_STRUCT || type == P_UNION)
    sym->size = ctype->size;
  appendsym(&Loclhead, &Locltail, sym);
  return (sym);
}

// 加一个符号表到参数符号表
struct symtable *addparm(char *name, int type, struct symtable *ctype,
			 int stype) {
  struct symtable *sym = newsym(name, type, ctype, stype, C_PARAM, 1, 0);
  appendsym(&Parmhead, &Parmtail, sym);
  return (sym);
}

// 加一个符号表到临时成员表
struct symtable *addmemb(char *name, int type, struct symtable *ctype,
			 int stype, int nelems) {
  struct symtable *sym =
    newsym(name, type, ctype, stype, C_MEMBER, nelems, 0);

  // 对于structs/union，从它的类型节点那里拷贝大小
  if (type == P_STRUCT || type == P_UNION)
    sym->size = ctype->size;
  appendsym(&Membhead, &Membtail, sym);
  return (sym);
}

// 加一个struct到struct列表
struct symtable *addstruct(char *name) {
  struct symtable *sym = newsym(name, P_STRUCT, NULL, 0, C_STRUCT, 0, 0);
  appendsym(&Structhead, &Structtail, sym);
  return (sym);
}

// 加一个union到union列表
struct symtable *addunion(char *name) {
  struct symtable *sym = newsym(name, P_UNION, NULL, 0, C_UNION, 0, 0);
  appendsym(&Unionhead, &Uniontail, sym);
  return (sym);
}

// 加枚举类型或枚举值到枚举列表
// Class是C_ENUMTYPE或C_ENUMVAL.
// posn存储枚举值.
struct symtable *addenum(char *name, int class, int value) {
  struct symtable *sym = newsym(name, P_INT, NULL, 0, class, 0, value);
  appendsym(&Enumhead, &Enumtail, sym);
  return (sym);
}

//加一个typedef到typedef列表
struct symtable *addtypedef(char *name, int type, struct symtable *ctype) {
  struct symtable *sym = newsym(name, type, ctype, 0, C_TYPEDEF, 0, 0);
  appendsym(&Typehead, &Typetail, sym);
  return (sym);
}

// 在指定列表搜素某个符号表
// 如果找到，返回指向找到节点的指针，否则返回NULL
// 如果class不为0，还要匹配class
static struct symtable *findsyminlist(char *s, struct symtable *list,
				      int class) {
  for (; list != NULL; list = list->next)
    if ((list->name != NULL) && !strcmp(s, list->name))
      if (class == 0 || class == list->class)
	return (list);
  return (NULL);
}

// 在全局符号表里面查找
struct symtable *findglob(char *s) {
  return (findsyminlist(s, Globhead, 0));
}


// 在本地符号表里面查找
struct symtable *findlocl(char *s) {
  struct symtable *node;

  // 如果正在解析函数体，在参数表里面找
  if (Functionid) {
    node = findsyminlist(s, Functionid->member, 0);
    if (node)
      return (node);
  }
  return (findsyminlist(s, Loclhead, 0));
}

// 在函数，本地，全局符号表里面查找
struct symtable *findsymbol(char *s) {
  struct symtable *node;

  // 如果正在解析函数体，在参数表里面找
  if (Functionid) {
    node = findsyminlist(s, Functionid->member, 0);
    if (node)
      return (node);
  }
  // 否则，尝试在本地和全局列表里面看看
  node = findsyminlist(s, Loclhead, 0);
  if (node)
    return (node);
  return (findsyminlist(s, Globhead, 0));
}

//在成员列表里面查找成员
struct symtable *findmember(char *s) {
  return (findsyminlist(s, Membhead, 0));
}

// 在struct列表里面查找struct
struct symtable *findstruct(char *s) {
  return (findsyminlist(s, Structhead, 0));
}

// 在union列表里面查找union
struct symtable *findunion(char *s) {
  return (findsyminlist(s, Unionhead, 0));
}

// 在枚举列表里面查找枚举类型
struct symtable *findenumtype(char *s) {
  return (findsyminlist(s, Enumhead, C_ENUMTYPE));
}

// 在枚举列表里面查找枚举值
struct symtable *findenumval(char *s) {
  return (findsyminlist(s, Enumhead, C_ENUMVAL));
}

// 在typedef列表里面查找typedef
struct symtable *findtypedef(char *s) {
  return (findsyminlist(s, Typehead, 0));
}

// 重置所有符号表列表头尾指针的值
void clear_symtable(void) {
  Globhead = Globtail = NULL;
  Loclhead = Locltail = NULL;
  Parmhead = Parmtail = NULL;
  Membhead = Membtail = NULL;
  Structhead = Structtail = NULL;
  Unionhead = Uniontail = NULL;
  Enumhead = Enumtail = NULL;
  Typehead = Typetail = NULL;
}

// 重置跟本地符号表相关的指针
void freeloclsyms(void) {
  Loclhead = Locltail = NULL;
  Parmhead = Parmtail = NULL;
  Functionid = NULL;
}

// 从全局符号表里面删除所有静态符号
void freestaticsyms(void) {
  // g指向当前节点，prev指向前面那个
  struct symtable *g, *prev = NULL;

  // 在全局表里面查找
  for (g = Globhead; g != NULL; g = g->next) {
    if (g->class == C_STATIC) {

      // 如果prev不为空，跳过当前节点
      // 否则， g就是在头部，所以还是跳过当前节点
      if (prev != NULL)
	      prev->next = g->next;
      else
	      Globhead->next = g->next;

      // 如果g在尾部，就把Globtail指向前面那个节点(如果有的话)，或者指向Globhead
      if (g == Globtail) {
        if (prev != NULL)
          Globtail = prev;
        else
          Globtail = Globhead;
      }
    }
     // 在移到下一个节点前，使prev指向g
    prev = g;
  }
}

// 打印一个符号
static void dumpsym(struct symtable *sym, int indent) {
  int i;

  for (i = 0; i < indent; i++)
    printf(" ");
  switch (sym->type & (~0xf)) {
    case P_VOID:
      printf("void ");
      break;
    case P_CHAR:
      printf("char ");
      break;
    case P_INT:
      printf("int ");
      break;
    case P_LONG:
      printf("long ");
      break;
    case P_STRUCT:
      if (sym->ctype != NULL)
	printf("struct %s ", sym->ctype->name);
      else
	printf("struct %s ", sym->name);
      break;
    case P_UNION:
      if (sym->ctype != NULL)
	printf("union %s ", sym->ctype->name);
      else
	printf("union %s ", sym->name);
      break;
    default:
      printf("unknown type ");
  }

  for (i = 0; i < (sym->type & 0xf); i++)
    printf("*");
  printf("%s", sym->name);

  switch (sym->stype) {
    case S_VARIABLE:
      break;
    case S_FUNCTION:
      printf("()");
      break;
    case S_ARRAY:
      printf("[]");
      break;
    default:
      printf(" unknown stype");
  }

  switch (sym->class) {
    case C_GLOBAL:
      printf(": global");
      break;
    case C_LOCAL:
      printf(": local");
      break;
    case C_PARAM:
      printf(": param");
      break;
    case C_EXTERN:
      printf(": extern");
      break;
    case C_STATIC:
      printf(": static");
      break;
    case C_STRUCT:
      printf(": struct");
      break;
    case C_UNION:
      printf(": union");
      break;
    case C_MEMBER:
      printf(": member");
      break;
    case C_ENUMTYPE:
      printf(": enumtype");
      break;
    case C_ENUMVAL:
      printf(": enumval");
      break;
    case C_TYPEDEF:
      printf(": typedef");
      break;
    default:
      printf(": unknown class");
  }

  switch (sym->stype) {
    case S_VARIABLE:
      if (sym->class == C_ENUMVAL)
	printf(", value %d\n", sym->stu.st_posn);
      else
	printf(", size %d\n", sym->size);
      break;
    case S_FUNCTION:
      printf(", %d params\n", sym->nelems);
      break;
    case S_ARRAY:
      printf(", %d elems, size %d\n", sym->nelems, sym->size);
      break;
  }

  switch (sym->type & (~0xf)) {
    case P_STRUCT:
    case P_UNION:
      dumptable(sym->member, NULL, 4);
  }

  switch (sym->stype) {
    case S_FUNCTION:
      dumptable(sym->member, NULL, 4);
  }
}

// 打印一个符号表
void dumptable(struct symtable *head, char *name, int indent) {
  struct symtable *sym;

  if (head != NULL && name != NULL)
    printf("%s\n--------\n", name);
  for (sym = head; sym != NULL; sym = sym->next)
    dumpsym(sym, indent);
}

void dumpsymtables(void) {
  dumptable(Globhead, "Global", 0);
  printf("\n");
  dumptable(Enumhead, "Enums", 0);
  printf("\n");
  dumptable(Typehead, "Typedefs", 0);
}
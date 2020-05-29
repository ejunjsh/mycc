#include "defs.h"
#include "data.h"
#include "decl.h"

// 解析声明
// Copyright (c) 2019 Warren Toomey, GPL3

static struct symtable *composite_declaration(int type);
static int typedef_declaration(struct symtable **ctype);
static int type_of_typedef(char *name, struct symtable **ctype);
static void enum_declaration(void);

// 解析当前token和返回一个原始类型枚举值，
// 返回一个指向复合类型的指针
// 可能会修改类型的存储类
int parse_type(struct symtable **ctype, int *class) {
  int type, exstatic = 1;

  // 是否有一个存储类变成extern或者static
  while (exstatic) {
    switch (Token.token) {
      case T_EXTERN:
        if (*class == C_STATIC)
            fatal("Illegal to have extern and static at the same time");
        *class = C_EXTERN;
        scan(&Token);
        break;
      case T_STATIC:
        if (*class == C_LOCAL)
            fatal("Compiler doesn't support static local declarations");
        if (*class == C_EXTERN)
            fatal("Illegal to have extern and static at the same time");
        *class = C_STATIC;
        scan(&Token);
        break;
      default:
	    exstatic = 0;
    }
  }

  // 现在开始解析类型关键字
  switch (Token.token) {
    case T_VOID:
      type = P_VOID;
      scan(&Token);
      break;
    case T_CHAR:
      type = P_CHAR;
      scan(&Token);
      break;
    case T_INT:
      type = P_INT;
      scan(&Token);
      break;
    case T_LONG:
      type = P_LONG;
      scan(&Token);
      break;

      // 如果类型后面跟着';' ，那就是代表不是在用类型声明变量
      // 而是声明类型，所以返回-1
      // 例如: struct x {int y; int z};
    case T_STRUCT:
      type = P_STRUCT;
      *ctype = composite_declaration(P_STRUCT);
      if (Token.token == T_SEMI)
	    type = -1;
      break;
    case T_UNION:
      type = P_UNION;
      *ctype = composite_declaration(P_UNION);
      if (Token.token == T_SEMI)
	    type = -1;
      break;
    case T_ENUM:
      type = P_INT;		// 枚举其实就是整型
      enum_declaration();
      if (Token.token == T_SEMI)
	    type = -1;
      break;
    case T_TYPEDEF:
      type = typedef_declaration(ctype);
      if (Token.token == T_SEMI)
	    type = -1;
      break;
    case T_IDENT:
      type = type_of_typedef(Text, ctype);
      break;
    default:
      fatals("Illegal type, token", Token.tokstr);
  }
  return (type);
}

// 给定一个由函数parse_type()解析过的type，解析接下来出现的'*'，并返回新类型
int parse_stars(int type) {

  while (1) {
    if (Token.token != T_STAR)
      break;
    type = pointer_to(type);
    scan(&Token);
  }
  return (type);
}

// 解析cast（类型转换）
int parse_cast(struct symtable **ctype) {
  int type, class = 0;

  // 从括号里面获取类型
  type = parse_stars(parse_type(ctype, &class));

  // 错误检查，目前不支持，强转到复合类型
  if (type == P_STRUCT || type == P_UNION || type == P_VOID)
    fatal("Cannot cast to a struct, union or void type");
  return (type);
}

// 给定一个类型type，解析一个字面量表达式，保证type跟表达式的类型一致
// 解析任何在表达式之前类型转换（cast）
// 如果是整型字面量，返回该值
// 如果是字符串字面量，返回标签号
int parse_literal(int type) {
  struct ASTnode *tree;

  // 解析表达式，并优化解析出来的语法树tree
  tree = optimise(binexpr(0));

  // 如果这里有个cast，获取子树应用到tree上
  if (tree->op == A_CAST) {
    tree->left->type = tree->type;
    tree = tree->left;
  }

  // 这颗tree现在必须是个整型或者字符串字面量
  if (tree->op != A_INTLIT && tree->op != A_STRLIT)
    fatal("Cannot initialise globals with a general expression");

  // 如果type是char*
  if (type == pointer_to(P_CHAR)) {
    // tree是字符字面量，返回标签号
    if (tree->op == A_STRLIT)
      return (tree->a_intvalue);
    // tree是整型零值，所以是个NULL，所以返回0
    if (tree->op == A_INTLIT && tree->a_intvalue == 0)
      return (0);
  }

  // 代码走到这里就是处理整型字面量量
  // 输入的type必须是个整型，同时足够宽来容纳这个字面值
  if (inttype(type) && typesize(type, NULL) >= typesize(tree->type, NULL))
    return (tree->a_intvalue);

  fatal("Type mismatch: literal vs. variable");
  return (0);		
}

// 给定一个符号表指针，如果该符号不存在，返回true
// 用这个函数来转换extern（外部）到global（全局）
static int is_new_symbol(struct symtable *sym, int class, 
		  int type, struct symtable *ctype) {

  // 不存在，代表是新的
  if (sym==NULL) return(1);

  // 如果类型匹配，就转换为global（全局）
  if ((sym->class== C_GLOBAL && class== C_EXTERN)
      || (sym->class== C_EXTERN && class== C_GLOBAL)) {

      // 类型不匹配
      if (type != sym->type)
        fatals("Type mismatch between global/extern", sym->name);

      // Struct/unions, 比较ctype
      if (type >= P_STRUCT && ctype != sym->ctype)
        fatals("Type mismatch between global/extern", sym->name);

      // 到这里，代表类型匹配来，转换为global（全局）
      sym->class= C_GLOBAL;
      // 返回0，表示符号不是新的
      return(0);
  }

  // 如果到了这里，代表重复定义
  fatals("Duplicate global variable declaration", sym->name);
  return(-1);	
}

// 给定标量变量的类型（type），变量名（name），存储类（class），
// 解析初始化值，和分配空间给他
// 返回该变量的符号表指针
// *什么是标量变量，其实就是除开函数，数组之外的变量，应该没错吧
static struct symtable *scalar_declaration(char *varname, int type,
					   struct symtable *ctype,
					   int class, struct ASTnode **tree) {
  struct symtable *sym = NULL;
  struct ASTnode *varnode, *exprnode;
  *tree = NULL;

  // 加入到相应的符号表
  switch (class) {
    case C_STATIC:
    case C_EXTERN:
    case C_GLOBAL:
      // 检查变量是新的还是已经存在
      sym= findglob(varname);
      if (is_new_symbol(sym, class, type, ctype))
        sym = addglob(varname, type, ctype, S_VARIABLE, class, 1, 0);
      break;
    case C_LOCAL:
      sym = addlocl(varname, type, ctype, S_VARIABLE, 1);
      break;
    case C_PARAM:
      sym = addparm(varname, type, ctype, S_VARIABLE);
      break;
    case C_MEMBER:
      sym = addmemb(varname, type, ctype, S_VARIABLE, 1);
      break;
  }

  // 这个变量需要初始化
  if (Token.token == T_ASSIGN) {
    // 只能在全局或者本地
    if (class != C_GLOBAL && class != C_LOCAL && class != C_STATIC)
      fatals("Variable can not be initialised", varname);
    scan(&Token);

    // 全局只能是字面量值
    if (class == C_GLOBAL || class == C_STATIC) {
      // 为这个变量创建一个初始值，同时解析这个值
      sym->initlist = (int *) malloc(sizeof(int));
      sym->initlist[0] = parse_literal(type);
    }
    // 处理本地
    if (class == C_LOCAL) {
      // 为这个变量创建一个A_IDENT语法树节点
      varnode = mkastleaf(A_IDENT, sym->type, sym->ctype, sym, 0);

      // 获得这个赋值的表达式语法树，同时令他右值
      exprnode = binexpr(0);
      exprnode->rvalue = 1;

      // 保证表达式类型匹配这个变量
      exprnode = modify_type(exprnode, varnode->type, varnode->ctype, 0);
      if (exprnode == NULL)
	      fatal("Incompatible expression in assignment");

      // 创建一个赋值语法树
      *tree = mkastnode(A_ASSIGN, exprnode->type, exprnode->ctype, exprnode,
			NULL, varnode, NULL, 0);
    }
  }

  // 如果包含全局或静态变量定义的话，创建全局空间
  if (class == C_GLOBAL || class == C_STATIC)
    genglobsym(sym);

  return (sym);
}

// 给定数组变量的类型（type），变量名（name），存储类（class），
// 解析数组大小，如果有，解析初始化值并分配空间给它
// 返回该变量的符号表指针
static struct symtable *array_declaration(char *varname, int type,
					  struct symtable *ctype, int class) {

  struct symtable *sym;	// 新符号表指针
  int nelems = -1;	// 假设元素数量没有给定
  int maxelems;		// 在初始化列表中最大元素数量
  int *initlist;	// 初始化元素列表 
  int i = 0, j;

  // 跳过'['
  scan(&Token);

  // 看看是否有数组大小
  if (Token.token != T_RBRACKET) {
    nelems = parse_literal(P_INT);
    if (nelems <= 0)
      fatald("Array size is illegal", nelems);
  }

  // 保证接下来一定是']'
  match(T_RBRACKET, "]");

  // 加到符号表 
  // 这里把数组当成指向元素类型指针来处理
  switch (class) {
    case C_STATIC:
    case C_EXTERN:
    case C_GLOBAL:
      // 检查看看变量是否已经存在
      sym= findglob(varname);
      if (is_new_symbol(sym, class, pointer_to(type), ctype))
        sym = addglob(varname, pointer_to(type), ctype, S_ARRAY, class, 0, 0);
      break;
    case C_LOCAL:
      sym = addlocl(varname, pointer_to(type), ctype, S_ARRAY, 0);
      break;
    default:
      fatal("Declaration of array parameters is not implemented");
  }

  // 数组初始化
  if (Token.token == T_ASSIGN) {
    if (class != C_GLOBAL && class != C_STATIC)
      fatals("Variable can not be initialised", varname);
    scan(&Token);

    // 匹配"{"
    match(T_LBRACE, "{");

#define TABLE_INCREMENT 10

    // 如果知道数组大小，直接分配内存
    // 否则先分配TABLE_INCREMENT个内存
    if (nelems != -1)
      maxelems = nelems;
    else
      maxelems = TABLE_INCREMENT;
    initlist = (int *) malloc(maxelems * sizeof(int));

    // 循环从列表获得一个新的字面量值
    while (1) {

      // 看看我们能不能加下一个值
      if (nelems != -1 && i == maxelems)
	      fatal("Too many values in initialisation list");

      initlist[i++] = parse_literal(type);

      // 如果我们不知道数组大小的情况并到达列表最后，
      // 重新分配列表大小，大小为之前大小加上TABLE_INCREMENT
      if (nelems == -1 && i == maxelems) {
        maxelems += TABLE_INCREMENT;
        initlist = (int *) realloc(initlist, maxelems * sizeof(int));
      }

      // 如果遇到"}",就退出
      if (Token.token == T_RBRACE) {
        scan(&Token);
        break;
      }

      // 下一个肯定是逗号
      comma();
    }

    // 对于a[7]={1,2,3};这种情况，用零来填充还没有初始化的值
    // 把初始化列表放入符号表中
    for (j = i; j < sym->nelems; j++)
      initlist[j] = 0;

    if (i > nelems)
      nelems = i;
    sym->initlist = initlist;
  }

  // 设置数组大小和元素个数
  // 只有extern（外部）变量可以没有元素
  if (class != C_EXTERN && nelems<=0)
    fatals("Array must have non-zero elements", sym->name);

  sym->nelems = nelems;
  sym->size = sym->nelems * typesize(type, ctype);
  // 如果包含全局或静态变量定义的话，创建全局空间
  if (class == C_GLOBAL || class == C_STATIC)
    genglobsym(sym);
  return (sym);
}

// 给定一个新函数的符号表，和一个可能是NULL的旧函数符号表（函数原型）
// 解析参数列表，跟旧的函数符号表比较
// 返回函数参数个数
static int param_declaration_list(struct symtable *oldfuncsym,
				  struct symtable *newfuncsym) {
  int type, paramcnt = 0;
  struct symtable *ctype;
  struct symtable *protoptr = NULL;
  struct ASTnode *unused;

  // 获取旧函数的成员
  if (oldfuncsym != NULL)
    protoptr = oldfuncsym->member;

  // 循环获取参数
  while (Token.token != T_RPAREN) {

    // 如果第一个token是'void'
    if (Token.token == T_VOID) {
      // 偷看下个token，如果是')', 表示函数没有参数，离开循环  
      scan(&Peektoken);
      if (Peektoken.token == T_RPAREN) {
        // 把Peektoken放回到Token
        paramcnt = 0;
        scan(&Token);
        break;
      }
    }

    // 获得下一个参数的类型
    type = declaration_list(&ctype, C_PARAM, T_COMMA, T_RPAREN, &unused);
    if (type == -1)
      fatal("Bad type in parameter list");

    // 保证这个参数的类型跟旧的匹配
    if (protoptr != NULL) {
      if (type != protoptr->type)
	      fatald("Type doesn't match prototype for parameter", paramcnt + 1);
      protoptr = protoptr->next;
    }
    paramcnt++;

    // 遇到右括号，退出循环
    if (Token.token == T_RPAREN)
      break;
    // 否则看看是否是个逗号
    comma();
  }

  if (oldfuncsym != NULL && paramcnt != oldfuncsym->nelems)
    fatals("Parameter count mismatch for function", oldfuncsym->name);

  // 返回函数参数
  return (paramcnt);
}

//
// function_declaration: type identifier '(' parameter_list ')' ;
//      | type identifier '(' parameter_list ')' compound_statement   ;
//
// 解析函数声明
static struct symtable *function_declaration(char *funcname, int type,
					     struct symtable *ctype,
					     int class) {
  struct ASTnode *tree, *finalstmt;
  struct symtable *oldfuncsym, *newfuncsym = NULL;
  int endlabel, paramcnt;
  int linenum= Line;

  // 如果如果之前有这个函数（原型），获取函数id。如果不存在设置oldfuncsym为空
  if ((oldfuncsym = findsymbol(funcname)) != NULL)
    if (oldfuncsym->stype != S_FUNCTION)
      oldfuncsym = NULL;

  // 如果这是个新函数定义，获取函数结束的标签号，
  // 并把函数加入到符号表
  if (oldfuncsym == NULL) {
    endlabel = genlabel();
    // 假设：函数只会返回标量，所以设置NULL在ctype参数
    newfuncsym =
      addglob(funcname, type, NULL, S_FUNCTION, class, 0, endlabel);
  }

  // 扫描'(',参数和 ')'
  // 传递已存在的函数原型符号指针到param_declaration_list函数
  lparen();
  paramcnt = param_declaration_list(oldfuncsym, newfuncsym);
  rparen();

  // 如果是新函数声明，更新函数符号表的函数参数数量
  // 同时拷贝参数列表到函数符号表里面
  if (newfuncsym) {
    newfuncsym->nelems = paramcnt;
    newfuncsym->member = Parmhead;
    oldfuncsym = newfuncsym;
  }

  // 清空参数列表
  Parmhead = Parmtail = NULL;

  // 如果声明后面跟着分号，那就是个函数原型
  if (Token.token == T_SEMI)
    return (oldfuncsym);

  // 这不是个原型
  // 设置函数符号表指针到Functionid
  Functionid = oldfuncsym;

  // 从组合语句中获得语法树
  // 同时清空以下两个标记，代表没有循环和开关（switch）
  Looplevel = 0;
  Switchlevel = 0;
  lbrace();
  tree = compound_statement(0);
  rbrace();

  // 如果函数返回类型不是P_VOID
  if (type != P_VOID) {

    // 如果函数没有语句，报错
    if (tree == NULL)
      fatal("No statements in function with non-void type");

    // 检查最后个语法树操作，是否是个返回语句
    finalstmt = (tree->op == A_GLUE) ? tree->right : tree;
    if (finalstmt == NULL || finalstmt->op != A_RETURN)
      fatal("No return for function with non-void type");
  }

  // 创建一个A_FUNCTION语法树节点，它包含了函数符号指针和组合语句子树
  tree = mkastunary(A_FUNCTION, type, ctype, tree, oldfuncsym, endlabel);
  tree->linenum= linenum;

  // 优化树
  tree = optimise(tree);

  // 如果需要，打印语法树
  if (O_dumpAST) {
    dumpAST(tree, NOLABEL, 0);
    fprintf(stdout, "\n\n");
  }

  // 生成汇编
  genAST(tree, NOLABEL, NOLABEL, NOLABEL, 0);

  // 清空跟这个函数相关的符号
  freeloclsyms();
  return (oldfuncsym);
}

// 解析复合类型声明：struct/union
// 找到一个存在或者创建一个新的符号表并返回它的指针
static struct symtable *composite_declaration(int type) {
  struct symtable *ctype = NULL;
  struct symtable *m;
  struct ASTnode *unused;
  int offset;
  int t;

  // 跳过struct/union关键字
  scan(&Token);

  // 看看是否接下来是个struct/union名
  if (Token.token == T_IDENT) {
    // 查找任何匹配的复合类型
    if (type == P_STRUCT)
      ctype = findstruct(Text);
    else
      ctype = findunion(Text);
    scan(&Token);
  }

  // 如果接下来不是是左括号，代表现在在用已存在的struct/union类型
  // 直接返回指向类型的指针
  if (Token.token != T_LBRACE) {
    if (ctype == NULL)
      fatals("unknown struct/union type", Text);
    return (ctype);
  }

  // 保证struct/union类型之前没有被定义
  if (ctype)
    fatals("previously defined struct/union", Text);

  // 创建这个复合类型，跳过左括号
  if (type == P_STRUCT)
    ctype = addstruct(Text);
  else
    ctype = addunion(Text);
  scan(&Token);

  // 扫描成员列表
  while (1) {
    // 获得下个成员放在变量m
    t = declaration_list(&m, C_MEMBER, T_SEMI, T_RBRACE, &unused);
    if (t == -1)
      fatal("Bad type in member list");
    if (Token.token == T_SEMI)
      scan(&Token);
    if (Token.token == T_RBRACE)
      break;
  }

  // 把成员列表附着在复合类型节点上
  rbrace();
  if (Membhead == NULL)
    fatals("No members in struct", ctype->name);
  ctype->member = Membhead;
  Membhead = Membtail = NULL;

  // 设置第一个成员的位置，然后找到下一个空闲字节的位移
  m = ctype->member;
  m->st_posn = 0;
  offset = typesize(m->type, m->ctype);

  // 设置每个成员的位置
  // union很简单，都是0，对于struct，要对齐成员，并找出下个空闲字节的位移
  for (m = m->next; m != NULL; m = m->next) {
    // 为成员设置所在位置
    if (type == P_STRUCT){
      m->st_posn = genalign(m->type, offset, 1);
      offset = m->st_posn + typesize(m->type, m->ctype); // 获得当前成员下个空闲字节的位移
    }
    else{
      m->st_posn = 0;
      offset = typesize(m->type, m->ctype) > offset ? typesize(m->type, m->ctype) : offset; // 对于union来说，取最大的那个成员作为大小
    }
  }

  // 设置复合类型的大小
  // TODO：check this later !done
  ctype->size = offset;
  return (ctype);
}

// 解析枚举声明
static void enum_declaration(void) {
  struct symtable *etype = NULL;
  char *name = NULL;
  int intval = 0;

  // 跳过枚举关键字
  scan(&Token);

  // 如果接下来是枚举类型名，
  // 获得一个指向存在枚举类型节点的指针
  if (Token.token == T_IDENT) {
    etype = findenumtype(Text);
    name = strdup(Text); // 拷贝出来
    scan(&Token);
  }

  // 如果下个token不是个左括号
  // 检查是否存在这个枚举类型，然后返回
  if (Token.token != T_LBRACE) {
    if (etype == NULL)
      fatals("undeclared enum type:", name);
    return;
  }

  // 跳过左括号
  scan(&Token);

  // 如果之前有声明过，报错
  if (etype != NULL)
    fatals("enum type redeclared:", etype->name);
  else
    // 创建一个枚举类型节点
    etype = addenum(name, C_ENUMTYPE, 0);

  // 循环获得所有枚举值
  while (1) {

    // 保证我们有一个标识符    
    ident();
    name = strdup(Text); // 拷贝出来

    // 保证之前枚举值没有声明过
    etype = findenumval(name);
    if (etype != NULL)
      fatals("enum value redeclared:", Text);

    // 如果下个token是'=',跳过它去获取接下来的整型字面量
    if (Token.token == T_ASSIGN) {
      scan(&Token);
      if (Token.token != T_INTLIT)
	      fatal("Expected int literal after '='");
      intval = Token.intvalue;
      scan(&Token);
    }

    // 创建一个枚举值节点
    // 为下个枚举值加一
    etype = addenum(name, C_ENUMVAL, intval++);

    // 如果是右括号，退出
    // 否则匹配逗号，然后继续
    if (Token.token == T_RBRACE)
      break;
    comma();
  }
  scan(&Token);			// 跳过右括号
}

// Parse a typedef declaration and return the type
// and ctype that it represents
static int typedef_declaration(struct symtable **ctype) {
  int type, class = 0;

  // 跳过typedef关键字.
  scan(&Token);

  // 获得接下来实际的类型
  type = parse_type(ctype, &class);
  if (class != 0)
    fatal("Can't have static/extern in a typedef declaration");

  // 检查看看是否已经存在
  if (findtypedef(Text) != NULL)
    fatals("redefinition of typedef", Text);

  // 解析指针类型
  // TODO：这里有问题
  type = parse_stars(type);

  // 不存在就把它加入到typedef列表
  addtypedef(Text, type, *ctype);
  scan(&Token);
  return (type);
}

// 给定一个typedef名字，返回它表示的类型
static int type_of_typedef(char *name, struct symtable **ctype) {
  struct symtable *t;

  // 查找typedef列表
  t = findtypedef(name);
  if (t == NULL)
    fatals("unknown type", name);
  scan(&Token);
  *ctype = t->ctype;
  return (t->type);
}

// 解析变量或函数的声明
// 类型和接下来的*已经被扫描了，同时标识符已经扫描进Token变量
// class参数是当前符号的存储类
// 返回这个符号表的指针
static struct symtable *symbol_declaration(int type, struct symtable *ctype,
					   int class, struct ASTnode **tree) {
  struct symtable *sym = NULL;
  char *varname = strdup(Text); // 拷贝出来以防后面Text被覆盖

  // 保证我们有个标识符
  ident();

  // 处理函数声明
  if (Token.token == T_LPAREN) {
    return (function_declaration(varname, type, ctype, class));
  }

  // 检查变量是否已经声明过了
  switch (class) {
    case C_EXTERN:
    case C_STATIC:
    case C_GLOBAL:
    case C_LOCAL:
    case C_PARAM:
      if (findlocl(varname) != NULL)
	      fatals("Duplicate local variable declaration", varname);
    case C_MEMBER:
      if (findmember(varname) != NULL)
	      fatals("Duplicate struct/union member declaration", varname);
  }

  // 把数组或标量变量加入到符号表
  if (Token.token == T_LBRACKET) {
    sym = array_declaration(varname, type, ctype, class);
    *tree= NULL;	// 本地数组不支持初始化
  } else
    sym = scalar_declaration(varname, type, ctype, class, tree);
  return (sym);
}

// 解析一列表的符号，它们都有同一个类型，例如 int a=0,b=0,c=1; 这个函数也支持普通变量或函数的解析
// 返回符号们的类型，et1和et2是结束token
int declaration_list(struct symtable **ctype, int class, int et1, int et2,
		     struct ASTnode **gluetree) {
  int inittype, type;
  struct symtable *sym;
  struct ASTnode *tree;
  *gluetree = NULL; // TODO：有问题？

  // 获取初始的类型， 如果是-1，代表是个复合类型声明，直接返回
  if ((inittype = parse_type(ctype, &class)) == -1)
    return (inittype);

  // 现在解析一列表符号
  while (1) {
    // 看看这个符号是否是个指针
    type = parse_stars(inittype);

    // 解析符号
    sym = symbol_declaration(type, *ctype, class, &tree);

    // 解析到函数，直接返回
    if (sym->stype == S_FUNCTION) {
      if (class != C_GLOBAL && class != C_STATIC)
	      fatal("Function definition not at global level");
      return (type);
    }

    // 把tree粘起来（Glue）
    // 这只在本地变量声明并有赋值操作的时候，
    // 这样就能把一系列赋值操作连起来
    if (*gluetree == NULL)
      *gluetree = tree;
    else
      *gluetree =
	      mkastnode(A_GLUE, P_NONE, NULL, *gluetree, NULL, tree, NULL, 0);

    // 遇到结束token，返回
    if (Token.token == et1 || Token.token == et2)
      return (type);

    // 否则，匹配逗号，继续
    comma();
  }

  return(0);	
}

// 解析全局声明，包括变量，函数，struct/union
void global_declarations(void) {
  struct symtable *ctype= NULL;
  struct ASTnode *unused;

  // 循环解析直到文件结束
  while (Token.token != T_EOF) {
    declaration_list(&ctype, C_GLOBAL, T_SEMI, T_EOF, &unused);

    // 跳过分号
    if (Token.token == T_SEMI)
      scan(&Token);
  }
}
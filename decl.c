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
// 解析函数定义
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
    // Assumption: functions only return scalar types, so NULL below
    newfuncsym =
      addglob(funcname, type, NULL, S_FUNCTION, class, 0, endlabel);
  }

  // Scan in the '(', any parameters and the ')'.
  // Pass in any existing function prototype pointer
  lparen();
  paramcnt = param_declaration_list(oldfuncsym, newfuncsym);
  rparen();

  // If this is a new function declaration, update the
  // function symbol entry with the number of parameters.
  // Also copy the parameter list into the function's node.
  if (newfuncsym) {
    newfuncsym->nelems = paramcnt;
    newfuncsym->member = Parmhead;
    oldfuncsym = newfuncsym;
  }

  // Clear out the parameter list
  Parmhead = Parmtail = NULL;

  // If the declaration ends in a semicolon, only a prototype.
  if (Token.token == T_SEMI)
    return (oldfuncsym);

  // This is not just a prototype.
  // Set the Functionid global to the function's symbol pointer
  Functionid = oldfuncsym;

  // Get the AST tree for the compound statement and mark
  // that we have parsed no loops or switches yet
  Looplevel = 0;
  Switchlevel = 0;
  lbrace();
  tree = compound_statement(0);
  rbrace();

  // If the function type isn't P_VOID ...
  if (type != P_VOID) {

    // Error if no statements in the function
    if (tree == NULL)
      fatal("No statements in function with non-void type");

    // Check that the last AST operation in the
    // compound statement was a return statement
    finalstmt = (tree->op == A_GLUE) ? tree->right : tree;
    if (finalstmt == NULL || finalstmt->op != A_RETURN)
      fatal("No return for function with non-void type");
  }

  // Build the A_FUNCTION node which has the function's symbol pointer
  // and the compound statement sub-tree
  tree = mkastunary(A_FUNCTION, type, ctype, tree, oldfuncsym, endlabel);
  tree->linenum= linenum;

  // Do optimisations on the AST tree
  tree = optimise(tree);

  // Dump the AST tree if requested
  if (O_dumpAST) {
    dumpAST(tree, NOLABEL, 0);
    fprintf(stdout, "\n\n");
  }

  // Generate the assembly code for it
  genAST(tree, NOLABEL, NOLABEL, NOLABEL, 0);

  // Now free the symbols associated with this function
  freeloclsyms();
  return (oldfuncsym);
}

// Parse composite type declarations: structs or unions.
// Either find an existing struct/union declaration, or build
// a struct/union symbol table entry and return its pointer.
static struct symtable *composite_declaration(int type) {
  struct symtable *ctype = NULL;
  struct symtable *m;
  struct ASTnode *unused;
  int offset;
  int t;

  // Skip the struct/union keyword
  scan(&Token);

  // See if there is a following struct/union name
  if (Token.token == T_IDENT) {
    // Find any matching composite type
    if (type == P_STRUCT)
      ctype = findstruct(Text);
    else
      ctype = findunion(Text);
    scan(&Token);
  }

  // If the next token isn't an LBRACE , this is
  // the usage of an existing struct/union type.
  // Return the pointer to the type.
  if (Token.token != T_LBRACE) {
    if (ctype == NULL)
      fatals("unknown struct/union type", Text);
    return (ctype);
  }

  // Ensure this struct/union type hasn't been
  // previously defined
  if (ctype)
    fatals("previously defined struct/union", Text);

  // Build the composite type and skip the left brace
  if (type == P_STRUCT)
    ctype = addstruct(Text);
  else
    ctype = addunion(Text);
  scan(&Token);

  // Scan in the list of members
  while (1) {
    // Get the next member. m is used as a dummy
    t = declaration_list(&m, C_MEMBER, T_SEMI, T_RBRACE, &unused);
    if (t == -1)
      fatal("Bad type in member list");
    if (Token.token == T_SEMI)
      scan(&Token);
    if (Token.token == T_RBRACE)
      break;
  }

  // Attach to the struct type's node
  rbrace();
  if (Membhead == NULL)
    fatals("No members in struct", ctype->name);
  ctype->member = Membhead;
  Membhead = Membtail = NULL;

  // Set the offset of the initial member
  // and find the first free byte after it
  m = ctype->member;
  m->st_posn = 0;
  offset = typesize(m->type, m->ctype);

  // Set the position of each successive member in the composite type
  // Unions are easy. For structs, align the member and find the next free byte
  for (m = m->next; m != NULL; m = m->next) {
    // Set the offset for this member
    if (type == P_STRUCT)
      m->st_posn = genalign(m->type, offset, 1);
    else
      m->st_posn = 0;

    // Get the offset of the next free byte after this member
    offset += typesize(m->type, m->ctype);
  }

  // Set the overall size of the composite type
  ctype->size = offset;
  return (ctype);
}

// Parse an enum declaration
static void enum_declaration(void) {
  struct symtable *etype = NULL;
  char *name = NULL;
  int intval = 0;

  // Skip the enum keyword.
  scan(&Token);

  // If there's a following enum type name, get a
  // pointer to any existing enum type node.
  if (Token.token == T_IDENT) {
    etype = findenumtype(Text);
    name = strdup(Text);	// As it gets tromped soon
    scan(&Token);
  }

  // If the next token isn't a LBRACE, check
  // that we have an enum type name, then return
  if (Token.token != T_LBRACE) {
    if (etype == NULL)
      fatals("undeclared enum type:", name);
    return;
  }

  // We do have an LBRACE. Skip it
  scan(&Token);

  // If we have an enum type name, ensure that it
  // hasn't been declared before.
  if (etype != NULL)
    fatals("enum type redeclared:", etype->name);
  else
    // Build an enum type node for this identifier
    etype = addenum(name, C_ENUMTYPE, 0);

  // Loop to get all the enum values
  while (1) {
    // Ensure we have an identifier
    // Copy it in case there's an int literal coming up
    ident();
    name = strdup(Text);

    // Ensure this enum value hasn't been declared before
    etype = findenumval(name);
    if (etype != NULL)
      fatals("enum value redeclared:", Text);

    // If the next token is an '=', skip it and
    // get the following int literal
    if (Token.token == T_ASSIGN) {
      scan(&Token);
      if (Token.token != T_INTLIT)
	fatal("Expected int literal after '='");
      intval = Token.intvalue;
      scan(&Token);
    }

    // Build an enum value node for this identifier.
    // Increment the value for the next enum identifier.
    etype = addenum(name, C_ENUMVAL, intval++);

    // Bail out on a right curly bracket, else get a comma
    if (Token.token == T_RBRACE)
      break;
    comma();
  }
  scan(&Token);			// Skip over the right curly bracket
}

// Parse a typedef declaration and return the type
// and ctype that it represents
static int typedef_declaration(struct symtable **ctype) {
  int type, class = 0;

  // Skip the typedef keyword.
  scan(&Token);

  // Get the actual type following the keyword
  type = parse_type(ctype, &class);
  if (class != 0)
    fatal("Can't have static/extern in a typedef declaration");

  // See if the typedef identifier already exists
  if (findtypedef(Text) != NULL)
    fatals("redefinition of typedef", Text);

  // Get any following '*' tokens
  type = parse_stars(type);

  // It doesn't exist so add it to the typedef list
  addtypedef(Text, type, *ctype);
  scan(&Token);
  return (type);
}

// 给定一个typedef名字，返回它表示的类型
static int type_of_typedef(char *name, struct symtable **ctype) {
  struct symtable *t;

  // Look up the typedef in the list
  t = findtypedef(name);
  if (t == NULL)
    fatals("unknown type", name);
  scan(&Token);
  *ctype = t->ctype;
  return (t->type);
}

// Parse the declaration of a variable or function.
// The type and any following '*'s have been scanned, and we
// have the identifier in the Token variable.
// The class argument is the symbol's class.
// Return a pointer to the symbol's entry in the symbol table
static struct symtable *symbol_declaration(int type, struct symtable *ctype,
					   int class, struct ASTnode **tree) {
  struct symtable *sym = NULL;
  char *varname = strdup(Text);

  // Ensure that we have an identifier. 
  // We copied it above so we can scan more tokens in, e.g.
  // an assignment expression for a local variable.
  ident();

  // Deal with function declarations
  if (Token.token == T_LPAREN) {
    return (function_declaration(varname, type, ctype, class));
  }

  // See if this array or scalar variable has already been declared
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

  // Add the array or scalar variable to the symbol table
  if (Token.token == T_LBRACKET) {
    sym = array_declaration(varname, type, ctype, class);
    *tree= NULL;	// Local arrays are not initialised
  } else
    sym = scalar_declaration(varname, type, ctype, class, tree);
  return (sym);
}

// Parse a list of symbols where there is an initial type.
// Return the type of the symbols. et1 and et2 are end tokens.
int declaration_list(struct symtable **ctype, int class, int et1, int et2,
		     struct ASTnode **gluetree) {
  int inittype, type;
  struct symtable *sym;
  struct ASTnode *tree;
  *gluetree = NULL;

  // Get the initial type. If -1, it was
  // a composite type definition, return this
  if ((inittype = parse_type(ctype, &class)) == -1)
    return (inittype);

  // Now parse the list of symbols
  while (1) {
    // See if this symbol is a pointer
    type = parse_stars(inittype);

    // Parse this symbol
    sym = symbol_declaration(type, *ctype, class, &tree);

    // We parsed a function, there is no list so leave
    if (sym->stype == S_FUNCTION) {
      if (class != C_GLOBAL && class != C_STATIC)
	fatal("Function definition not at global level");
      return (type);
    }

    // Glue any AST tree from a local declaration
    // to build a sequence of assignments to perform
    if (*gluetree == NULL)
      *gluetree = tree;
    else
      *gluetree =
	mkastnode(A_GLUE, P_NONE, NULL, *gluetree, NULL, tree, NULL, 0);

    // We are at the end of the list, leave
    if (Token.token == et1 || Token.token == et2)
      return (type);

    // Otherwise, we need a comma as separator
    comma();
  }

  return(0);	// Keep -Wall happy
}

// Parse one or more global declarations,
// either variables, functions or structs
void global_declarations(void) {
  struct symtable *ctype= NULL;
  struct ASTnode *unused;

  // Loop parsing one declaration list until the end of file
  while (Token.token != T_EOF) {
    declaration_list(&ctype, C_GLOBAL, T_SEMI, T_EOF, &unused);

    // Skip any separating semicolons
    if (Token.token == T_SEMI)
      scan(&Token);
  }
}
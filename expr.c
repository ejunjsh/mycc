#include "defs.h"
#include "data.h"
#include "decl.h"

// 表达式解析
// Copyright (c) 2019 Warren Toomey, GPL3

// expression_list: <null>
//        | expression
//        | expression ',' expression_list
//        ;


// 解析由逗号分隔的表达式列表（列表可能为空），并返回语法树（树也可能为空）
// 语法树由A_GLUE节点组成，节点左树是前一个表达式，右树是下一个表达式
// A_GLUE节点有个大小字段，表示当前表达式数量
// 如果没有表达式解析到，返回NULL
// 解析时遇到endtoken，停止解析
struct ASTnode *expression_list(int endtoken) {
  struct ASTnode *tree = NULL;
  struct ASTnode *child = NULL;
  int exprcount = 0;

  // 循环直到遇到结束的token
  while (Token.token != endtoken) {

    // 解析下个表达式，同时增加表达式数量
    child = binexpr(0);
    exprcount++;

    // 创建一个A_GLUE节点，左树是之前的tree，右树是新的表达式child，同时保存表达式数量
    tree =
      mkastnode(A_GLUE, P_NONE, NULL, tree, NULL, child, NULL, exprcount);

    // 遇到结束token
    if (Token.token == endtoken)
      break;

    // 必须有逗号
    match(T_COMMA, ",");
  }

  // 返回所有表达式的树
  return (tree);
}

// 解析函数调用并返回语法树
static struct ASTnode *funccall(void) {
  struct ASTnode *tree;
  struct symtable *funcptr;

  // 判断函数是否已经定义了
  if ((funcptr = findsymbol(Text)) == NULL || funcptr->stype != S_FUNCTION) {
    fatals("Undeclared function", Text);
  }

  // 是否匹配'('
  lparen();

  // 解析参数的表达式列表
  tree = expression_list(T_RPAREN);

  // TODO: 要检查每个参数类型是否匹配函数原型

  // 创建函数调用语法树节点，保存函数返回类型作为节点类型
  // 同时记录函数符号号
  tree =
    mkastunary(A_FUNCCALL, funcptr->type, funcptr->ctype, tree, funcptr, 0);

  // 是否匹配')'
  rparen();
  return (tree);
}

// 解析通过索引访问数组，并返回语法树
static struct ASTnode *array_access(struct ASTnode *left) {
  struct ASTnode *right;

  // 确认子树是否为指针
  if (!ptrtype(left->type))
    fatal("Not an array or pointer");

  // 获得'['
  scan(&Token);

  // 解析接下来的表达式
  right = binexpr(0);

  // 匹配']'
  match(T_RBRACKET, "]");

  // 保证是整型
  if (!inttype(right->type))
    fatal("Array index is not of integer type");

  // 让左树为右值
  left->rvalue = 1;

  // 使索引按元素类型大小来增大
  right = modify_type(right, left->type, left->ctype, A_ADD);

  // 返回一个语法树节点，该节点是给这个数组的基地址加上位移
  // 然后再在这个节点基础上，再加个节点来解引用。现在这些节点还是左值
  left =
    mkastnode(A_ADD, left->type, left->ctype, left, NULL, right, NULL, 0);
  left =
    mkastunary(A_DEREF, value_at(left->type), left->ctype, left, NULL, 0);
  return (left);
}

// 解析一个struct/union的成员引用，并返回语法树
// 如果withpointer是true，那访问是通过一个指向成员的指针
static struct ASTnode *member_access(struct ASTnode *left, int withpointer) {
  struct ASTnode *right;
  struct symtable *typeptr;
  struct symtable *m;

  // 检查左树是不是个struct/union指针
  if (withpointer && left->type != pointer_to(P_STRUCT)
      && left->type != pointer_to(P_UNION))
    fatal("Expression is not a pointer to a struct/union");

  // 否则，检查左树是不是个struct/union
  // 如果是就改变节点类型，从A_IDENT到A_ADDR
  // 这样我们获得的是基地址，而不是地址里面的值
  if (!withpointer) {
    if (left->type == P_STRUCT || left->type == P_UNION)
      left->op = A_ADDR;
    else
      fatal("Expression is not a struct/union");
  }

  // 获取组合类型
  typeptr = left->ctype;

  // 跳过'.' 或者 '->'
  // 获取成员名字
  scan(&Token);
  ident();

  // 查找是否存在这个成员
  for (m = typeptr->member; m != NULL; m = m->next)
    if (!strcmp(m->name, Text))
      break;
  if (m == NULL)
    fatals("No member found in struct/union: ", Text);

  // 令左树为右值
  left->rvalue = 1;

  // 创建一个A_INTLIT节点，表示位移
  right = mkastleaf(A_INTLIT, P_INT, NULL, NULL, m->st_posn);

  // 返回一个语法树节点，该节点是给这个struct/union的基地址加上位移
  // 然后再在这个节点基础上，再加个节点来解引用。现在这些节点还是左值
  left =
    mkastnode(A_ADD, pointer_to(m->type), m->ctype, left, NULL, right, NULL,
	      0);
  left = mkastunary(A_DEREF, m->type, m->ctype, left, NULL, 0);
  return (left);
}

// 解析括号表达式，返回语法树
static struct ASTnode *paren_expression(int ptp) {
  struct ASTnode *n;
  int type = 0;
  struct symtable *ctype = NULL;

  // 跳过'('
  scan(&Token);

  // 如果后面跟的是一个类型标识符，有可能是一个cast表达式
  switch (Token.token) {
  case T_IDENT:
    // 如果一个标识符匹配一个typedef
    // 如果不是，那就是普通表达式
    if (findtypedef(Text) == NULL) {
      n = binexpr(0);	// ptp是0，当表达式在( )里
      break;
    }
  case T_VOID:
  case T_CHAR:
  case T_INT:
  case T_LONG:
  case T_STRUCT:
  case T_UNION:
  case T_ENUM:
    // 在括号里面获取类型
    type = parse_cast(&ctype);

    // 跳过')'，然后解析接下来的表达式
    rparen();

  default:
    n = binexpr(ptp);		// 扫描表达式，我们传入ptp，是因为cast不会改变表达式优先级

  }

  // n现在至少有一个表达式，
  // type如果是非0，那就代表有cast
  // type如果是0，代表没有cast，那就跳过')'
  if (type == 0)
    rparen();
  else
    // 否则，为cast创建一个单语法树节点
    n = mkastunary(A_CAST, type, ctype, n, NULL, 0);
  return (n);
}

// 解析主语，主语就类似操作数但不包括前缀和后缀，例如 ++a[i],a就是这个函数要处理的，++是prefix()处理，[i]是postfix()处理
// 返回语法树节点
static struct ASTnode *primary(int ptp) {
  struct ASTnode *n;
  struct symtable *enumptr;
  struct symtable *varptr;
  int id;
  int type = 0;
  int size, class;
  struct symtable *ctype;

  switch (Token.token) {
  case T_STATIC:
  case T_EXTERN:
    fatal("Compiler doesn't support static or extern local declarations");
  case T_SIZEOF:
    // 跳过T_SIZEOF和保证我们有个左括号
    scan(&Token);
    if (Token.token != T_LPAREN)
      fatal("Left parenthesis expected after sizeof");
    scan(&Token);

    // 获取括号中的类型
    type = parse_stars(parse_type(&ctype, &class));

    // 获取类型大小
    size = typesize(type, ctype);
    rparen();

    // 创建一个叶子节点，整型字面量，值为类型大小
    return (mkastleaf(A_INTLIT, P_INT, NULL, NULL, size));

  case T_INTLIT:
    // 对于INTLIT， 直接创建个叶子节点
    // 如果在字符的范围，那就令它为P_CHAR
    if (Token.intvalue >= 0 && Token.intvalue < 256)
      n = mkastleaf(A_INTLIT, P_CHAR, NULL, NULL, Token.intvalue);
    else
      n = mkastleaf(A_INTLIT, P_INT, NULL, NULL, Token.intvalue);
    break;

  case T_STRLIT:
    // 对于STRLIT，生成汇编给它
    id = genglobstr(Text, 0);

    // 对于后面还有STRLIT，直接加在后面
    while (1) {
      scan(&Peektoken);
      if (Peektoken.token != T_STRLIT) break;
      genglobstr(Text, 1);
      scan(&Token);	// 跳过，才能偷窥下一个
    }

    // 现在建一个叶子节点，id就是字符的标签号
    genglobstrend();
    n = mkastleaf(A_STRLIT, pointer_to(P_CHAR), NULL, NULL, id);
    break;

  case T_IDENT:
    // 如果标识符是个枚举值
    // 返回一个A_INTLIT节点
    if ((enumptr = findenumval(Text)) != NULL) {
      n = mkastleaf(A_INTLIT, P_INT, NULL, NULL, enumptr->st_posn);
      break;
    }

    // 检查标识符是否存在在符号表里面。如果是数组，设置rvalue为1
    if ((varptr = findsymbol(Text)) == NULL)
      fatals("Unknown variable or function", Text);
    switch (varptr->stype) {
    case S_VARIABLE:
      n = mkastleaf(A_IDENT, varptr->type, varptr->ctype, varptr, 0);
      break;
    case S_ARRAY:
      n = mkastleaf(A_ADDR, varptr->type, varptr->ctype, varptr, 0);
      n->rvalue = 1;
      break;
    case S_FUNCTION:
      // 函数调用，检查下个token是不是个左括号
      scan(&Token);
      if (Token.token != T_LPAREN)
	      fatals("Function name used without parentheses", Text);
      return (funccall());
    default:
      fatals("Identifier not a scalar or array variable", Text);
    }

    break;

  case T_LPAREN:
    return (paren_expression(ptp));

  default:
    fatals("Expecting a primary expression, got token", Token.tokstr);
  }

  // 扫描下一个token，返回叶子节点
  scan(&Token);
  return (n);
}

// 解析后缀表达式，并返回语法树节点
// 标识符已经在Text变量里面
static struct ASTnode *postfix(int ptp) {
  struct ASTnode *n;

  // 获取主语表达式
  n = primary(ptp);

  // 循环直到没有后缀操作符
  while (1) {
    switch (Token.token) {
    case T_LBRACKET:
      // 访问数组
      n = array_access(n);
      break;

    case T_DOT:
      // 访问struct/union
      n = member_access(n, 0);
      break;

    case T_ARROW:
      // 指针访问struct/union
      n = member_access(n, 1);
      break;

    case T_INC:
      // 后缀加加
      if (n->rvalue == 1)
	      fatal("Cannot ++ on rvalue");
      scan(&Token); // 跳过token

      // 不能做两次
      if (n->op == A_POSTINC || n->op == A_POSTDEC)
	      fatal("Cannot ++ and/or -- more than once");

      // 改变节点类型
      n->op = A_POSTINC;
      break;

    case T_DEC:
      // 后缀减减
      if (n->rvalue == 1)
	      fatal("Cannot -- on rvalue");
      scan(&Token); // 跳过token

      // 不能做两次
      if (n->op == A_POSTINC || n->op == A_POSTDEC)
	      fatal("Cannot ++ and/or -- more than once");

      // 改变节点类型
      n->op = A_POSTDEC;
      break;

    default:
      return (n);
    }
  }

  return (NULL);
}


// 转换一个二元操作到一个二叉语法树操作
// 由于我们已经是一一映射了，所以不需要switch case来写了
static int binastop(int tokentype) {
  if (tokentype > T_EOF && tokentype <= T_MOD)
    return (tokentype);
  fatals("Syntax error, token", Tstring[tokentype]);
  return (0);		
}

// 如果token是右关联，就返回true，否则false
// 例如a=b+1,=就是右关联，因为要先算=右边先
static int rightassoc(int tokentype) {
  if (tokentype >= T_ASSIGN && tokentype <= T_ASSLASH)
    return (1);
  return (0);
}

// 操作符优先级
// 顺序必须跟defs.h定义的一致
static int OpPrec[] = {
  0, 10, 10,			// T_EOF, T_ASSIGN, T_ASPLUS,
  10, 10,			// T_ASMINUS, T_ASSTAR,
  10, 10,			// T_ASSLASH, T_ASMOD,
  15,				// T_QUESTION,
  20, 30,			// T_LOGOR, T_LOGAND
  40, 50, 60,			// T_OR, T_XOR, T_AMPER 
  70, 70,			// T_EQ, T_NE
  80, 80, 80, 80,		// T_LT, T_GT, T_LE, T_GE
  90, 90,			// T_LSHIFT, T_RSHIFT
  100, 100,			// T_PLUS, T_MINUS
  110, 110, 110			// T_STAR, T_SLASH, T_MOD
};

// 检查是否为一个二元操作符，并返回它的优先级
static int op_precedence(int tokentype) {
  int prec;
  if (tokentype > T_MOD)
    fatals("Token with no precedence in op_precedence:", Tstring[tokentype]);
  prec = OpPrec[tokentype];
  if (prec == 0)
    fatals("Syntax error, token", Tstring[tokentype]);
  return (prec);
}

// prefix_expression: postfix_expression
//     | '*'  prefix_expression
//     | '&'  prefix_expression
//     | '-'  prefix_expression
//     | '++' prefix_expression
//     | '--' prefix_expression
//     ;

// 解析前缀表达式，并返回语法树节点
static struct ASTnode *prefix(int ptp) {
  struct ASTnode *tree;
  switch (Token.token) {
  case T_AMPER:
    // Get the next token and parse it
    // recursively as a prefix expression
    scan(&Token);
    tree = prefix(ptp);

    // Ensure that it's an identifier
    if (tree->op != A_IDENT)
      fatal("& operator must be followed by an identifier");

    // Prevent '&' being performed on an array
    if (tree->sym->stype == S_ARRAY)
      fatal("& operator cannot be performed on an array");

    // Now change the operator to A_ADDR and the type to
    // a pointer to the original type
    tree->op = A_ADDR;
    tree->type = pointer_to(tree->type);
    break;
  case T_STAR:
    // Get the next token and parse it
    // recursively as a prefix expression.
    // Make it an rvalue
    scan(&Token);
    tree = prefix(ptp);
    tree->rvalue= 1;

    // Ensure the tree's type is a pointer
    if (!ptrtype(tree->type))
      fatal("* operator must be followed by an expression of pointer type");

    // Prepend an A_DEREF operation to the tree
    tree =
      mkastunary(A_DEREF, value_at(tree->type), tree->ctype, tree, NULL, 0);
    break;
  case T_MINUS:
    // Get the next token and parse it
    // recursively as a prefix expression
    scan(&Token);
    tree = prefix(ptp);

    // Prepend a A_NEGATE operation to the tree and
    // make the child an rvalue. Because chars are unsigned,
    // also widen this if needed to int so that it's signed
    tree->rvalue = 1;
    if (tree->type == P_CHAR)
      tree->type = P_INT;
    tree = mkastunary(A_NEGATE, tree->type, tree->ctype, tree, NULL, 0);
    break;
  case T_INVERT:
    // Get the next token and parse it
    // recursively as a prefix expression
    scan(&Token);
    tree = prefix(ptp);

    // Prepend a A_INVERT operation to the tree and
    // make the child an rvalue.
    tree->rvalue = 1;
    tree = mkastunary(A_INVERT, tree->type, tree->ctype, tree, NULL, 0);
    break;
  case T_LOGNOT:
    // Get the next token and parse it
    // recursively as a prefix expression
    scan(&Token);
    tree = prefix(ptp);

    // Prepend a A_LOGNOT operation to the tree and
    // make the child an rvalue.
    tree->rvalue = 1;
    tree = mkastunary(A_LOGNOT, tree->type, tree->ctype, tree, NULL, 0);
    break;
  case T_INC:
    // Get the next token and parse it
    // recursively as a prefix expression
    scan(&Token);
    tree = prefix(ptp);

    // For now, ensure it's an identifier
    if (tree->op != A_IDENT)
      fatal("++ operator must be followed by an identifier");

    // Prepend an A_PREINC operation to the tree
    tree = mkastunary(A_PREINC, tree->type, tree->ctype, tree, NULL, 0);
    break;
  case T_DEC:
    // Get the next token and parse it
    // recursively as a prefix expression
    scan(&Token);
    tree = prefix(ptp);

    // For now, ensure it's an identifier
    if (tree->op != A_IDENT)
      fatal("-- operator must be followed by an identifier");

    // Prepend an A_PREDEC operation to the tree
    tree = mkastunary(A_PREDEC, tree->type, tree->ctype, tree, NULL, 0);
    break;
  default:
    tree = postfix(ptp);
  }
  return (tree);
}

// Return an AST tree whose root is a binary operator.
// Parameter ptp is the previous token's precedence.
struct ASTnode *binexpr(int ptp) {
  struct ASTnode *left, *right;
  struct ASTnode *ltemp, *rtemp;
  int ASTop;
  int tokentype;

  // Get the tree on the left.
  // Fetch the next token at the same time.
  left = prefix(ptp);

  // If we hit one of several terminating tokens, return just the left node
  tokentype = Token.token;
  if (tokentype == T_SEMI || tokentype == T_RPAREN ||
      tokentype == T_RBRACKET || tokentype == T_COMMA ||
      tokentype == T_COLON || tokentype == T_RBRACE) {
    left->rvalue = 1;
    return (left);
  }

  // While the precedence of this token is more than that of the
  // previous token precedence, or it's right associative and
  // equal to the previous token's precedence
  while ((op_precedence(tokentype) > ptp) ||
	 (rightassoc(tokentype) && op_precedence(tokentype) == ptp)) {
    // Fetch in the next integer literal
    scan(&Token);

    // Recursively call binexpr() with the
    // precedence of our token to build a sub-tree
    right = binexpr(OpPrec[tokentype]);

    // Determine the operation to be performed on the sub-trees
    ASTop = binastop(tokentype);

    switch (ASTop) {
    case A_TERNARY:
      // Ensure we have a ':' token, scan in the expression after it
      match(T_COLON, ":");
      ltemp = binexpr(0);

      // Build and return the AST for this statement. Use the middle
      // expression's type as the return type. XXX We should also
      // consider the third expression's type.
      return (mkastnode
	      (A_TERNARY, right->type, right->ctype, left, right, ltemp,
	       NULL, 0));

    case A_ASSIGN:
      // Assignment
      // Make the right tree into an rvalue
      right->rvalue = 1;

      // Ensure the right's type matches the left
      right = modify_type(right, left->type, left->ctype, 0);
      if (right == NULL)
	fatal("Incompatible expression in assignment");

      // Make an assignment AST tree. However, switch
      // left and right around, so that the right expression's 
      // code will be generated before the left expression
      ltemp = left;
      left = right;
      right = ltemp;
      break;

    default:
      // We are not doing a ternary or assignment, so both trees should
      // be rvalues. Convert both trees into rvalue if they are lvalue trees
      left->rvalue = 1;
      right->rvalue = 1;

      // Ensure the two types are compatible by trying
      // to modify each tree to match the other's type.
      ltemp = modify_type(left, right->type, right->ctype, ASTop);
      rtemp = modify_type(right, left->type, left->ctype, ASTop);
      if (ltemp == NULL && rtemp == NULL)
	fatal("Incompatible types in binary expression");
      if (ltemp != NULL)
	left = ltemp;
      if (rtemp != NULL)
	right = rtemp;
    }

    // Join that sub-tree with ours. Convert the token
    // into an AST operation at the same time.
    left =
      mkastnode(binastop(tokentype), left->type, left->ctype, left, NULL,
		right, NULL, 0);

    // Some operators produce an int result regardless of their operands
    switch (binastop(tokentype)) {
    case A_LOGOR:
    case A_LOGAND:
    case A_EQ:
    case A_NE:
    case A_LT:
    case A_GT:
    case A_LE:
    case A_GE:
      left->type = P_INT;
    }

    // Update the details of the current token.
    // If we hit a terminating token, return just the left node
    tokentype = Token.token;
    if (tokentype == T_SEMI || tokentype == T_RPAREN ||
	tokentype == T_RBRACKET || tokentype == T_COMMA ||
	tokentype == T_COLON || tokentype == T_RBRACE) {
      left->rvalue = 1;
      return (left);
    }
  }

  // Return the tree we have when the precedence
  // is the same or lower
  left->rvalue = 1;
  return (left);
}
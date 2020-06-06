#include "defs.h"
#include "data.h"
#include "decl.h"

// 解析语句
// Copyright (c) 2019 Warren Toomey, GPL3

// Prototypes
static struct ASTnode *single_statement(void);

// compound_statement:          // 空, 例如没有语句
//      |      statement
//      |      statement statements
//      ;
//
// statement: declaration
//      |     expression_statement
//      |     function_call
//      |     if_statement
//      |     while_statement
//      |     for_statement
//      |     return_statement
//      ;


// if_statement: if_head
//      |        if_head 'else' statement
//      ;
//
// if_head: 'if' '(' true_false_expression ')' statement  ;
//
// 解析IF语句，包括可选的ELSE语句
// 返回它的语法树
static struct ASTnode *if_statement(void) {
  struct ASTnode *condAST, *trueAST, *falseAST = NULL;

  // 确认有'if' '('
  match(T_IF, "if");
  lparen();

  // 解析接下来的表达式和')'
  // 强制把一个不可比较的操作转成可比较（A_TOBOOL）
  condAST = binexpr(0);
  if (condAST->op < A_EQ || condAST->op > A_GE)
    condAST =
      mkastunary(A_TOBOOL, condAST->type, condAST->ctype, condAST, NULL, 0);
  rparen();

  // 获得true时候的语句
  trueAST = single_statement();

  // 如果有'else'，跳过它，
  // 继续获得false的语句
  if (Token.token == T_ELSE) {
    scan(&Token);
    falseAST = single_statement();
  }

  // 创建一个A_IF的语法树，并返回
  return (mkastnode(A_IF, P_NONE, NULL, condAST, trueAST, falseAST, NULL, 0));
}


// while_statement: 'while' '(' true_false_expression ')' statement  ;
//
// 解析WHILE语句并返回它的语法树
static struct ASTnode *while_statement(void) {
  struct ASTnode *condAST, *bodyAST;

  // 保证有'while' '('
  match(T_WHILE, "while");
  lparen();

  // 解析接下来的表达式和')'
  // 强制把一个不可比较的操作转成可比较（A_TOBOOL）
  condAST = binexpr(0);
  if (condAST->op < A_EQ || condAST->op > A_GE)
    condAST =
      mkastunary(A_TOBOOL, condAST->type, condAST->ctype, condAST, NULL, 0);
  rparen();

  // 获得循环体语法树
  // 同时更新在这个过程中的循环深度
  Looplevel++;
  bodyAST = single_statement();
  Looplevel--;

  // 创建A_WHILE语法树并返回
  return (mkastnode(A_WHILE, P_NONE, NULL, condAST, NULL, bodyAST, NULL, 0));
}

// for_statement: 'for' '(' expression_list ';'
//                          true_false_expression ';'
//                          expression_list ')' statement  ;
//
// 解析FOR语句并返回它的语法树
static struct ASTnode *for_statement(void) {
  struct ASTnode *condAST, *bodyAST;
  struct ASTnode *preopAST, *postopAST;
  struct ASTnode *tree;

  // 保证有'for' '('
  match(T_FOR, "for");
  lparen();

  // 获取for循环第一个表达式，和后面的';'
  preopAST = expression_list(T_SEMI);
  semi();

  // 获得条件表达式和';'
  // 强制把一个不可比较的操作转成可比较（A_TOBOOL）
  condAST = binexpr(0);
  if (condAST->op < A_EQ || condAST->op > A_GE)
    condAST =
      mkastunary(A_TOBOOL, condAST->type, condAST->ctype, condAST, NULL, 0);
  semi();

  // 获得最后的表达式和')'
  postopAST = expression_list(T_RPAREN);
  rparen();

  // 获得循环体语法树
  // 同时更新在这个过程中的循环深度
  Looplevel++;
  bodyAST = single_statement();
  Looplevel--;

  // 把循环体语句和最后的表达式黏在一起
  tree = mkastnode(A_GLUE, P_NONE, NULL, bodyAST, NULL, postopAST, NULL, 0);

  // 然后跟条件表达式合成一个WHILE循环语句（这里就是强制把一个for循环变成一个while循环）
  tree = mkastnode(A_WHILE, P_NONE, NULL, condAST, NULL, tree, NULL, 0);

  // 把第一个表达式跟WHILE循环黏在一起
  return (mkastnode(A_GLUE, P_NONE, NULL, preopAST, NULL, tree, NULL, 0));
}

// return_statement: 'return' '(' expression ')'  ;
//
// 解析返回语句,并返回语法树
static struct ASTnode *return_statement(void) {
  struct ASTnode *tree= NULL;

  // 保证有个 'return'
  match(T_RETURN, "return");

  // 检查是否有返回值
  if (Token.token == T_LPAREN) {
    // 如果函数返回类型为P_VOID,报错
    if (Functionid->type == P_VOID)
      fatal("Can't return from a void function");

    // 跳过左括号
    lparen();

    // 解析接下来的表达式
    tree = binexpr(0);

    // 保证返回值跟函数返回类型是兼容的
    tree = modify_type(tree, Functionid->type, Functionid->ctype, 0);
    if (tree == NULL)
      fatal("Incompatible type to return");

    // 保证有个')'
    rparen();
  }

  // 包装为A_RETURN节点
  tree = mkastunary(A_RETURN, P_NONE, NULL, tree, NULL, 0);

  // 保证有';'
  semi();
  return (tree);
}

// break_statement: 'break' ;
//
// 解析break语句并返回它的语法树
static struct ASTnode *break_statement(void) {

  if (Looplevel == 0 && Switchlevel == 0)
    fatal("no loop or switch to break out from");
  scan(&Token);
  semi();
  return (mkastleaf(A_BREAK, P_NONE, NULL, NULL, 0));
}

// continue_statement: 'continue' ;
//
// 解析continue语句并返回它的语法树
static struct ASTnode *continue_statement(void) {

  if (Looplevel == 0)
    fatal("no loop to continue to");
  scan(&Token);
  semi();
  return (mkastleaf(A_CONTINUE, P_NONE, NULL, NULL, 0));
}

// 解析switch语句并返回它的语法树
static struct ASTnode *switch_statement(void) {
  struct ASTnode *left, *body, *n, *c;
  struct ASTnode *casetree = NULL, *casetail;
  int inloop = 1, casecount = 0;
  int seendefault = 0;
  int ASTop, casevalue;

  // 跳过'switch'和'('
  scan(&Token);
  lparen();

  // 获得switch表达式, ')' 和 '{'
  left = binexpr(0);
  rparen();
  lbrace();

  // 保证是个整型
  if (!inttype(left->type))
    fatal("Switch expression is not of integer type");

  // 创建一个A_SWITCH节点,用上面的表达式作为孩子
  n = mkastunary(A_SWITCH, P_NONE, NULL, left, NULL, 0);

  // 解析case
  Switchlevel++;
  while (inloop) {
    switch (Token.token) {
      // 当遇到'}'跳出循环
      case T_RBRACE:
        if (casecount == 0)
          fatal("No cases in switch");
        inloop = 0;
        break;
      case T_CASE:
      case T_DEFAULT:
        // 保证不是在'default'后面
        if (seendefault)
          fatal("case or default after existing default");

        // 设置语法树操作,如果需要,扫描case值
        if (Token.token == T_DEFAULT) {
          ASTop = A_DEFAULT;
          seendefault = 1;
          scan(&Token);
        } else {
          ASTop = A_CASE;
          scan(&Token);
          left = binexpr(0);

          // 保证case的值是整型字面量
          if (left->op != A_INTLIT)
            fatal("Expecting integer literal for case value");
          casevalue = left->atu.a_intvalue;

          // 检查有没有相同的case值
          for (c = casetree; c != NULL; c = c->right)
            if (casevalue == c->atu.a_intvalue)
              fatal("Duplicate case value");
        }

        // 匹配':' 和增加case数量
        match(T_COLON, ":");
        casecount++;

        // 如果下个token是个T_CASE,代表当前的case将没有body,直接进入下个case
        // 如果不是,解析body
        if (Token.token == T_CASE)
          body = NULL;
        else
          body = compound_statement(1);

        // 创建一个子树,body作为它的孩子,然后把这个子树链接到casetree的尾部
        if (casetree == NULL) {
          casetree = casetail =
            mkastunary(ASTop, P_NONE, NULL, body, NULL, casevalue);
        } else {
          casetail->right =
            mkastunary(ASTop, P_NONE, NULL, body, NULL, casevalue);
          casetail = casetail->right;
        }
        break;
      default:
        fatals("Unexpected token in switch", Token.tokstr);
    }
  }
  Switchlevel--;

  // 把casetree加入到A_SWITCH节点,同时也把case数设进去
  n->atu.a_intvalue = casecount;
  n->right = casetree;
  rbrace();

  return (n);
}

// 解析单一的语句并返回语法树
static struct ASTnode *single_statement(void) {
  struct ASTnode *stmt;
  struct symtable *ctype;
  int linenum= Line;

  switch (Token.token) {
    case T_SEMI:
      // 空语句
      semi();
      break;
    case T_LBRACE:
      // 有 '{', 所以这个是个组合语句
      lbrace();
      stmt = compound_statement(0);
      stmt->linenum= linenum;
      rbrace();
      return (stmt);
    case T_IDENT:
      // 如果标识符不是个typedef,当它为表达式处理
      // 如果是,那就由下面的情况来处理
      if (findtypedef(Text) == NULL) {
        stmt = binexpr(0);
        stmt->linenum= linenum;
        semi();
        return (stmt);
      }
    case T_CHAR:
    case T_INT:
    case T_LONG:
    case T_STRUCT:
    case T_UNION:
    case T_ENUM:
    case T_TYPEDEF:
      // 变量声明
      declaration_list(&ctype, C_LOCAL, T_SEMI, T_EOF, &stmt);
      semi();
      return (stmt);		// 变量声明也可能有赋值
    case T_IF:
      stmt= if_statement(); stmt->linenum= linenum; return(stmt);
    case T_WHILE:
      stmt= while_statement(); stmt->linenum= linenum; return(stmt);
    case T_FOR:
      stmt= for_statement(); stmt->linenum= linenum; return(stmt);
    case T_RETURN:
      stmt= return_statement(); stmt->linenum= linenum; return(stmt);
    case T_BREAK:
      stmt= break_statement(); stmt->linenum= linenum; return(stmt);
    case T_CONTINUE:
      stmt= continue_statement(); stmt->linenum= linenum; return(stmt);
    case T_SWITCH:
      stmt= switch_statement(); stmt->linenum= linenum; return(stmt);
    default:
      // 到这里,剩下的应该就是赋值的语句了
      stmt = binexpr(0);
      stmt->linenum= linenum;
      semi();
      return (stmt);
  }
  return (NULL);		
}

// 解析组合语句,并返回语法树
// 如果inswitch为true,那以'}', 'case' 或者 'default' 为结束token
// 否则就只是以'}'为结束token
struct ASTnode *compound_statement(int inswitch) {
  struct ASTnode *left = NULL;
  struct ASTnode *tree;

  while (1) {
    // 遇到结束token,
    // 先判断结束token,来允许空的组合语句
    if (Token.token == T_RBRACE)
      return (left);
    if (inswitch && (Token.token == T_CASE || Token.token == T_DEFAULT))
      return (left);

    // 解析单一语句
    tree = single_statement();

    // 对于每个新tree,如果left为空,就保存到left上
    // 否则跟left粘在一起
    if (tree != NULL) {
      if (left == NULL)
	      left = tree;
      else
	      left = mkastnode(A_GLUE, P_NONE, NULL, left, NULL, tree, NULL, 0);
    }
  }
  return (NULL);
}
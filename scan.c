#include "defs.h"
#include "data.h"
#include "decl.h"

// 词法分析
// Copyright (c) 2019 Warren Toomey, GPL3

// 返回字符位置，如果没有找到返回-1
static int chrpos(char *s, int c) {
  int i;
  for (i = 0; s[i] != '\0'; i++)
    if (s[i] == (char) c)
      return (i);
  return (-1);
}

// 从输入文件中获得下一个字符
static int next(void) {
  int c, l;

  if (Putback) {		// 如果putback有值，
    c = Putback;		// 就用putback的值
    Putback = 0;
    return (c);
  }

  c = fgetc(Infile);			// 从输入文件读入一个字符

  while (Linestart && c == '#') {	// 预处理开始
    Linestart = 0;			// 不在行首
    scan(&Token);			// 获取行号放到l
    if (Token.token != T_INTLIT)
      fatals("Expecting pre-processor line number, got:", Text);
    l = Token.intvalue;

    scan(&Token);			// 获得文件名放在Text
    if (Token.token != T_STRLIT)
      fatals("Expecting pre-processor file name, got:", Text);

    if (Text[0] != '<') {		// 如果这是真的文件名
      if (strcmp(Text, Infilename))	// 跟我们已有的不一样
	    Infilename = strdup(Text);	// 保存它，然后更新行号
      Line = l;
    }

    while ((c = fgetc(Infile)) != '\n'); // 调到下一行
    c = fgetc(Infile);			// 获取下一个字符
    Linestart = 1;			// 现在在行首了
  }

  Linestart = 0;			// 不在行首
  if ('\n' == c) {
    Line++;				// 增加行号
    Linestart = 1;			// 现在在行首了
  }
  return (c);
}

// 放回一个不需要的字符
static void putback(int c) {
  Putback = c;
}

// 跳过不需要处理的输入
// 例如，空格，换行等
// 返回我们需要处理的字符
static int skip(void) {
  int c;

  c = next();
  while (' ' == c || '\t' == c || '\n' == c || '\r' == c || '\f' == c) {
    c = next();
  }
  return (c);
}

// 从输入里面读入一个16进制常量
static int hexchar(void) {
  int c, h, n = 0, f = 0;

  // 循环获取字符
  while (isxdigit(c = next())) {
    // 转换字符为整型
    h = chrpos("0123456789abcdef", tolower(c));

    // Add to running hex value
    // 加到已有的16进制值
    n = n * 16 + h;
    f = 1;
  }

  // 碰到一个非16进制字符，放回
  putback(c);

  // 标志告诉我们，没有看到任何16进制字符
  if (!f)
    fatal("missing digits after '\\x'");
  if (n > 255)
    fatal("value out of range after '\\x'");

  return (n);
}

// 从一个字符或者字符字面量里面返回下一个字符
static int scanch(void) {
  int i, c, c2;

  // 获取下个字符，并解释那些有反斜杠开头的字符
  c = next();
  if (c == '\\') {
    switch (c = next()) {
      case 'a':
	    return ('\a');
      case 'b':
	    return ('\b');
      case 'f':
	    return ('\f');
      case 'n':
	    return ('\n');
      case 'r':
	    return ('\r');
      case 't':
	    return ('\t');
      case 'v':
	    return ('\v');
      case '\\':
	    return ('\\');
      case '"':
	    return ('"');
      case '\'':
	    return ('\'');

    // 处理8进制，直到碰到不是8进制数字为止
    // 值放在c2，位数放在i，只允许3位8进制数字
      case '0':
      case '1':
      case '2':
      case '3':
      case '4':
      case '5':
      case '6':
      case '7':
        for (i = c2 = 0; isdigit(c) && c < '8'; c = next()) {
            if (++i > 3)
                break;
            c2 = c2 * 8 + (c - '0');
        }

	    putback(c);		// 返回非8进制字符
	    return (c2);
      case 'x':
	    return (hexchar());
      default:
	    fatalc("unknown escape sequence", c);
    }
  }
  return (c);			// 返回普通的字符（没有反斜杠转义的）
}

// 从输入文件中扫描并返回整型字面量值
static int scanint(int c) {
  int k, val = 0, radix = 10;

  // 一般都是10进制，除非是0开头
  if (c == '0') {
    // 如果接下来是'x', 16进制
    if ((c = next()) == 'x') {
      radix = 16;
      c = next();
    } else
      // 否则8进制
      radix = 8;

  }

  // 转换字符到整型
  while ((k = chrpos("0123456789abcdef", tolower(c))) >= 0) {
    if (k >= radix)
      fatalc("invalid digit in integer literal", c);
    val = val * radix + k;
    c = next();
  }

  // 遇到非整型字符，放回
  putback(c);
  return (val);
}

// 从输入文件中扫描字符
// 并存储在buf[]
// 返回字符的长度
static int scanstr(char *buf) {
  int i, c;

  // 循环直到没有缓存
  for (i = 0; i < TEXTLEN - 1; i++) {
    // 获取下一个字符，并放入到buf， 直到遇到双引号
    if ((c = scanch()) == '"') {
      buf[i] = 0;
      return (i);
    }
    buf[i] = (char)c;
  }

  // 用完buf[]空间
  fatal("String literal too long");
  return (0);
}

// 从输入文件中扫描标识符
// 并存储在buf[]
// 返回标识符的长度
static int scanident(int c, char *buf, int lim) {
  int i = 0;

  // 允许数字，字母，下划线
  while (isalpha(c) || isdigit(c) || '_' == c) {
    // 如果超过标识符长度限制，就报错，
    // 否则放入到buf[],并获取下个字符
    if (lim - 1 == i) {
      fatal("Identifier too long");
    } else if (i < lim - 1) {
      buf[i++] = (char)c;
    }
    c = next();
  }

  // 遇到无效字符，放回
  // 0作为终结符放到buf[]中，并返回长度
  putback(c);
  buf[i] = '\0';
  return (i);
}

// 给定一个单词，如果是关键字返回token号，如果不是返回0
// 用首字母匹配，就不用浪费时间在strcmp()
static int keyword(char *s) {
  switch (*s) {
    case 'b':
      if (!strcmp(s, "break"))
	    return (T_BREAK);
      break;
    case 'c':
      if (!strcmp(s, "case"))
	    return (T_CASE);
      if (!strcmp(s, "char"))
	    return (T_CHAR);
      if (!strcmp(s, "continue"))
	    return (T_CONTINUE);
      break;
    case 'd':
      if (!strcmp(s, "default"))
	    return (T_DEFAULT);
      break;
    case 'e':
      if (!strcmp(s, "else"))
	    return (T_ELSE);
      if (!strcmp(s, "enum"))
	    return (T_ENUM);
      if (!strcmp(s, "extern"))
	    return (T_EXTERN);
      break;
    case 'f':
      if (!strcmp(s, "for"))
	    return (T_FOR);
      break;
    case 'i':
      if (!strcmp(s, "if"))
	    return (T_IF);
      if (!strcmp(s, "int"))
	    return (T_INT);
      break;
    case 'l':
      if (!strcmp(s, "long"))
	    return (T_LONG);
      break;
    case 'r':
      if (!strcmp(s, "return"))
	    return (T_RETURN);
      break;
    case 's':
      if (!strcmp(s, "sizeof"))
	    return (T_SIZEOF);
      if (!strcmp(s, "static"))
	    return (T_STATIC);
      if (!strcmp(s, "struct"))
	    return (T_STRUCT);
      if (!strcmp(s, "switch"))
	    return (T_SWITCH);
      break;
    case 't':
      if (!strcmp(s, "typedef"))
	    return (T_TYPEDEF);
      break;
    case 'u':
      if (!strcmp(s, "union"))
	    return (T_UNION);
      break;
    case 'v':
      if (!strcmp(s, "void"))
	    return (T_VOID);
      break;
    case 'w':
      if (!strcmp(s, "while"))
	    return (T_WHILE);
      break;
  }
  return (0);
}

// token字符串列表，为了调试方便
char *Tstring[] = {
  "EOF", "=", "+=", "-=", "*=", "/=", "%=",
  "?", "||", "&&", "|", "^", "&",
  "==", "!=", "<", ">", "<=", ">=", "<<", ">>",
  "+", "-", "*", "/", "%", "++", "--", "~", "!",
  "void", "char", "int", "long",
  "if", "else", "while", "for", "return",
  "struct", "union", "enum", "typedef",
  "extern", "break", "continue", "switch",
  "case", "default", "sizeof", "static",
  "intlit", "strlit", ";", "identifier",
  "{", "}", "(", ")", "[", "]", ",", ".",
  "->", ":"
};

// 扫描并返回下一个token
// 返回1代表token有效，否则代表没有token了
int scan(struct token *t) {
  int c, tokentype;

  // 如果有个向前看的token，返回这个token
  if (Peektoken.token != 0) {
    t->token = Peektoken.token;
    t->tokstr = Peektoken.tokstr;
    t->intvalue = Peektoken.intvalue;
    Peektoken.token = 0;
    return (1);
  }

  // 跳过空格
  c = skip();

  // 判断token是什么样的token
  switch (c) {
    case EOF:
      t->token = T_EOF;
      return (0);
    case '+':
      if ((c = next()) == '+') {
	    t->token = T_INC;
      } else if (c == '=') {
	    t->token = T_ASPLUS;
      } else {
        putback(c);
        t->token = T_PLUS;
      }
      break;
    case '-':
      if ((c = next()) == '-') {
	    t->token = T_DEC;
      } else if (c == '>') {
	    t->token = T_ARROW;
      } else if (c == '=') {
	    t->token = T_ASMINUS;
      } else if (isdigit(c)) {	// 负数
	    t->intvalue = -scanint(c);
	    t->token = T_INTLIT;
      } else {
        putback(c);
        t->token = T_MINUS;
      }
      break;
    case '*':
      if ((c = next()) == '=') {
	    t->token = T_ASSTAR;
      } else {
        putback(c);
        t->token = T_STAR;
      }
      break;
    case '/':
      if ((c = next()) == '=') {
	    t->token = T_ASSLASH;
      } else {
        putback(c);
        t->token = T_SLASH;
      }
      break;
    case '%':
      if ((c = next()) == '=') {
	    t->token = T_ASMOD;
      } else {
        putback(c);
        t->token = T_MOD;
      }
      break;
    case ';':
      t->token = T_SEMI;
      break;
    case '{':
      t->token = T_LBRACE;
      break;
    case '}':
      t->token = T_RBRACE;
      break;
    case '(':
      t->token = T_LPAREN;
      break;
    case ')':
      t->token = T_RPAREN;
      break;
    case '[':
      t->token = T_LBRACKET;
      break;
    case ']':
      t->token = T_RBRACKET;
      break;
    case '~':
      t->token = T_INVERT;
      break;
    case '^':
      t->token = T_XOR;
      break;
    case ',':
      t->token = T_COMMA;
      break;
    case '.':
      t->token = T_DOT;
      break;
    case ':':
      t->token = T_COLON;
      break;
    case '?':
      t->token = T_QUESTION;
      break;
    case '=':
      if ((c = next()) == '=') {
	    t->token = T_EQ;
      } else {
        putback(c);
        t->token = T_ASSIGN;
      }
      break;
    case '!':
      if ((c = next()) == '=') {
	    t->token = T_NE;
      } else {
        putback(c);
        t->token = T_LOGNOT;
      }
      break;
    case '<':
      if ((c = next()) == '=') {
	    t->token = T_LE;
      } else if (c == '<') {
	    t->token = T_LSHIFT;
      } else {
        putback(c);
        t->token = T_LT;
      }
      break;
    case '>':
      if ((c = next()) == '=') {
	    t->token = T_GE;
      } else if (c == '>') {
	    t->token = T_RSHIFT;
      } else {
        putback(c);
        t->token = T_GT;
      }
      break;
    case '&':
      if ((c = next()) == '&') {
	    t->token = T_LOGAND;
      } else {
        putback(c);
        t->token = T_AMPER;
      }
      break;
    case '|':
      if ((c = next()) == '|') {
	    t->token = T_LOGOR;
      } else {
        putback(c);
        t->token = T_OR;
      }
      break;
    case '\'':
      // 如果是单引号，那就是要扫描字符了
      t->intvalue = scanch();
      t->token = T_INTLIT;
      if (next() != '\'')
	    fatal("Expected '\\'' at end of char literal");
      break;
    case '"':
      // 扫描字符串
      scanstr(Text);
      t->token = T_STRLIT;
      break;
    default:
      // 如果是数字，就扫描为字面量整型
      if (isdigit(c)) {
        t->intvalue = scanint(c);
        t->token = T_INTLIT;
        break;
      } else if (isalpha(c) || '_' == c) {
        // 读入关键字或标识符
        scanident(c, Text, TEXTLEN);

        // 如果是关键字，返回关键字的token
        if ((tokentype = keyword(Text)) != 0) {
            t->token = tokentype;
            break;
        }

        // 如果不是关键字，那就肯定是标识符
        t->token = T_IDENT;
        break;
      }

      // 到这里，就代表不是认识的token了，报错
      fatalc("Unrecognised character", c);
  }

  // 找到token啦
  t->tokstr = Tstring[t->token];
  return (1);
}
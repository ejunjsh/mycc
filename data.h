#ifndef extern_
#define extern_ extern
#endif

// 全局变量
// Copyright (c) 2019 Warren Toomey, GPL3

extern_ int Line;		     	// 当前行号
extern_ int Linestart;		     	// 如果在行开始就返回true
extern_ int Putback;		     	// 由scanner放回的字符
extern_ struct symtable *Functionid; 	// 当前函数的符号表指针
extern_ FILE *Infile;		     	// 输入输出文件
extern_ FILE *Outfile;
extern_ char *Infilename;		// 正在解析的文件名
extern_ char *Outfilename;		// 打开的输出文件的文件名
extern_ struct token Token;		// 上一个扫描到的token
extern_ struct token Peektoken;		// 向前看一个token
extern_ char Text[TEXTLEN + 1];		// 上一个扫描到的标识符
extern_ int Looplevel;			// 循环嵌套深度
extern_ int Switchlevel;		// switche嵌套深度
extern char *Tstring[];			// 字符串token列表

// 符号表列表
extern_ struct symtable *Globhead, *Globtail;	  // 全局变量和函数
extern_ struct symtable *Loclhead, *Locltail;	  // 本地变量
extern_ struct symtable *Parmhead, *Parmtail;	  // 本地参数
extern_ struct symtable *Membhead, *Membtail;	  // 临时的struct/union的成员列表
extern_ struct symtable *Structhead, *Structtail; // struct类型列表
extern_ struct symtable *Unionhead, *Uniontail;   // union类型列表
extern_ struct symtable *Enumhead,  *Enumtail;    // enum类型和值列表
extern_ struct symtable *Typehead,  *Typetail;    // typedef列表

// 命令行参数
extern_ int O_dumpAST;		// 如果true, 打印语法树
extern_ int O_dumpsym;		// 如果true, 打印符号表
extern_ int O_keepasm;		// 如果true, 保留汇编文件
extern_ int O_assemble;		// 如果true, 编译汇编文件
extern_ int O_dolink;		// 如果true, 连接目标文件
extern_ int O_verbose;		// 如果true, 在编译阶段打印信息
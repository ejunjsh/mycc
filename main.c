#include "defs.h"
#define extern_
#include "data.h"
#undef extern_
#include "decl.h"
#include <errno.h>
#include <unistd.h>

// 编译器设置和顶层执行
// Copyright (c) 2019 Warren Toomey, GPL3

// 给定一个字符串，和一个只有一个字符的后缀，改变给定字符串的后缀
// 返回新的字符串，如果原来字符串没有改动，返回NULL
char *alter_suffix(char *str, char suffix) {
  char *posn;
  char *newstr;

  // 拷贝字符串
  if ((newstr = strdup(str)) == NULL)
    return (NULL);

  // 找到'.'的位置
  if ((posn = strrchr(newstr, '.')) == NULL)
    return (NULL);

  // 保证这里有个后缀
  posn++;
  if (*posn == '\0')
    return (NULL);

  // 改变后缀，并用NULL终结
  *posn = suffix;
  posn++;
  *posn = '\0';
  return (newstr);
}

// 给定一个输入文件名，编译这个文件到汇编，返回新文件名
static char *do_compile(char *filename) {
  char cmd[TEXTLEN];

  // 改变输入文件后缀为.s
  Outfilename = alter_suffix(filename, 's');
  if (Outfilename == NULL) {
    fprintf(stderr, "Error: %s has no suffix, try .c on the end\n", filename);
    exit(1);
  }

  // 生成预处理器命令
  snprintf(cmd, TEXTLEN, "%s %s %s", CPPCMD, INCDIR, filename);

  // 打开预处理器管道
  if ((Infile = popen(cmd, "r")) == NULL) {
    fprintf(stderr, "Unable to open %s: %s\n", filename, strerror(errno));
    exit(1);
  }
  Infilename = filename;

  // 创建输出文件
  if ((Outfile = fopen(Outfilename, "w")) == NULL) {
    fprintf(stderr, "Unable to create %s: %s\n", Outfilename,
	    strerror(errno));
    exit(1);
  }

  Line = 1;			// 重置扫描器
  Linestart = 1;
  Putback = '\n';
  clear_symtable();		// 清空符号表
  if (O_verbose)
    printf("compiling %s\n", filename);
  scan(&Token);			//从输入中获取第一个token
  Peektoken.token = 0;		// 设置没有向前看token
  genpreamble(filename);	// 输出前置汇编
  global_declarations();	// 解析开始,解析全局声明
  genpostamble();		// 输出后置汇编
  fclose(Outfile);		// 关闭输出文件

  // 如果需要,打印符号表
  if (O_dumpsym) {
    printf("Symbols for %s\n", filename);
    dumpsymtables();
    fprintf(stdout, "\n\n");
  }

  freestaticsyms();		// 清理静态符号表
  return (Outfilename);
}

// 给定输入文件名,汇编这个文件到一个目标代码
// 返回目标文件名
char *do_assemble(char *filename) {
  char cmd[TEXTLEN];
  int err;

  char *outfilename = alter_suffix(filename, 'o');
  if (outfilename == NULL) {
    fprintf(stderr, "Error: %s has no suffix, try .s on the end\n", filename);
    exit(1);
  }

  // 创建汇编命令,然后运行它
  snprintf(cmd, TEXTLEN, "%s %s %s", ASCMD, outfilename, filename);
  if (O_verbose)
    printf("%s\n", cmd);
  err = system(cmd);
  if (err != 0) {
    fprintf(stderr, "Assembly of %s failed\n", filename);
    exit(1);
  }
  return (outfilename);
}

// 给定一堆目标文件和输出文件名
// 链接所有目标文件
void do_link(char *outfilename, char **objlist) {
  int cnt, size = TEXTLEN;
  char cmd[TEXTLEN], *cptr;
  int err;

  // 先设置链接器命令和输出文件
  cptr = cmd;
  cnt = snprintf(cptr, size, "%s %s ", LDCMD, outfilename);
  cptr += cnt;
  size -= cnt;

  // 然后添加每个目标文件
  while (*objlist != NULL) {
    cnt = snprintf(cptr, size, "%s ", *objlist);
    cptr += cnt;
    size -= cnt;
    objlist++;
  }

  if (O_verbose)
    printf("%s\n", cmd);
  err = system(cmd);
  if (err != 0) {
    fprintf(stderr, "Linking failed\n");
    exit(1);
  }
}

// 打印命令行帮助
static void usage(char *prog) {
  fprintf(stderr, "Usage: %s [-vcSTM] [-o outfile] file [file ...]\n", prog);
  fprintf(stderr,
	  "       -v give verbose output of the compilation stages\n");
  fprintf(stderr, "       -c generate object files but don't link them\n");
  fprintf(stderr, "       -S generate assembly files but don't link them\n");
  fprintf(stderr, "       -T dump the AST trees for each input file\n");
  fprintf(stderr, "       -M dump the symbol table for each input file\n");
  fprintf(stderr, "       -o outfile, produce the outfile executable file\n");
  exit(1);
}

// 主函数: 检查参数,如果没有参数,打印帮助
// 打开多个输入文件,并开始编译
enum { MAXOBJ = 100 };
int main(int argc, char **argv) {
  char *outfilename = AOUT;
  char *asmfile, *objfile;
  char *objlist[MAXOBJ];
  int i, j, objcnt = 0;

  // 初始化变量
  O_dumpAST = 0;
  O_dumpsym = 0;
  O_keepasm = 0;
  O_assemble = 0;
  O_verbose = 0;
  O_dolink = 1;

  // 扫描命令行参数
  for (i = 1; i < argc; i++) {
    // 不是'-'开头,停止扫描
    if (*argv[i] != '-')
      break;

    // 根据不同的参数设置不同的变量
    for (j = 1; (*argv[i] == '-') && argv[i][j]; j++) {
      switch (argv[i][j]) {
      case 'o':
        outfilename = argv[++i];	// 直接获取下一个参数
        break;
      case 'T':
        O_dumpAST = 1;
        break;
      case 'M':
        O_dumpsym = 1;
        break;
      case 'c':
        O_assemble = 1;
        O_keepasm = 0;
        O_dolink = 0;
        break;
      case 'S':
        O_keepasm = 1;
        O_assemble = 0;
        O_dolink = 0;
        break;
      case 'v':
        O_verbose = 1;
        break;
      default:
        usage(argv[0]);
      }
    }
  }

  // 保证至少由一个输入文件参数
  if (i >= argc)
    usage(argv[0]);

  // 循环处理每个输入文件
  while (i < argc) {
    asmfile = do_compile(argv[i]);	// 编译源文件

    if (O_dolink || O_assemble) {
      objfile = do_assemble(asmfile);	// 汇编成目标文件
      if (objcnt == (MAXOBJ - 2)) {
        fprintf(stderr, "Too many object files for the compiler to handle\n");
        exit(1);
      }
      objlist[objcnt++] = objfile;	//  把目标文件名加入到目标文件列表
      objlist[objcnt] = NULL;	
    }

    if (!O_keepasm)		// 如果不需要汇编后的文件,删之
      unlink(asmfile);		
    i++;
  }

  // 链接所有目标文件
  if (O_dolink) {
    do_link(outfilename, objlist);

    // 如果不需要目标文件,删之
    if (!O_assemble) {
      for (i = 0; objlist[i] != NULL; i++)
        unlink(objlist[i]);
    }
  }

  return (0);
}
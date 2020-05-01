#include "defs.h"
#include "data.h"
#include "decl.h"
#include <stdio.h>
#include <unistd.h>

//杂项函数
// Copyright (c) 2019 Warren Toomey, GPL3

//保证当前token是t，和获取下个token，否则就报错
void match(int t, char *what){
    if(Token.token == t){
        scan(&Token);
    } else{
        fatals("Expected", what);
    }
}

//匹配;和获取下个token
void semi(void){
    match(T_SEMI,";");
}

//匹配{和获取下个token
void lbrace(void){
    match(T_LBRACE, "{");
}

//匹配}和获取下个token
void rbrace(void) {
  match(T_RBRACE, "}");
}

//匹配(和获取下个token
void lparen(void) {
  match(T_LPAREN, "(");
}

//匹配)和获取下个token
void rparen(void) {
  match(T_RPAREN, ")");
}

//匹配标识符和获取下个token
void ident(void) {
  match(T_IDENT, "identifier");
}

//匹配,和获取下个token
void comma(void) {
  match(T_COMMA, "comma");
}

//输出fatal信息
void fatal(char *s) {
  fprintf(stderr, "%s on line %d of %s\n", s, Line, Infilename);
  fclose(Outfile);
  unlink(Outfilename);
  exit(1);
}

void fatals(char *s1, char *s2) {
  fprintf(stderr, "%s:%s on line %d of %s\n", s1, s2, Line, Infilename);
  fclose(Outfile);
  unlink(Outfilename);
  exit(1);
}

void fatald(char *s, int d) {
  fprintf(stderr, "%s:%d on line %d of %s\n", s, d, Line, Infilename);
  fclose(Outfile);
  unlink(Outfilename);
  exit(1);
}

void fatalc(char *s, int c) {
  fprintf(stderr, "%s:%c on line %d of %s\n", s, c, Line, Infilename);
  fclose(Outfile);
  unlink(Outfilename);
  exit(1);
}
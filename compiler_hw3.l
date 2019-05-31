/* Definition section */
%{
	#include <stdio.h>
	#include <stdlib.h>
        #include "y.tab.h"
        #define BUF_SIZE 100

        extern YYSTYPE yylval; //YYSTYPE
        extern void yyerror(char*);
        extern int print_table_flag;
        extern int print_error_flag;
        extern int g_scope;
        extern void dump_symbol(int);

        char buf[BUF_SIZE];
        char error_buf[BUF_SIZE];

        int line_num = 0;
        int comment_num = 0;
        #define CONCAT { strncat(buf, yytext, BUF_SIZE - strlen(buf) - 1);}
        static void comment(void);
        void print_table();
%}

/* Define regular expression label */

letter [a-zA-Z]
digit [0-9]

ws [\t\n]+
id {letter}+({letter}|{digit})*
number ([1-9]{digit}+)|{digit}+



F_CONST {number}"."{digit}+
b ([\40-\41\43-\133\135-\170])
any ([\40-\170])
STR_CONST [\42]({b}|[\134]{any})+[\42]

%option yylineno

%x COMMENT STRING_STATE

/* Rules section */
%%

 /* Arithmetic */

"+" 	    { CONCAT return ADD; }
"-"	    { CONCAT return SUB; }
"*"         { CONCAT return MUL; }
"/"         { CONCAT return DIV; }
"%"         { CONCAT return MOD; }
"++"        { CONCAT return INC; }
"--"        { CONCAT return DEC; }

 /* Relational */
">"         { CONCAT return MT; }
"<"         { CONCAT return LT; }
">="        { CONCAT return MTE; }
"<="        { CONCAT return LTE; }
"=="        { CONCAT return EQ; }
"!="        { CONCAT return NE; }

 /* Assignment */
"="         { CONCAT return ASGN; }
"+="        { CONCAT return ADDASGN; }
"-="        { CONCAT return SUBASGN; }
"*="        { CONCAT return MULASGN; }
"/="        { CONCAT return DIVASGN; }
"%="        { CONCAT return MODASGN; }

 /* Logical */
"&&"        { CONCAT return AND; }
"||"        { CONCAT return OR; }
"!"         { CONCAT return NOT; }

 /* Delimiters */
"("         { CONCAT return LB; }
")"         { CONCAT return RB; }
"{"         { CONCAT return LCB; }
"}"         { CONCAT return RCB; }
"["         { CONCAT return LSB; }
"]"         { CONCAT return RSB; }
","         { CONCAT return COMMA; }

 /* Print Keywords */
"print"     { CONCAT return PRINT; }

 /* Condition and Loop Keywords */
"if"        { CONCAT return IF; }
"else"      { CONCAT return ELSE; }
"for"       { CONCAT return FOR; }
"while"     { CONCAT return WHILE; }

 /* Declaration Keywords */
"void"      { CONCAT return VOID; }
"int"       { CONCAT return INT; }
"float"     { CONCAT return FLOAT; }
"string"    { CONCAT return STRING; }
"bool"      { CONCAT return BOOL; }

 /* boolean Keywords */
"true"      { CONCAT return TRUE; }
"false"     { CONCAT return FALSE; }

"return"    { CONCAT return RET; }

 /*";"         { CONCAT return SEMICOLON; }*/
 /*"\""        { CONCAT return QUOTA; }*/

 /*"continue"  { CONCAT return CONT; }*/
 /*"break"     { CONCAT return BREAK; }*/

 /* String Constant */
\"                  { BEGIN STRING_STATE; CONCAT return QUOTA; }
<STRING_STATE>\"    { BEGIN INITIAL; CONCAT return QUOTA; }
<STRING_STATE>[^"]* { yylval.s_val = strdup(yytext); return STR_CONST; }

{number}    { yylval.i_val = atoi(yytext); CONCAT return I_CONST; }
{F_CONST}   { yylval.f_val = atof(yytext); CONCAT return F_CONST; }
 /*{STR_CONST} { yylval.s_val = yytext; CONCAT return STR_CONST;}*/


 /* C++ type Comment */
\/\/.*      {   } 

 /* Variable ID */
{id}	    { /*yylval.s_val = (char*)malloc(sizeof(yytext));
                strcpy(yylval.s_val,yytext);*/
                yylval.s_val = strdup(yytext);
                CONCAT return ID; }

 /* others */
[;]         { return SEMICOLON; }
[\n]	    { //line_num++; 
                /*if(strlen(buf) == 0){
                    printf("%d:\n", yylineno);
                } else {
                    printf("%d: %s\n", yylineno, buf); 
                }*/
                print_table();
                memset(buf, 0, sizeof(buf));
                }

[ \t]          { CONCAT }
[^ \t\n]          { CONCAT }
<<EOF>>     { /*dump_symbol(0);*/
                /*static int once = 0; return once++ ? 0: SEMICOLON;*/
                return 0;}

%%

/*	C Code section */
int yywrap(void)
{
    return 1;
}

static void comment(void)
{
    int c;

    while ((c = input()) != 0){
        strcat(buf, (const char *)&c);
        
        if (c == '*') {
            while ((c = input()) == '*')
                strcat(buf, (const char *)&c);

            strcat(buf, (const char * )&c);
            if (c == '/') { 
                return;
            }
            if (c == 0)
                break;
        }
        
        if (c == '\n'){
            if(strlen(buf) == 0){
                printf("%d:\n", yylineno);
            } else {
                printf("%d: %s", yylineno, buf); 
            }
            memset(buf, 0, sizeof(buf));
        }
    }
    yyerror("unterminated comment");
}

void print_table(){
    if(print_table_flag != 0){
        dump_symbol(print_table_flag);
        print_table_flag = 0;
    }
    if(print_error_flag != 0){
        yyerror(error_buf);
        memset(error_buf, 0, BUF_SIZE);
        print_error_flag = 0;
    }
}


/*	Definition section */
%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#define BUF_SIZE 100
extern int yylineno;
extern int yylex();
extern void yyerror(char*);
extern char* yytext;   // Get current token from lex
extern char buf[BUF_SIZE];  // Get current code line from lex
extern char error_buf[BUF_SIZE]; // Pass error message to lex
char j_buf[65536]; // Store jasmin code

FILE *file; //To generate .j file for Jasmin

struct symbol{
    int index;
    char *name;
    int kind; //0: function, 1: parameter, 2: variable 
    int type; //0: int, 1: float, 2: bool, 3: string, 4: void
    int parameter; // parameter number
    struct symbol *next;
};

struct symbol_table{
    struct symbol *child;
    struct symbol_table *next;
    int scope;
};

struct symbol_table *head;
/* Symbol table function - you can add new function if needed. */
int lookup_symbol(char* name, int declare_or_use, int define_function_or_not);
struct symbol_table *create_symbol(int scope);
struct symbol * insert_symbol(char *name, int kind, int type, int pa, int scope);
void dump_symbol(int scope);
void free_table(struct symbol_table *);
int intlen(int a);
struct symbol * find_symbol(char *name, int scope);

void ge_field(char *name, int type, int value_type, float value);
void ge_method(char *name, int type, int return_type);
struct symbol * load_var(char *name);
struct symbol * store_var(char *name, int a_type);
char type_i2c(int type);
void insert_str2j_buf(char *str, int pos);
char *get_j_buf(int start, int end);

int g_scope; //for parsing, calculate scope
int print_table_flag = 0;
int print_error_flag = 0;
int left_operand_flag = 0; // to store how many rule has record in j_buf
int right_operand_flag = 0; // 'F' or 'I'
int label_num = 0;

char * KIND[3] = {"function", "parameter", "variable"};
char * TYPE[6] = {"null", "int", "float", "bool", "string", "void"};


%}

/* Use variable or self-defined structure to represent
 * nonterminal and token type
 */

%union {
    int i_val;
    double f_val;
    char* s_val;
    /*_Bool boolean;*/
    struct{
        int type;
        int pos; // store the position of rule in j_buf
        int reg; // store what the register use now
    }t;
}

/* Token without return */
%token PRINT 
%token IF ELSE FOR WHILE
%token SEMICOLON
%token ADD SUB MUL DIV MOD INC DEC 
%token MT LT MTE LTE EQ NE 
%token AND OR NOT
%token BREAK CONT RET 
%token LSB RSB COMMA QUOTA LB RB LCB RCB
%token ASGN ADDASGN SUBASGN MULASGN DIVASGN MODASGN

/* Token with return, which need to sepcify type */
%token <i_val> I_CONST
%token <f_val> F_CONST
%token <s_val> STR_CONST
%token <i_val> TRUE FALSE

%token <s_val> INT FLOAT VOID STRING BOOL
%token <s_val> ID

/* Nonterminal with return, which need to sepcify type */
%type <i_val> type
%type <i_val> parameter_list
%type <i_val> while_lb
%type <t> assignment_expression expression
%type <t> constant
%type <t> logical_or_expression logical_and_expression
%type <t> equality_expression relational_expression 
%type <t> additive_expression multiplicative_expression
%type <t> cast_expression unary_expression
%type <t> postfix_expression primary_expression 
%type <t> function_expression 
%type <t> block_item_list 
%type <t> compound_stat
%type <t> declaration_list declaration
%type <t> selection_stat iteration_stat jump_stat expression_stat
%type <t> print_func
%type <t> stat

/* Yacc will start at this nonterminal */
%start program

/* Grammar section */
%%

program
    /*: program stat*/
    : external_declaration 
    | program external_declaration 
;

stat
    : declaration_list SEMICOLON 
    | compound_stat 
    | selection_stat
    | iteration_stat
    | jump_stat
    | expression_stat
    | print_func
;

ex_declaration
    : type ID ASGN I_CONST
        {
            if(lookup_symbol($2,'d', 0) == 0){
                insert_symbol($2, 2, $1, -1, g_scope);
            } else {
                // semantic error
                strcat(error_buf, "Redeclared variable ");
                strcat(error_buf, $2);
                print_error_flag = 1;
            }
            ge_field($2,$1,1,$4);
        }
    | type ID ASGN F_CONST
        {
            if(lookup_symbol($2,'d', 0) == 0){
                insert_symbol($2, 2, $1, -1, g_scope);
            } else {
                // semantic error
                strcat(error_buf, "Redeclared variable ");
                strcat(error_buf, $2);
                print_error_flag = 1;
            }
            ge_field($2,$1,2,$4);
        }
    | type ID ASGN QUOTA STR_CONST QUOTA
        {
            if(lookup_symbol($2,'d', 0) == 0){
                insert_symbol($2, 2, $1, -1, g_scope);
            } else {
                // semantic error
                strcat(error_buf, "Redeclared variable ");
                strcat(error_buf, $2);
                print_error_flag = 1;
            }
            // $5 include " , so lost a " to fix
            fprintf(file, ".field public static %s Ljava/lang/String; = \"%s\n", $2, $5);
        }
    | type ID ASGN TRUE
        {
            if(lookup_symbol($2,'d', 0) == 0){
                insert_symbol($2, 2, $1, -1, g_scope);
            } else {
                // semantic error
                strcat(error_buf, "Redeclared variable ");
                strcat(error_buf, $2);
                print_error_flag = 1;
            }
            ge_field($2,$1,1,1);
        }
    | type ID ASGN FALSE
        {
            if(lookup_symbol($2,'d', 0) == 0){
                insert_symbol($2, 2, $1, -1, g_scope);
            } else {
                // semantic error
                strcat(error_buf, "Redeclared variable ");
                strcat(error_buf, $2);
                print_error_flag = 1;
            }
            ge_field($2,$1,1,0);
        }
    | type ID
        { 
            if(lookup_symbol($2, 'd',0) == 0){
                insert_symbol($2, 2, $1, -1, g_scope);
            } else {
                // semantic error
                strcat(error_buf, "Redeclared variable ");
                strcat(error_buf, $2);
                print_error_flag = 1;
            }
            ge_field($2,$1,-1,0);
        }
    ;

declaration
    : type ID ASGN assignment_expression
        {
            struct symbol * now;
            if(lookup_symbol($2,'d', 0) == 0){
                now = insert_symbol($2, 2, $1, -1, g_scope);
                store_var($2,$<t.type>4);
                /*if(now->type == 1){*/
                    /*if($<t.type>4 == 'F'){*/
                        /*sprintf(j_buf, "%s\tf2i\n", j_buf);*/
                    /*}*/
                    /*sprintf(j_buf, "%s\tistore %d\n", j_buf, now->index);*/
                /*} else if (now->type == 2){*/
                    /*if($<t.type>4 == 'I'){*/
                        /*sprintf(j_buf, "%s\ti2f\n", j_buf);*/
                    /*}*/
                    /*sprintf(j_buf, "%s\tfstore %d\n", j_buf, now->index);*/
                /*}*/
            } else {
                // semantic error
                strcat(error_buf, "Redeclared variable ");
                strcat(error_buf, $2);
                print_error_flag = 1;
            }

        }

    | type ID 
        { 
            struct symbol *now;
            if(lookup_symbol($2, 'd',0) == 0){
                now = insert_symbol($2, 2, $1, -1, g_scope);
            } else {
                // semantic error
                strcat(error_buf, "Redeclared variable ");
                strcat(error_buf, $2);
                print_error_flag = 1;
            }
            sprintf(j_buf, "%s\tldc 0\n\tistore %d\n",j_buf, now->index);
        }
;


/* actions can be taken when meet the token or rule */
type
    : INT { $$ = 1; }
    | FLOAT { $$ = 2; }
    | BOOL  { $$ = 3; }
    | STRING { $$ = 4; }
    | VOID { $$ = 5; }
;


primary_expression
	: ID {
            if(lookup_symbol($1,'u',0) == 0){ //Undeclare
                // semantic error
                strcat(error_buf, "Undeclared variable ");
                strcat(error_buf, $1);
                print_error_flag = 1;
            } else {
                struct symbol * now = load_var($1);
                $<t.type>$ = type_i2c(now -> type);
                $<t.pos>$ = strlen(j_buf);
                $<t.reg>$ = now -> index;
            }
        }
	| constant 
            {$$ = $1;}
	| QUOTA STR_CONST QUOTA
            { 
                sprintf(j_buf, "%s\tldc \"%s\n", j_buf, $2);
                $<t.type>$ = 'S';
                $<t.pos>$ = strlen(j_buf);
            }
        | LB expression RB { $$ = $2;}
        | TRUE
            {
                sprintf(j_buf, "%s\tldc 1\n", j_buf);
                $<t.type>$ = 'Z';
                $<t.pos>$ = strlen(j_buf);
            }
        | FALSE
            {
                sprintf(j_buf, "%s\tldc 0\n", j_buf);
                $<t.type>$ = 'Z';
                $<t.pos>$ = strlen(j_buf);
            }
	;

constant
	: I_CONST 
            {
                sprintf(j_buf, "%s\tldc %d\n", j_buf,yylval.i_val);
                $<t.type>$ = 'I';
                $<t.pos>$ = strlen(j_buf);
            }
	| F_CONST 
            {
                sprintf(j_buf, "%s\tldc %f\n", j_buf,yylval.f_val);
                $<t.type>$ = 'F';
                $<t.pos>$ = strlen(j_buf);
            }
	;

function_expression
        : ID LB RB {
            if(lookup_symbol($1,'u',0) == 0){ //Undeclare
                // semantic error
                strcat(error_buf, "Undeclared function ");
                strcat(error_buf, $1);
                print_error_flag = 1;
            }
            }
        | ID LB argument_expression_list RB {
            if(lookup_symbol($1,'u',0) == 0){ //Undeclare
                // semantic error
                strcat(error_buf, "Undeclared function ");
                strcat(error_buf, $1);
                print_error_flag = 1;
            }
        }
        ;

postfix_expression
	: primary_expression
            {$$ = $1;}
        | function_expression
	| postfix_expression LSB expression RSB
	| postfix_expression INC
            {
                if($<t.type>1 == 'I'){
                    sprintf(j_buf,"%s\tldc 1\n\tiadd\n\tistore %d\n", j_buf, $<t.reg>1);
                } else if($<t.type>1 == 'F'){
                    sprintf(j_buf,"%s\tldc 1.0\n\tfadd\nfstore %d\n", j_buf, $<t.reg>1);
                }
                $<t.pos>$ = strlen(j_buf);
                //TODO has load : add a type in <t>
                $<t.reg>$ = $<t.reg>1;
            }
	| postfix_expression DEC
            {
                if($<t.type>1 == 'I'){
                    sprintf(j_buf,"%s\tldc 1\n\tisub\n\tistore %d\n", j_buf, $<t.reg>1);
                } else if($<t.type>1 == 'F'){
                    sprintf(j_buf,"%s\tldc 1.0\n\tfsub\nfstore %d\n", j_buf, $<t.reg>1);
                }
                $<t.pos>$ = strlen(j_buf); 
                $<t.reg>$ = $<t.reg>1;
            }
	;

argument_expression_list
	: assignment_expression
	| argument_expression_list COMMA assignment_expression
	;

unary_expression
	: postfix_expression
            {$$ = $1;}
	| INC unary_expression 
            { 
                if($<t.type>2 == 'I'){
                    sprintf(j_buf,"%s\tldc 1\n\tiadd\n\tistore %d\n", j_buf, $<t.reg>1);
                } else if($<t.type>2 == 'F'){
                    sprintf(j_buf,"%s\tldc 1.0\n\tfadd\nfstore %d\n", j_buf, $<t.reg>1);
                }
                $<t.pos>$ = strlen(j_buf); 
            }
	| DEC unary_expression 
            { 
                if($<t.type>2 == 'I'){
                    sprintf(j_buf,"%s\tldc 1\n\tisub\n\tistore %d\n", j_buf, $<t.reg>1);
                } else if($<t.type>2 == 'F'){
                    sprintf(j_buf,"%s\tldc 1.0\n\tfsub\nfstore %d\n", j_buf, $<t.reg>1);
                }
                $<t.pos>$ = strlen(j_buf); 
            }
	| unary_operator cast_expression { $$ = $2;}
	;

unary_operator
	: MUL
	| ADD 
	| SUB
	;

cast_expression
	: unary_expression
            {$$ = $1;}
	| LB type RB cast_expression
            {
                $<t.type>$ = type_i2c($2);
            }
	;

multiplicative_expression
	: cast_expression
            {$$ = $1;}
	| multiplicative_expression MUL cast_expression
            {
                if($<t.type>1 == 'I' && $<t.type>3 == 'I'){
                    sprintf(j_buf, "%s\timuv\n", j_buf);
                    $<t.type>$ = 'I';
                    $<t.pos>$ = strlen(j_buf);
                } else {
                    if($<t.type>1 == 'I'){
                        insert_str2j_buf("\ti2f\n", $<t.pos>1);
                    }
                    if($<t.type>3 == 'I'){
                        sprintf(j_buf, "%s\ti2f\n", j_buf);
                    }
                    sprintf(j_buf, "%s\tfmul\n", j_buf);
                    $<t.type>$ = 'F';
                    $<t.pos>$ = strlen(j_buf);
                }
                
            }
	| multiplicative_expression DIV cast_expression
            {
                if($<t.type>1 == 'I' && $<t.type>3 == 'I'){
                    sprintf(j_buf, "%s\tidiv\n", j_buf);
                    $<t.type>$ = 'I';
                    $<t.pos>$ = strlen(j_buf);
                } else {
                    if($<t.type>1 == 'I'){
                        insert_str2j_buf("\ti2f\n", $<t.pos>1);
                    }
                    if($<t.type>3 == 'I'){
                        sprintf(j_buf, "%s\ti2f\n", j_buf);
                    }
                    sprintf(j_buf, "%s\tfdiv\n", j_buf);
                    $<t.type>$ = 'F';
                    $<t.pos>$ = strlen(j_buf);
                }
                
            }
	| multiplicative_expression MOD cast_expression
            {
                if($<t.type>1 == 'I' && $<t.type>3 == 'I'){
                    sprintf(j_buf, "%s\tirem\n", j_buf);
                    $<t.type>$ = 'I';
                    $<t.pos>$ = strlen(j_buf);
                } else {
                    strcat(error_buf, "Mod can only be integer");
                    print_error_flag = 1;
                }
                
            }
	;

additive_expression
	: multiplicative_expression
            {$$ = $1;}
	| additive_expression ADD multiplicative_expression 
            {
                if($<t.type>1 == 'I' && $<t.type>3 == 'I'){
                    sprintf(j_buf, "%s\tiadd\n", j_buf);
                    $<t.type>$ = 'I';
                    $<t.pos>$ = strlen(j_buf);
                } else {
                    if($<t.type>1 == 'I'){
                        insert_str2j_buf("\ti2f\n", $<t.pos>1);
                    }
                    if($<t.type>3 == 'I'){
                        sprintf(j_buf, "%s\ti2f\n", j_buf);
                    }
                    sprintf(j_buf, "%s\tfadd\n", j_buf);
                    $<t.type>$ = 'F';
                    $<t.pos>$ = strlen(j_buf);
                }
                
            }
	| additive_expression SUB multiplicative_expression
            {
                if($<t.type>1 == 'I' && $<t.type>3 == 'I'){
                    sprintf(j_buf, "%s\tisub\n", j_buf);
                    $<t.type>$ = 'I';
                    $<t.pos>$ = strlen(j_buf);
                } else {
                    if($<t.type>1 == 'I'){
                        insert_str2j_buf("\ti2f\n", $<t.pos>1);
                    }
                    if($<t.type>3 == 'I'){
                        sprintf(j_buf, "%s\ti2f\n", j_buf);
                    }
                    sprintf(j_buf, "%s\tfsub\n", j_buf);
                    $<t.type>$ = 'F';
                    $<t.pos>$ = strlen(j_buf);
                }
                
            }
	;

relational_expression
	: additive_expression
            {$$ = $1;}
	| relational_expression LT additive_expression
            {
                if($<t.type>1 == 'I' && $<t.type>3 == 'I'){
                    sprintf(j_buf, "%s\tisub\n", j_buf);
                    $<t.type>$ = 'L'+'T';
                    $<t.pos>$ = strlen(j_buf);
                } else {
                    if($<t.type>1 == 'I'){
                        insert_str2j_buf("\ti2f\n", $<t.pos>1);
                    }
                    if($<t.type>3 == 'I'){
                        sprintf(j_buf, "%s\ti2f\n", j_buf);
                    }
                    sprintf(j_buf, "%s\tfsub\n\tf2i\n", j_buf);
                    $<t.type>$ = 'L'+'T';
                    $<t.pos>$ = strlen(j_buf);
                }
                
            }
	| relational_expression MT additive_expression
            {
                if($<t.type>1 == 'I' && $<t.type>3 == 'I'){
                    sprintf(j_buf, "%s\tisub\n", j_buf);
                    $<t.type>$ = 'M'+'T';
                    $<t.pos>$ = strlen(j_buf);
                } else {
                    if($<t.type>1 == 'I'){
                        insert_str2j_buf("\ti2f\n", $<t.pos>1);
                    }
                    if($<t.type>3 == 'I'){
                        sprintf(j_buf, "%s\ti2f\n", j_buf);
                    }
                    sprintf(j_buf, "%s\tfsub\n\tf2i\n", j_buf);
                    $<t.type>$ = 'M'+'T';
                    $<t.pos>$ = strlen(j_buf);
                }
                
            }
	| relational_expression LTE additive_expression
            {
                if($<t.type>1 == 'I' && $<t.type>3 == 'I'){
                    sprintf(j_buf, "%s\tisub\n", j_buf);
                    $<t.type>$ = 'L'+'T'+'E';
                    $<t.pos>$ = strlen(j_buf);
                } else {
                    if($<t.type>1 == 'I'){
                        insert_str2j_buf("\ti2f\n", $<t.pos>1);
                    }
                    if($<t.type>3 == 'I'){
                        sprintf(j_buf, "%s\ti2f\n", j_buf);
                    }
                    sprintf(j_buf, "%s\tfsub\n\tf2i\n", j_buf);
                    $<t.type>$ = 'L'+'T'+'E';
                    $<t.pos>$ = strlen(j_buf);
                }
                
            }
	| relational_expression MTE additive_expression
            {
                if($<t.type>1 == 'I' && $<t.type>3 == 'I'){
                    sprintf(j_buf, "%s\tisub\n", j_buf);
                    $<t.type>$ = 'M'+'T'+'E';
                    $<t.pos>$ = strlen(j_buf);
                } else {
                    if($<t.type>1 == 'I'){
                        insert_str2j_buf("\ti2f\n", $<t.pos>1);
                    }
                    if($<t.type>3 == 'I'){
                        sprintf(j_buf, "%s\ti2f\n", j_buf);
                    }
                    sprintf(j_buf, "%s\tfsub\n\tf2i\n", j_buf);
                    $<t.type>$ = 'M'+'T'+'E';
                    $<t.pos>$ = strlen(j_buf);
                }
                
            }
	;

equality_expression
	: relational_expression
            {$$ = $1;}
	| equality_expression EQ relational_expression
            {
                if($<t.type>1 == 'I' && $<t.type>3 == 'I'){
                    sprintf(j_buf, "%s\tisub\n", j_buf);
                    $<t.type>$ = 'E'+'Q';
                    $<t.pos>$ = strlen(j_buf);
                } else {
                    if($<t.type>1 == 'I'){
                        insert_str2j_buf("\ti2f\n", $<t.pos>1);
                    }
                    if($<t.type>3 == 'I'){
                        sprintf(j_buf, "%s\ti2f\n", j_buf);
                    }
                    sprintf(j_buf, "%s\tfsub\n\tf2i\n", j_buf);
                    $<t.type>$ = 'E'+'Q';
                    $<t.pos>$ = strlen(j_buf);
                }
                
            }
	| equality_expression NE relational_expression
            {
                if($<t.type>1 == 'I' && $<t.type>3 == 'I'){
                    sprintf(j_buf, "%s\tisub\n", j_buf);
                    $<t.type>$ = 'N'+'E';
                    $<t.pos>$ = strlen(j_buf);
                } else {
                    if($<t.type>1 == 'I'){
                        insert_str2j_buf("\ti2f\n", $<t.pos>1);
                    }
                    if($<t.type>3 == 'I'){
                        sprintf(j_buf, "%s\ti2f\n", j_buf);
                    }
                    sprintf(j_buf, "%s\tfsub\n", j_buf);
                    $<t.type>$ = 'N'+'E';
                    $<t.pos>$ = strlen(j_buf);
                }
                
            }
	;

logical_and_expression
        : equality_expression
            {$$ = $1;}
	| logical_and_expression AND equality_expression
	;

logical_or_expression
	: logical_and_expression
            {$$ = $1;}
	| logical_or_expression OR logical_and_expression
	;

assignment_expression
	: logical_or_expression 
            {$$ = $1;}
	| ID assignment_operator assignment_expression
            {
                if(lookup_symbol($1,'u',0) == 0){ //Undeclare
                    // semantic error
                    strcat(error_buf, "Undeclared variable ");
                    strcat(error_buf, $1);
                    print_error_flag = 1;
                } else {
                    struct symbol *now = store_var($1, $<t.type>3);
                    // don't care type of $3, because only i==i f==f
                    $<t.type>$ = type_i2c(now->type);
                    $<t.pos>$ = strlen(j_buf);
                }

            }
	;

assignment_operator
	: ASGN
	| MULASGN
	| DIVASGN
	| MODASGN
	| ADDASGN
	| SUBASGN
	;

expression
	: assignment_expression
	| expression COMMA assignment_expression { $$ = $3;}
	;


parameter_list
	: type ID 
            {   
                g_scope++;
                $$ = $1;
                    
                if(lookup_symbol($2, 'd', 0) == 0){
                    insert_symbol($2, 1, $1, -1, g_scope); 
                } else{
                    // semantic error
                    strcat(error_buf, "Redeclared variable ");
                    strcat(error_buf, $2);
                    print_error_flag = 1;
                }
                g_scope--;
            }
	| parameter_list COMMA type ID 
            {   
                g_scope++;
                $$ = $1*10 + $3; 
                
                if(lookup_symbol($4, 'd',0) == 0){
                    insert_symbol($4, 1, $3, -1, g_scope);
                }else{
                    // semantic error
                    strcat(error_buf, "Redeclared variable ");
                    strcat(error_buf, $4);
                    print_error_flag = 1;
                }

                g_scope--;
            }
        ;

compound_stat
	: lcb rcb {$<t.type>$ = 0;}
	| lcb block_item_list rcb {$$ = $2;}
	;

lcb
    : LCB {
        g_scope++;
        }
    ;

rcb
    : RCB {
            print_table_flag = g_scope;
            g_scope--;
            }
    ;

block_item_list
	: stat 
	| block_item_list stat {$$ = $2;}
	;

expression_stat
	: SEMICOLON
            {
                //TODO
                $<t.type>$ = 0;
            }
	| expression SEMICOLON
	;

selection_stat
	: IF LB expression RB stat ELSE stat
            {
                char tmp[256];
                switch($<t.type>3){
                    case 'E'+'Q':
                        sprintf(tmp, "\tifne Label_%d\n", label_num);
                        insert_str2j_buf(tmp, $<t.pos>3);
                        break;
                    case 'N'+'E':
                        sprintf(tmp, "\tifeq Label_%d\n", label_num);
                        insert_str2j_buf(tmp, $<t.pos>3);
                        break;
                    case 'M'+'T':
                        sprintf(tmp, "\tifle Label_%d\n", label_num);
                        insert_str2j_buf(tmp, $<t.pos>3);
                        break;
                    case 'L'+'T':
                        sprintf(tmp, "\tifge Label_%d\n", label_num);
                        insert_str2j_buf(tmp, $<t.pos>3);
                        break;
                    case 'M'+'T'+'E':
                        sprintf(tmp, "\tiflt Label_%d\n", label_num);
                        insert_str2j_buf(tmp, $<t.pos>3);
                        break;
                    case 'L'+'T'+'E':
                        sprintf(tmp, "\tifgt Label_%d\n", label_num);
                        insert_str2j_buf(tmp, $<t.pos>3);
                        break;

                }

                // above insert str in j_buf, so nead to refresh other
                $<t.pos>5 += strlen(tmp);
                $<t.pos>7 += strlen(tmp);

                sprintf(tmp, "\tgoto EXIT_%d\nLabel_%d:\n", label_num, label_num);
                insert_str2j_buf(tmp, $<t.pos>5);

                // above insert str in j_buf, so nead to refresh other
                $<t.pos>7 += strlen(tmp);
                sprintf(tmp, "EXIT_%d:\n", label_num);
                insert_str2j_buf(tmp, $<t.pos>7);

                label_num++;
                $<t.pos>$ = strlen(j_buf);
            }
	| IF LB expression RB stat
            {
                char tmp[256];
                switch($<t.type>3){
                    case 'E'+'Q':
                        sprintf(tmp, "\tifne Label_%d\n", label_num);
                        insert_str2j_buf(tmp, $<t.pos>3);
                        break;
                    case 'N'+'E':
                        sprintf(tmp, "\tifeq Label_%d\n", label_num);
                        insert_str2j_buf(tmp, $<t.pos>3);
                        break;
                    case 'M'+'T':
                        sprintf(tmp, "\tifle Label_%d\n", label_num);
                        insert_str2j_buf(tmp, $<t.pos>3);
                        break;
                    case 'L'+'T':
                        sprintf(tmp, "\tifge Label_%d\n", label_num);
                        insert_str2j_buf(tmp, $<t.pos>3);
                        break;
                    case 'M'+'T'+'E':
                        sprintf(tmp, "\tiflt Label_%d\n", label_num);
                        insert_str2j_buf(tmp, $<t.pos>3);
                        break;
                    case 'L'+'T'+'E':
                        sprintf(tmp, "\tifgt Label_%d\n", label_num);
                        insert_str2j_buf(tmp, $<t.pos>3);
                        break;

                }

                // above insert str in j_buf, so nead to refresh other
                $<t.pos>5 += strlen(tmp);

                sprintf(tmp, "Label_%d:\n", label_num);
                insert_str2j_buf(tmp, $<t.pos>5);


                label_num++;
                $<t.pos>$ = strlen(j_buf);
            }
	;

while_lb: LB{
                $$ = strlen(j_buf);
            }
        ;
iteration_stat
	: WHILE while_lb expression RB stat 
            {
                char tmp[256];
                sprintf(tmp, "Label_%d_BEGIN:\n", label_num);
                insert_str2j_buf(tmp, $2);

                $<t.pos>3 += strlen(tmp);
                $<t.pos>5 += strlen(tmp);
                switch($<t.type>3){
                    case 'E'+'Q':
                        sprintf(tmp, "\tifeq Label_%d_TRUE\n\tgoto Label_%d_FALSE\nLabel_%d_TRUE:\n", label_num, label_num, label_num);
                        insert_str2j_buf(tmp, $<t.pos>3);
                        break;
                    case 'N'+'E':
                        sprintf(tmp, "\tifne Label_%d_TRUE\n\tgoto Label_%d_FALSE\nLabel_%d_TRUE:\n",label_num, label_num, label_num);
                        insert_str2j_buf(tmp, $<t.pos>3);
                        break;
                    case 'M'+'T':
                        sprintf(tmp, "\tifgt Label_%d_TRUE\n\tgoto Label_%d_FALSE\nLabel_%d_TRUE:\n",label_num, label_num, label_num);
                        insert_str2j_buf(tmp, $<t.pos>3);
                        break;
                    case 'L'+'T':
                        sprintf(tmp, "\tiflt Label_%d_TRUE\n\tgoto Label_%d_FALSE\nLabel_%d_TRUE:\n",label_num, label_num, label_num);
                        insert_str2j_buf(tmp, $<t.pos>3);
                        break;
                    case 'M'+'T'+'E':
                        sprintf(tmp, "\tifge Label_%d_TRUE\n\tgoto Label_%d_FALSE\nLabel_%d_TRUE:\n",label_num, label_num, label_num);
                        insert_str2j_buf(tmp, $<t.pos>3);
                        break;
                    case 'L'+'T'+'E':
                        sprintf(tmp, "\tifle Label_%d_TRUE\n\tgoto Label_%d_FALSE\nLabel_%d_TRUE:\n",label_num, label_num, label_num);
                        insert_str2j_buf(tmp, $<t.pos>3);
                        break;

                }

                // above insert str in j_buf, so nead to refresh other
                $<t.pos>5 += strlen(tmp);

                sprintf(tmp, "\tgoto Label_%d_BEGIN\nLabel_%d_FALSE:\n\tgoto EXIT_%d\nEXIT_%d:\n", label_num, label_num, label_num, label_num);
                insert_str2j_buf(tmp, $<t.pos>5);


                label_num++;
                $<t.pos>$ = strlen(j_buf);
            }
	| FOR LB expression_stat expression_stat RB stat
            {
                //TODO
                $<t.type>$ = 0;
            }
	| FOR LB expression_stat expression_stat expression RB stat
            {
                //TODO
                $<t.type>$ = 0;
            }
	| FOR LB declaration expression_stat RB stat
            {
                //TODO
                $<t.type>$ = 0;
            }
	| FOR LB declaration expression_stat expression RB stat
            {
                //TODO
                $<t.type>$ = 0;
            }
	;

jump_stat
	: CONT SEMICOLON 
            {
                //TODO
                $<t.type>$ = 0;
            }
	| BREAK SEMICOLON
            {
                //TODO
                $<t.type>$ = 0;
            }
	| RET SEMICOLON 
            {
                //TODO
                $<t.type>$ = 0;
            }
	| RET expression SEMICOLON
            {
                //TODO
                $$ = $2;
            }
	;



external_declaration
	: function_definition 
        | function_declaration
	| ex_declaration_list SEMICOLON
	;

function_definition 
        : type ID LB parameter_list RB compound_stat 
            {   
                int err = lookup_symbol($2, 'd', 1);
                if(err == 0){
                    insert_symbol($2, 0, $1, $4, g_scope);
                } else if(err == 1){
                    // semantic error
                    strcat(error_buf, "Redeclared function ");
                    strcat(error_buf, $2);
                    print_error_flag = 1;
                } else {
                    //err == 2:  has declare function
                }
                ge_method($2, $4, $1);
            }
	| type ID LB RB compound_stat 
            {   
                int err = lookup_symbol($2, 'd', 1);
                if(err == 0){
                    insert_symbol($2, 0, $1, -1, g_scope);
                } else if(err == 1){
                    // semantic error
                    strcat(error_buf, "Redeclared function ");
                    strcat(error_buf, $2);
                    print_error_flag = 1;
                } else {
                    //err == 2:  has declare function
                }
                ge_method($2,-1,$1);
            }
	;



function_declaration
	: type ID LB parameter_list RB SEMICOLON
            {
                if(lookup_symbol($2, 'd', 0) == 0){
                    insert_symbol($2, 0, $1, $4, g_scope);
                } else {
                    // semantic error
                    strcat(error_buf, "Redeclared function ");
                    strcat(error_buf, $2);
                    print_error_flag = 1;
                }

                // take out all inserted parameter
                struct symbol_table *now;
                now = head -> next;
                while(now != NULL && now -> scope != g_scope + 1){
                    now = now -> next;
                }
                
                free_table(now);
            }
	| type ID LB RB SEMICOLON
            {
                if(lookup_symbol($2, 'd', 0) == 0){
                    insert_symbol($2, 0, $1, -1, g_scope);
                } else {
                    // semantic error
                    strcat(error_buf, "Redeclared function ");
                    strcat(error_buf, $2);
                    print_error_flag = 1;
                }
            }
        ;

ex_declaration_list
	: ex_declaration
	| ex_declaration_list COMMA ex_declaration 
	;


declaration_list
	: declaration
	| declaration_list COMMA declaration {$$ = $3;}
	;

print_func
        : PRINT LB I_CONST RB SEMICOLON
            {
                sprintf(j_buf, "%s\tldc %d\n", j_buf, $3);
                sprintf(j_buf, "%s\tgetstatic java/lang/System/out Ljava/io/PrintStream;\n\tswap\n\tinvokevirtual java/io/PrintStream/println(I)V\n", j_buf);
                $<t.pos>$ = strlen(j_buf);
            }
        | PRINT LB F_CONST RB SEMICOLON
            {
                sprintf(j_buf, "%s\tldc %f\n", j_buf, $3);
                sprintf(j_buf, "%s\tgetstatic java/lang/System/out Ljava/io/PrintStream;\n\tswap\n\tinvokevirtual java/io/PrintStream/println(F)V\n", j_buf);
                $<t.pos>$ = strlen(j_buf);
            }
        | PRINT LB QUOTA STR_CONST QUOTA RB SEMICOLON
            {
                char tmp[256];
                strncpy(tmp,$4,strlen($4)-3);
                sprintf(j_buf, "%s\tldc \"%s\"\n", j_buf, tmp);
                sprintf(j_buf, "%s\tgetstatic java/lang/System/out Ljava/io/PrintStream;\n\tswap\n\tinvokevirtual java/io/PrintStream/println(Ljava/lang/String;)V\n", j_buf);
                $<t.pos>$ = strlen(j_buf);
            }
        | PRINT LB ID RB SEMICOLON
            {
                if(lookup_symbol($3, 'u', 0) == 0){
                    // semantic error
                    strcat(error_buf, "Undeclared variable ");
                    strcat(error_buf, $3);
                    print_error_flag = 1;
                } else {
                    struct symbol *now = load_var($3);
                    sprintf(j_buf, "%s\tgetstatic java/lang/System/out Ljava/io/PrintStream;\n\tswap\n\tinvokevirtual java/io/PrintStream/println(", j_buf);
                    if(now -> type <= 2){ // int or float
                        sprintf(j_buf, "%s%c)V\n", j_buf, type_i2c(now->type));
                    } else {
                        sprintf(j_buf, "%sLjava/lang/String;)V\n", j_buf);
                    }
                }
                $<t.pos>$ = strlen(j_buf);

            }
        ;

%%

/* C code section */
int main(int argc, char** argv)
{
    yylineno = 0;
    head = malloc(sizeof(*head));
    memset(head, 0, sizeof(struct symbol_table));

    file = fopen("compiler_hw3.j", "w");
    fprintf(file,   ".class public compiler_hw3\n"
                    ".super java/lang/Object\n"
                    //".method public static main([Ljava/lang/String;)V\n"
                    );
    yyparse();
    printf("\nTotal lines: %d \n",yylineno);

    /*fprintf(file,   "\treturn\n"*/
                    /*".end method\n");*/

    fclose(file);

    return 0;
}

void yyerror(char *s)
{
    if(strcmp(s, "syntax error") == 0){
        yylineno++;
        if(print_error_flag == 1){
            yyerror(error_buf);
            memset(error_buf, 0, BUF_SIZE);
            print_error_flag = 0;
        }
    }
    printf("\n|-----------------------------------------------|\n");
    printf("| Error found in line %d: %s\n", yylineno, buf);
    printf("| %s", s);
    printf("\n| Unmatched token: %s", yytext);
    printf("\n|-----------------------------------------------|\n\n");
    
    //TODO delete file
    
    fclose(file);

    
    if(strcmp(s, "syntax error") == 0){
        exit(1);
    }

    //exit(-1);
}

struct symbol_table * create_symbol(int scope) {
    struct symbol_table *new, *now;
    new = malloc(sizeof(*new));
    memset(new, 0, sizeof(struct symbol_table));
    now = head;

    while(now -> next != NULL){
        now = now -> next;
    }
    now -> next = new;
    new -> scope = scope;
    return new;
}
struct symbol * insert_symbol(char *name, int kind, int type, int pa, int scope) {
    struct symbol_table *t;
    struct symbol *new, *now;

    t = head -> next;
    // because the scope number will not appear twice at the same time
    // find the symbol_table match the scope_number 
    if(t == NULL){
        t = create_symbol(scope);
    } else if(t != NULL &&scope != t -> scope){
        while(t != NULL && scope != t -> scope){
            t = t -> next;
        }
        if(t == NULL){
            t = create_symbol(scope);
        }
    }

    new = malloc(sizeof(struct symbol));
    memset(new, 0, sizeof(struct symbol));
    if(t -> child == NULL){
        t -> child = new;
        new -> index = 0;
    } else {
        now = t -> child;
        while(now -> next != NULL){
            now = now -> next;
        }
        now -> next = new;
        new -> index = now -> index + 1;
    }

    new -> name = name;
    new -> kind = kind;
    new -> type = type;
    new -> parameter = pa;
    return new;
}

int lookup_symbol(char * name, int declare_or_use, int define_function_or_not) {
    struct symbol_table *t;
    struct symbol *now;
    int scope_tmp;
    // check whether exist the table whose scope is g_scope
    t = head -> next;
    if(t == NULL){ // no table no error
        return 0;
    }

    if(declare_or_use == 'd'){
        scope_tmp = 0;
    } else if(declare_or_use == 'u'){
        scope_tmp = g_scope;
    } else {
        printf("error, call lookup_symbol() error\n");
        exit(1);
    }

    // if use the variable, find all scope <= g_scope
    // if decaration, only find the scope = g_scope
    for(int i = 0; i <= scope_tmp; i++){
        t = head -> next;
        while(t != NULL && t -> scope != g_scope-i){
            t = t -> next;
        }

        if(t == NULL) continue; // no such table that scope == g_scope-i

        now = t -> child;
        while(now != NULL && strcmp(now -> name, name) != 0){
            now = now -> next;
        }

        if(now == NULL) {
            continue;
        }else if(define_function_or_not == 1 && now -> kind == 0) {
            return 2; // already declare function
        }
        else {  // the table already has the symbol name
                
            return 1;
        }
    }

    return 0;
}

void dump_symbol(int scope) {
    struct symbol_table *t;
    struct symbol *now;

    if(head -> next == NULL){
        /*printf("error: head has no next\n");*/
        /*exit(1);*/
        return;
    }
    t = head -> next;
    
    while(t != NULL && t -> scope != scope){
        t = t -> next;
    }
    
    if(t == NULL){
        //printf("error: no such scope: %d in symbol table\n", scope);
        //exit(1);
        return;
    }


    printf("\n%-10s%-10s%-12s%-10s%-10s%-10s\n\n",
           "Index", "Name", "Kind", "Type", "Scope", "Attribute");
    
    if(t -> child != NULL){
        now = t -> child;
    } else {
        free_table(t);
        return;
    }
    // KIND :: 0: function, 1: parameter, 2: variable 
    // TYPE :: 1: int, 2: float, 3: bool, 4: string, 5: void
    
    while(now != NULL){
        printf("%-10d%-10s%-12s%-10s%-10d",
               now -> index, now -> name, KIND[now -> kind], TYPE[now -> type], t -> scope);
        if(now-> kind == 0 && now -> parameter != -1){ //function has parameter
            // turn the int which store paramamter to a int array
            char tmp[256];
            int out[intlen(now->parameter)];
            sprintf(tmp, "%d", now->parameter);
            for(int i = 0;i < intlen(now->parameter); i++){
                out[i] = tmp[i]-48; //turn ASCII code to decimal
            }

            memset(tmp, 0, sizeof(char)*256);
            for(int i = 0; i < intlen(now->parameter); i++){
                strcat(tmp, TYPE[out[i]]);
                if(i != intlen(now->parameter)-1)
                    strcat(tmp, ", ");
            }
            

            
            printf("%-s", tmp);     
        }
        printf("\n");
        now = now -> next;
    }

    printf("\n");
    free_table(t);
}

int intlen(int a){
    return floor(log10(abs(a))) + 1;
}

void free_table(struct symbol_table *t){
    struct symbol_table *prev;
    struct symbol *now, *next;
    prev = head;
    while(prev -> next != t && prev -> next != NULL){
        prev = prev -> next;
    }
    if(prev -> next == NULL){
        printf("error, no such symbol table to free\n");
        exit(1);
    }
    
    prev -> next = prev -> next -> next;

    now = t -> child;
    while(now != NULL){
        next = now->next;
        free(now);
        now = next;
    }

    free(t);

}

struct symbol * find_symbol(char *name, int scope){
    struct symbol * now_symbol;
    struct symbol_table * now_table;
    
    now_table = head -> next;

    while(now_table != NULL && now_table -> scope != scope){
        now_table = now_table -> next;
    }

    if(now_table == NULL){ // no such table
        return NULL;
    }

    now_symbol = now_table -> child;

    while(now_symbol != NULL && strcmp(now_symbol->name, name) != 0){
        now_symbol = now_symbol -> next;
    }
    
    return now_symbol;
}
/*void ge_field_s(char *name, char *value){*/
    /*fprintf(file, ".field public static %s S = %s", name, value);*/
/*}*/
void ge_field(char *name, int type, int value_type, float value){
    char t;
    // TYPE :: 1: int, 2: float, 3: bool, 4: string, 5: void
    switch(type){
        case 1:
            t = 'I';
            break;
        case 2: 
            t = 'F';
            break;
        case 3:
            t = 'Z';
            break;
    }
    // value_type :: -1: none value, 1: no dot(int), 2: has dot(float)
    fprintf(file, ".field public static %s %c", name, t);

    // turn float to integer
    if(type == 1 && value_type == 2){
        value_type = 1;
    }

    switch(value_type){
        case -1:
            fprintf(file, "\n");
            break;
        case 1:
            fprintf(file, " = %d\n", (int)value);
            break;
        case 2:
            fprintf(file, " = %f\n", value);
            break;
    }
}

void ge_method(char *name, int type, int return_type){
    if(strcmp(name,"main") == 0){
        fprintf(file, ".method public static main([Ljava/lang/String;)V\n");
    } else {
        fprintf(file, ".method public static %s (", name);

        char tmp[256];
        sprintf(tmp, "%d", type);
        for(int i = 0; i < strlen(tmp); i++){
            if(tmp[i] != '4'){ // not string
                fprintf(file, "%c", type_i2c(tmp[i]-48));
            } else {
                fprintf(file, "Ljava/lang/String;");
            }
        }
        fprintf(file, ")");
        fprintf(file, "%c", type_i2c(return_type));
        fprintf(file, "\n");

    }
    fprintf(file, ".limit stack 50\n"
                    ".limit locals 50\n");

    fprintf(file, "%s", j_buf);
    memset(j_buf, 0, sizeof(j_buf));

    fprintf(file, "\treturn\n"
                    ".end method\n");

}

struct symbol * load_var(char *name){
    struct symbol *now = find_symbol(name, g_scope);
    int t_scope = g_scope;
    while(t_scope > 0 && now == NULL){
        t_scope--;
        now = find_symbol(name, t_scope);
    }
    if(t_scope == 0){
        if(now->type != 4){
            sprintf(j_buf, "%s\tgetstatic compiler_hw3/%s %c\n", j_buf, name, type_i2c(now->type));
        } else {
            sprintf(j_buf, "%s\tgetstatic compiler_hw3/%s Ljava/lang/String;\n", j_buf, name);
        }
    } else {
        if(now -> type == 1 || now->type == 3){
            sprintf(j_buf, "%s\tiload %d\n", j_buf, now->index);
        } else if (now -> type == 2){
            sprintf(j_buf, "%s\tfload %d\n", j_buf, now->index);
        } else if (now -> type == 4){
            sprintf(j_buf, "%s\taload %d\n", j_buf, now -> index);
        }
    }
    return now;
}

struct symbol * store_var(char *name, int a_type){
    struct symbol *now = find_symbol(name, g_scope);
    int t_scope = g_scope;
    while(t_scope > 0 && now == NULL){
        t_scope--;
        now = find_symbol(name, t_scope);
    }
    if(t_scope == 0){
        if(now->type != 4){
            sprintf(j_buf, "%s\tputstatic compiler_hw3/%s %c\n", j_buf, name, type_i2c(now->type));
        } else {
            sprintf(j_buf, "%s\tputstatic compiler_hw3/%s Ljava/lang/String;\n", j_buf, name);
        }
    } else {
        if(now -> type == 1 || now -> type == 3){ //int or bool
            //f2i
            if(a_type == 'F'){
                strcat(j_buf, "\tf2i\n");
            }
            sprintf(j_buf, "%s\tistore %d\n", j_buf, now->index);
        } else if (now -> type == 2){
            //i2f
            if(a_type == 'I'){
                strcat(j_buf, "\ti2f\n");
            }
            sprintf(j_buf, "%s\tfstore %d\n", j_buf, now->index);
        } else if (now -> type == 4){
            sprintf(j_buf, "%s\tastore %d\n", j_buf, now->index);
        }
    }
    return now;
}

char type_i2c(int type){
    // TYPE :: 1: int, 2: float, 3: bool, 4: string, 5: void
    switch(type){
        case 1: 
            return 'I';
        case 2: 
            return 'F';
        case 3: 
            return 'Z';
        case 4: 
            return 'S';
        case 5:
            return 'V';
    }
    return '\0';
}
void insert_str2j_buf(char *str, int pos){
    char tmp[65536];
    strcpy(tmp, j_buf);
    memset(j_buf, 0, sizeof(j_buf));
    
    if(pos == strlen(tmp)){
        sprintf(j_buf, "%s%s", tmp, str);
    } else {
        sprintf(j_buf, "%s%s%s", strncat(j_buf, tmp, pos), str, tmp+pos);
    }
}
char *get_j_buf(int start, int end){
    char *tmp;
    tmp = strndup(j_buf+start, end-start);
    return tmp;
}

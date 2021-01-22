/*
EK_PARSER.Y
by Nesta Lenhert-Scholer
for MPCS 51300 - Compilers

This is the main grammar file for the Extended-Kaleidoscope Language.
*/

%error-verbose
%locations

%{
#include "ast.hpp"
#include <string>
#include <fstream>

Prog* the_prog;
extern int yylex();
extern int yyparse();
extern int yylineno;

extern FILE* yyin;
extern char* yytext;

void yyerror(const char* s);
%}

%union {
     float floatVal;
     int intVal;
     char* strVal;
     bool boolVal;
     Type* t_type;
     VDecl* t_vdecl;
     TDecls* t_tdecls;
     VDecls* t_vdecls;
     Exp* t_exp;
     Exps* t_exps;
     UOP* t_uop;
     Binop* t_binop;
     Stmt* t_stmt;
     Stmts* t_stmts;
     Blk* t_blk;
     Func* t_func;
     Funcs* t_funcs;
     Extern* t_extern;
     Externs* t_externs;
}

%token EXTERN
%token DEF
%token RETURN
%token WHILE
%token IF
%token ELSE
%token PRINT
%token <boolVal> TRUE
%token <boolVal> FALSE

%token INT
%token CINT
%token FLOAT
%token BOOL
%token VOID
%token NOALIAS
%token REF

%token EQ
%token AND
%token OR

%token <floatVal> FLIT
%token <intVal> LIT
%token <strVal> SLIT
%token <strVal> VARID
%token <strVal> GLOBID

%type <strVal> varid
%type <t_vdecl> vdecl
%type <t_tdecls> tdecls
%type <t_vdecls> vdecls
%type <t_type> type
%type <t_uop> uop
%type <t_binop> binop
%type <t_exp> exp
%type <t_exps> exps
%type <t_stmt> stmt
%type <t_stmts> stmts
%type <t_blk> blk
%type <t_func> func
%type <t_funcs> funcs
%type <t_extern> extern
%type <t_externs> externs

%nonassoc LOWER_THAN_ELSE
%nonassoc ELSE

%right '='
%left OR
%left AND
%left EQ
%left '<' '>'
%left '+' '-'
%left '*' '/'
%right "unary_minus" '!' TYPE_CAST

%code {
     void addFunc(char* id, Type* t, int lineno, VDecls* vdecls = nullptr, TDecls* tdecls = nullptr) {
          // Add a function to a vector when it is declared     
          if(BaseNode::functions.find(id) != BaseNode::functions.end()) {
               
               std::cerr << "error: line " << lineno << " - Function " << id << " already " \
                         << "declared. Functions may not have the same name." << std::endl;
               exit(1);

          } else if(vdecls) {
               BaseNode::functions.emplace(id, std::make_pair(t, vdecls));
          } else if(tdecls) {
               BaseNode::functions.emplace(id, std::make_pair(t, tdecls));
          } else {
               BaseNode::functions.emplace(id, std::make_pair(t, nullptr));
          }
     }

} //%code

%%

prog:     externs funcs  { the_prog = new Prog($2, $1); }
          | funcs        { the_prog = new Prog($1); }
          ;

externs:  externs extern      { $$ = $1; $$->externs.push_back($2); }
          | extern            { $$ = new Externs(); $$->externs.push_back($1); }
          ;

extern:   EXTERN type GLOBID '(' tdecls ')' ';'   { 
               addFunc($3, $2, yylineno, nullptr, $5);
               $$ = new Extern($2, $3, yylineno, $5); 
          }
          | EXTERN type GLOBID '(' ')' ';'        { 
               addFunc($3, $2, yylineno);
               $$ = new Extern($2, $3, yylineno); 
          }
          ; 

funcs:    funcs func     { $$ = $1; $$->funcs.push_back($2); }
          | func         { $$ = new Funcs(); $$->funcs.push_back($1); }
          ;

func:     DEF type GLOBID '(' { 
               // Needed to place scoping here to avoid seg fault
               BaseNode::scopes.emplace_back(); 
               BaseNode::last_scope_function = true; 
          } vdecls ')' { 
               // Add the parameter variables to the scope
               addFunc($3, $2, yylineno, $6); 
          } blk { 
               $$ = new Func($2, $3, $9, yylineno, $6); 
          }
          | DEF type GLOBID '(' ')' { addFunc($3, $2, yylineno); } blk { 
               $$ = new Func($2, $3, $7, yylineno); 
          }
          ;

blk:      '{' { if(BaseNode::last_scope_function) { 
                    BaseNode::last_scope_function = false;
               } else {
                    BaseNode::scopes.emplace_back(); 
               }
          } stmts '}'  { 
               BaseNode::scopes.pop_back(); 
               $$ = new Blk($3); 
          }
          | '{' '}'      { $$ = new Blk(); }
          ;

stmts:    stmts stmt     { $$ = $1; $$->stmts.push_back($2); }
          | stmt         { $$ = new Stmts(); $$->stmts.push_back($1); }
          ;

stmt:     blk                                          { $$ = $1; }
          | RETURN exp ';'                             { $$ = new Ret(yylineno, $2); }
          | RETURN ';'                                 { $$ = new Ret(yylineno); }
          | vdecl '=' exp ';'                          { $$ = new VDeclStmt($1, $3, yylineno); }
          | exp ';'                                    { $$ = new ExpStmt($1); }
          | WHILE '(' exp ')' stmt                     { $$ = new WhileStmt($3, $5, yylineno); }
          | IF '(' exp ')' stmt ELSE stmt              { $$ = new IfStmt($3, $5, yylineno, $7); }
          | IF '(' exp ')' stmt %prec LOWER_THAN_ELSE  { $$ = new IfStmt($3, $5, yylineno); }
          | PRINT exp ';'                              { $$ = new Print($2); }
          | PRINT SLIT ';'                             { $$ = new PrintSlit($2); }
          ;

exps:     exps ',' exp   { $$ = $1; $$->exps.push_back($3); }
          | exp          { $$ = new Exps(); $$->exps.push_back($1); }
          ;

exp:      '(' exp ')'                        { $$ = $2; }
          | binop                            { $$ = $1; }
          | uop                              { $$ = $1; }
          | varid '=' exp                    { $$ = new Assignment($1, $3, yylineno); }
          | '[' type ']' exp %prec TYPE_CAST { $$ = new TypeCast($2, $4, yylineno); }
          | TRUE                             { $$ = new BoolLit(true); }
          | FALSE                            { $$ = new BoolLit(false); }
          | FLIT                             { $$ = new FLit($1); }
          | LIT                              { $$ = new ILit($1); }
          | varid                            { $$ = new VarId($1, yylineno); }
          | GLOBID '(' exps ')'              { $$ = new FuncCall($1, yylineno, $3); }
          | GLOBID '(' ')'                   { $$ = new FuncCall($1, yylineno);}
          ;

binop:    exp '*' exp         { $$ = new Binop(Binop::mult, $1, $3, yylineno); }
          | exp '/' exp       { $$ = new Binop(Binop::div, $1, $3, yylineno); }
          | exp '+' exp       { $$ = new Binop(Binop::plus, $1, $3, yylineno); }
          | exp '-' exp       { $$ = new Binop(Binop::minus, $1, $3, yylineno); }
          | exp EQ exp        { $$ = new Binop(Binop::eq, $1, $3, yylineno); }
          | exp '<' exp       { $$ = new Binop(Binop::lt, $1, $3, yylineno); }
          | exp '>' exp       { $$ = new Binop(Binop::gt, $1, $3, yylineno); }
          | exp AND exp       { $$ = new Binop(Binop::bi_and, $1, $3, yylineno); }
          | exp OR exp        { $$ = new Binop(Binop::bi_or, $1, $3, yylineno); }
          ;

uop:      '!' exp   { $$ = new UOP(UOP::uop_not, $2, yylineno); }
          | '-' exp %prec "unary_minus" { $$ = new UOP(UOP::uop_minus, $2, yylineno); }
          ;

type:     INT  { $$ = new Type(Type::t_int); }
          | CINT    { $$ = new Type(Type::t_int); $$->check_overflow = true;}
          | FLOAT   { $$ = new Type(Type::t_float); }
          | BOOL    { $$ = new Type(Type::t_bool); }
          | VOID    { $$ = new Type(Type::t_void); }
          | NOALIAS REF type  { 
               $$ = $3;
               // TODO: Put this in a function
               // Handle case where type is void
               if($$->kind == Type::t_void) {
                    std::cerr << "error: line " << yylineno << " - " << "Reference variable " \
                              << "may not be type void." << std::endl;
                    exit(1);
               }
               
               // Handle case where type is already a reference
               if(!$$->ref) { 
                    $$->ref = true; 
               } else {
                    std::cerr << "error: line " << yylineno << " - " << "Reference variable " \
                              << "may not itself be a reference." << std::endl;
                    exit(1);
               }
               $$->noalias = true; 
          }
          | REF type     { 
               $$ = $2; 

               // Handle case where type is void
               if($$->kind == Type::t_void) {
                    std::cerr << "error: line " << yylineno << " - " << "Reference variable " \
                              << "may not be type void." << std::endl;
                    exit(1);
               }

               // Handle case where type is already a reference
               if(!$$->ref) { 
                    $$->ref = true; 
               } else {
                    std::cerr << "error: line " << yylineno << " - " << "Reference variable " \
                              << "may not itself be a reference." << std::endl;
                    exit(1);
               } 
          }
          ;

vdecls:   vdecls ',' vdecl    { $$ = $1; $$->vdecls.push_back($3); }
          | vdecl   { $$ = new VDecls(); $$->vdecls.push_back($1); }
          ;

tdecls:   tdecls ',' type     { $$ = $1; $$->types.push_back($3); }
          | type    { $$ = new TDecls(); $$->types.push_back($1); }
          ;

vdecl:    type varid     { $$ = new VDecl($1, $2, yylineno); }
          ;

varid:    VARID     { $$ = $1; }
          ;

%%

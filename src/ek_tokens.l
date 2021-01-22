/*EK_TOKENS.L*/
/*by Nesta Lenhert-Scholer and Bowen Bao*/
/*for MPCS 51300 - Compilers*/
/*Tokenizer Configuration file for Flex*/
/*TODO: fix print "5\n" being accepted*/

%{
#include <string>
#include "ast.hpp"
#include "ek_parser.hpp"

%}
%option noyywrap
%option yylineno
%%

extern              { return EXTERN; }
def                 { return DEF; }
return              { return RETURN; }
while               { return WHILE; }
if                  { return IF; }
else                { return ELSE; }
print               { return PRINT; }
true                { yylval.boolVal = true; return TRUE; }
false               { yylval.boolVal = false; return FALSE; }

int                 { return INT; }
cint                { return CINT; }
float               { return FLOAT; }
bool                { return BOOL; }
void                { return VOID; }
noalias             { return NOALIAS; }
ref                 { return REF; }

[ \t\n]             {}                  // Ignore basic whitespace
"#"[^\n]*           {}                  // Ignore comments
[();{}=,*/+\-<>!$]  { return *yytext; } 
"["                 { return *yytext; }
"]"                 { return *yytext; }
"=="                { return EQ; }
"&&"                { return AND; }
"||"                { return OR; }

[0-9]+\.[0-9]+               { try {
                                    yylval.floatVal = std::stof(yytext); 
                                    return FLIT; 
                               } catch(const std::out_of_range& e) {
                                   std::cout << "error: line " << yylineno 
                                             << " - float type out of range" << std::endl;
                                   exit(1);
                               }}
[0-9]+                       { try {
                                    yylval.intVal = std::stoi(yytext); 
                                    return LIT;
                               } catch(const std::out_of_range& e) {
                                   std::cout << "error: line " << yylineno 
                                             << " - integer type out of range" << std::endl;
                                   exit(1);
                               } }     
\"(\\.|[^\"\n\r])*\"         { yylval.strVal = strdup(yytext); return SLIT; }
\$[a-zA-Z_][a-zA-Z0-9_]*     { yylval.strVal = strdup(yytext); return VARID; }
[a-zA-Z_][a-zA-Z0-9_]*       { yylval.strVal = strdup(yytext); return GLOBID; }

%%

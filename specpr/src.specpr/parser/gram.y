%{

#include <stdio.h>

double atof();

%}

%start asexpr

%union {
        double floatval;
        RECORD recval;
}

%token <floatval> CONST
%token <recval> WAVEFILE
%token <recval> REC
%token LERR
%token EXP
%token LN
%token LOG
%token SIN
%token COS
%token TAN
%token INVCOS
%token INVSIN
%token INVTAN

%left '+' '-'
%left '*' '/'
%left UMINUS
%right '^'

%%

asexpr  :       expr
                        { pop(); }
                ;
        |       error
                        { YYABORT; }
                ;

expr    :       '(' expr ')'
                ;
        |       expr '+' expr
                        { add(); }
                ;
        |       expr '-' expr
                        { subtract(); }
                ;
        |       expr '*' expr
                        { multiply(); }
                ;
        |       expr '/' expr
                        { divide(); }
                ;
        |       '-' expr  %prec UMINUS
                        { negate(); }
                ;
        |       expr '^' expr
                        { power(); }
                ;
        |       EXP '(' expr ')'
                        { xexp();}
                ;
        |       LN  '(' expr ')'
                        { xln(); }
                ;
        |       LOG '(' expr ')'
                        { xlog(); }
                ;
        |       SIN '(' expr ')'
                        { xsin(); }
                ;
        |       COS '(' expr ')'
                        { xcos(); }
                ;
        |       TAN '(' expr ')'
                        { xtan();}
                ;
        |       INVCOS '(' expr ')'
                        { xinvcos(); }
                ;
        |       INVSIN '(' expr ')'
                        { xinvsin(); }
                ;
        |       INVTAN '(' expr ')'
                        { xinvtan(); }
                ;
        |       REC
                        { if(pushc($1)) {
                                YYERROR;
                        }       }
                ;
        |       WAVEFILE
                        { pushw($1); }
                ;
        |       CONST
                        { pushf($1); }
                ; 
		|       LERR
                        { YYERROR; }
                ;
%%
#include "lex.yy.c"

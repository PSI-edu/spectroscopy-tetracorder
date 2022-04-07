%{
extern char* lexeqp;
# define input() (((yytchar=yysptr>yysbuf?U(*--yysptr):nextchar(yyin))==10?(yylineno++,yytchar):yytchar)==EOF?0:yytchar)
# define unput(c) {yytchar= (c);if(yytchar=='\n')yylineno--;*yysptr++=yytchar;}
%}
D       [0-9]
E       [DEde][-+ ]?{D}+
F       [yuvwd]
G	[YUVWD]
%%
{D}+                    |
{D}+"."{D}*({E})?       |
{D}*"."{D}+({E})?       |
{D}+{E}                 {  yylval.floatval = atof(yytext);
                           strcpy(lexeqp,yytext);
                           lexeqp += yyleng;
                           return(CONST);
                        }
{F}{D}+                 {  yylval.recval.filename = yytext[0];
                           yylval.recval.recno = atoi(&yytext[1]);
                           strcpy(lexeqp,yytext);
                           lexeqp += yyleng;
                           return(REC);
                        }
{G}{D}                    {  yylval.recval.filename = yytext[0];
                           yylval.recval.recno = atoi(&yytext[1]);
                           strcpy(lexeqp,yytext);
                           lexeqp += yyleng;
                           return(WAVEFILE);
                        }
[-=+*/()^]              {  *lexeqp++ = yytext[0];
                           return(yytext[0]);
                        }
exp						{
							strcpy(lexeqp,yytext);
							lexeqp +=  yyleng;
							return(EXP);
						}
ln		                {
							strcpy(lexeqp,yytext);
							lexeqp +=  yyleng;
							return(LN);
						}
log		                {
							strcpy(lexeqp,yytext);
							lexeqp +=  yyleng;
							return(LOG);
						}
sin		 				{
							strcpy(lexeqp,yytext);
							lexeqp +=  yyleng;
							return(SIN);
						}
cos	   	                {
							strcpy(lexeqp,yytext);
							lexeqp +=  yyleng;
							return(COS);
						}
tan		                {
							strcpy(lexeqp,yytext);
							lexeqp +=  yyleng;
							return(TAN);
						}
invcos			        {
							strcpy(lexeqp,yytext);
							lexeqp +=  yyleng;
							return(INVCOS);
						}
invsin      			{
							strcpy(lexeqp,yytext);
							lexeqp +=  yyleng;
							return(INVSIN);
						}
invtan			        {
							strcpy(lexeqp,yytext);
							lexeqp +=  yyleng;
							return(INVTAN);
						}
" "						;
.                       return(LERR);
%%

#include "label1.h"
#include "blank.h"
#include "extdef.h"
#include "y.tab.c"
#include <math.h>

short numchans;						/* number of channels */
short error;							/* error comp flag */
struct specrec *stack[50];			/* calculation stack */
struct specrec *estack[50];			/* errors stack */
short tos = -1;						/* top of stack */
char equation[296];					/* equation for manual history */
char *lexeqp;						/* pointer to next char of equation */
char	inputline[80];
short	nextc;

parse(iopcon,nchans,err,oplen)

char *iopcon;
short *nchans,*err;
long oplen;
{
	short i;

#ifdef DEBUG
	fprintf(stderr,"Main called nchans=%d err=%d\n",*nchans,*err);
#endif

	lexeqp = &equation;
	for (i=0; i<sizeof equation; i++)
		equation[i] = '\0';

	for (;oplen>=0;oplen--)
		inputline[oplen] = iopcon[oplen];
	nextc = 0;
	
	numchans = *nchans;
	error = *err;

	return(yyparse());

}

parse_(iopcon,nchans,err,oplen)

char *iopcon;
short *nchans,*err;
long oplen;
{
	return(parse(iopcon,nchans,err,oplen));
}

abort()
{
#ifdef DEBUG
	fprintf(stderr,"Abort called\n");
#endif

}

yyerror(s)
char *s;
{
	fprintf(stderr,"%s\n",s);
}

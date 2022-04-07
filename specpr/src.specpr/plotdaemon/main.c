#define MAIN
#include	"splot.h"

/*
 *	%W% %G% %U%
 */


static char Sccsid[]="%W% %G% %U%";

FILE	*text;
FILE	*vector;
int		copies;
int		max_down;
int		max_across;


main(ac,av)
char **av;
{
	while (openargs()) {
		initplot();
		plotem();
		finishplt();
	}
	finishup();
}

#include	"hpdaemon.h"

/*
 *	@(#)hpdaemon.c	2.1 04/04/85 09:43:14
 */


static char Sccsid[]="@(#)hpdaemon.c	2.1 04/04/85 09:43:14";

FILE	*text;
FILE	*vector;
int		copies;
int		max_down;
int		max_across;


main(ac,av)
char **av;
{
	initport();
	while (openargs()) {
		while (copies--) {
			initplotter();
			plotem();
		}	
		finishplt();
	}
	finishup();
}

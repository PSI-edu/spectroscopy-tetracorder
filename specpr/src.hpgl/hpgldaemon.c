#include	"hpdaemon.h"

/*
 *	@(#)hpgldaemon.c	2.2 01/10/96 16:19:43
 */


static char Sccsid[]="@(#)hpgldaemon.c	2.2 01/10/96 16:19:43";

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

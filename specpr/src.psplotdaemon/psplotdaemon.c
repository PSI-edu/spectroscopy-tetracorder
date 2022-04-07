#include	"psdaemon.h"

/*
 *	psplotdaemon.c
 *	03/13/2006 Modified copy of hpgldaemon.c
 *
 *	Usage
 *		cat inputfile | pslotdaemon [-c] > outputfile.ps
 *			where:
 *			-c	graph line to consist a series of
 *				Bezier cubic curves.3
 */


FILE	*text;
FILE	*vector;
int		copies;
int		max_down;
int		max_across;


main(ac,av)
int ac;
char **av;
{
	char *prog = av[0];

	curveto = 0;   /* Default is line between two points */
	
	/* Check for curve option */
	while (--ac) {
		av++;
		if (*av[0] == '-') {
			if (!strcmp(*av, "-c"))
				curveto = 1;
			else {
				fprintf(stderr, "%s: unrecognized argument: %s\n", prog, *av);
			}
		} else {
			fprintf(stderr, "%s unrecognized argument: %s\n", prog, *av);
		}
	}


/* initport not needed since always going to stdout */
/*	initport(); */

	while (openargs()) {
		while (copies--) {
			initplotter();
			plotem();
		}	
		finishplt(); 
	}
	finishup(); 
}

#include	"splot.h"
/*
#include	<sys/vcmd.h>
 */
#include	"vcmd.h"

/*
 *	%W% %G% %U%
 */

int	plotmd[]	=	{VPLOT,	0,	0};
int	prtmd[]		=	{VPRINT, 0, 0};

static char Sccsid[]="%W% %G% %U%";

finishplt()
{
	int	fd,
		i;

	if (max_down+100 < NLINES) max_down += 100;
	else max_down = NLINES;

	ioctl(1, VSETSTATE, plotmd);

	for (i=0; i<max_down; i++) {
		write(1, &bbuf[WPL*i], WPL);
	}

	ioctl(1, VSETSTATE, prtmd);
/* RED - Commented out the following */
/*	close(fd); */
}

#include	"hpdaemon.h"

/*
 *	@(#)initpltr.c	2.3 02/07/89 16:53:38
 */


static char Sccsid[]="@(#)initpltr.c	2.3 02/07/89 16:53:38";

static	char line[256];


initplotter()
{
	int		error;

	pagefeed();
	INITIALIZE;
#ifdef PLOTTER
	SCPTS(300,600,8300,11400);
#endif
#ifdef PLOTTER2
 	SCPTS(300,-270,8300,10530);
#endif
	SCALE(0,2000,0,2048);
	LINETYPE(-1);
	PEN(3);
	VS(10);
}

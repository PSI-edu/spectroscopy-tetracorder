#include	"hpdaemon.h"

/*
 *	@(#)initpltr.c	2.5 01/12/96 12:55:28
 */


static char Sccsid[]="@(#)initpltr.c	2.5 01/12/96 12:55:28";

static	char line[256];


initplotter()
{
	int		error;

	pagefeed();

	/*
	 *SCPTS(300,600,8300,11400);
	 */

 	SCPTS(300,-270,8300,10530);

	SCALE(0,2000,0,2048);
	LINETYPE(-1);
	PEN(3);
	VS(10);
}

#include	"hpdaemon.h"

/*
 *	@(#)finishup.c	2.4 01/10/96 16:19:42
 */

static char Sccsid[]="@(#)finishup.c	2.4 01/10/96 16:19:42";

finishup()
{
	char line[1024];
	int	error;

	PEN(0);
	PLOTOFF;
	lseek (0, 0L, 0);

	while (gets(line)!=NULL) {
		switch (line[0]) {
		case 'R':
			error = unlink(&line[1]);
			if (error!=0) logerr("Can't delete %s (%d)\n",&line[1],error);
			else logerr("Deleted %s\n", &line[1]);
			break;
		default:
			break;
		}
	}
}

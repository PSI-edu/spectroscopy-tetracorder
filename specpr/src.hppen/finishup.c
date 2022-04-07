#include	"hpdaemon.h"

/*
 *	@(#)finishup.c	2.3 10/19/87 16:43:39
 */

static char Sccsid[]="@(#)finishup.c	2.3 10/19/87 16:43:39";

finishup()
{
	char line[1024];
	int	error;

	PEN(0);
	WAIT(line);
	GERR(error,line);
	if (error<0) {
		logerr("Finishup: Can't get error status\n");
	} else {
		line[error] = '\0';
		logerr("finishup: error = %s\n",line);
	}

#ifdef RS232 
	PLOTOFF;
#endif
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

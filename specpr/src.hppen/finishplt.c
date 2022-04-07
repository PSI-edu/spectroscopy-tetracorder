#include	"hpdaemon.h"

/*
 *	@(#)finishplt.c	2.1 04/04/85 09:42:19
 */

static char Sccsid[]="@(#)finishplt.c	2.1 04/04/85 09:42:19";

finishplt()
{
	char	line[256];
	int		error;

	WAIT(line);
	GERR(error, line);
	if (error<0) {
		logerr("finishplt: Can't get error status\n");
	} else {
		line[error] = '\0';
		logerr("finishplt error=%s\n", line);
	}
	PEN(0);
	pagefeed();
}

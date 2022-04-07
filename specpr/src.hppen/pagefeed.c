#include	"hpdaemon.h"

/*
 *	@(#)pagefeed.c	2.2 02/26/88 16:12:21
 */


static char Sccsid[]="@(#)pagefeed.c	2.2 02/26/88 16:12:21";

pagefeed()
{
	int		error;
	char	line[256];
	
	GOUTOPT(error, line);
	if (error<=0) {
		logerr("pagefeed: can't get plotter options\n");
	} else {
		line[error] = '\0';
		logerr("pagefeed: plotter options = '%s'\n", line);
	}
	sscanf(line, "%d", &error);
	switch(error) {
	case	3:
		ENABLE_CUTTER;
		logerr("pagefeed: dirty paper, max_down = %d\n", max_down);
		if (max_down > 1950) {
			PAPER_FULL;
			logerr("pagefeed: FULL\n");
		} else if (max_down!=0) {
			PAPER_HALF;
			logerr("pagefeed: HALF\n");
		} else {
			logerr("pagefeed: no feed\n");
		}
		break;
	
	case	1:
		logerr("pagefeed: Clean paper\n");
		break;

	case	0:
	case	2:
		logerr("pagefeed: abnormal plotter option(%d)\n", error);
		break;
	
	default:
		logerr("pagefeed: invalid plotter option(%d)\n", error);
		break;
	}
}

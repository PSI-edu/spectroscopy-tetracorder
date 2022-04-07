#include	"splot.h"

/*
 *	%W% %G% %U%
 */

static char Sccsid[]="%W% %G% %U%";

finishup()
{
	char	line[256];
	int		error;
	
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

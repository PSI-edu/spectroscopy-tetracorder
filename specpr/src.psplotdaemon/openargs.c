#include	"psdaemon.h"
/* RED - Added to following include on RH5 64bit to eliminate TLS mismatch error */
#include <errno.h>

/*
 *	openargs.c
 *	03/23/2006 Copy of openargs.c from hpgldaemon source directory
 */

extern int errno;

openargs()
{
	char line[128];
	int	error;

	copies = 1;

/*	We expect to get from stdin the contents of a file like one usually
	in /var/spool/gplot named aa0Nxxxxxxxxx.  Example:
		V/usr/spool/gplot/p0N141930996v
		T/usr/spool/gplot/p0N141930996t
		R/usr/spool/gplot/aa0N141930996
		R/usr/spool/gplot/p0N141930996v
		R/usr/spool/gplot/p0N141930996t
		C   1
*/

	while (gets(line) != NULL) {
		switch (line[0]) {
		case 'T':
			errno = 0;
			text = fopen(&line[1],"r");
			logerr("Text = %s\n", &line[1]);
			break;
		case 'V':
			errno = 0;
			vector = fopen(&line[1],"r");
			logerr("Vector = %s\n", &line[1]);
			break;
		case 'C':
			copies = atoi(&line[1]);
			if (copies<1 || copies>9) copies=1;
			if (vector==(FILE *)NULL || text==(FILE *)NULL) {
				logerr("Can't open input files vector=%d, text=%d\n",vector,text);
				exit(BAD_INPUT_FILE);
			}
			return (1);
		default:
			break;
		}
	}
	if (vector!=NULL) fclose(vector);
	if (text!=NULL) fclose(text);
	return (0);
}

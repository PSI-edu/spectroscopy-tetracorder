/* 02/29/2002 Randall Dailey
 *
 * Dumbed down code to just return with 0 with no processing
 */
#include "tape.h"
#ifdef WIN
   /* do not include non-standard iocrl.h used for tape operations */
#else
   #include <sys/ioctl.h>
#endif

/* 
 * fortran interface for rewind mag tape drive 
 *   usage:
 *		call rewinf(lun,ier)
 *
 *	arguments:
 *		lun	integer*2 variable which contains the 
 *			fortran logical unit number of the tape drive.
 *		ier	interer*2 variable used to return error indications
 *			for the rewind call. 0 indicates normal completion.
 *			nonzero values are the UNIX error number. 
 *			See intro(2) in the UNIX Manual for a description
 *			of these error numbers.
 */

rewinf(lun,ier)

short	*lun;
short *ier;
{
	int i;
	*ier = 0;
	i=1;
	return(i);
}

/* below is identical to above except _ added */

rewinf_(lun,ier)

short	*lun;
short *ier;
{
	int i;
	*ier = 0;
	i=1;
	return(i);
}

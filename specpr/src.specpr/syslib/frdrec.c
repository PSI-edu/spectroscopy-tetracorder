/* 02/29/2002 Randall Dailey
 *       Changed ../tape.h to tape.h 
 */

#include "tape.h"
#ifdef WIN
   /* do not include non-standard iocrl.h used for tape operations */
#else
   #include <sys/ioctl.h>
#endif

/* 
 * fortran interface for forward spacing a mag tape (by records)
 *   usage:
 *		call frdrec(lun,count,ier)
 *
 *	arguments:
 *		lun		integer*2 variable which contains the 
 *				fortran logical unit number of the tape drive.
 *		count	integer*2 variable which contains a count of
 *				the number of records to space.
 *		ier		interer*2 variable used to return error indications
 *				for the rewind call. 0 indicates normal completion.
 *				nonzero values are the UNIX error number. 
 *				See intro(2) in the UNIX Manual for a description
 *				of these error numbers.
 */

#define TMK	0x8000		/* Tape mark detected (in XS0 of ts11) */

/*  machine dependency for HPUX added 3/27/85. Roger Clark */


frdrec(lun,count,ier)
short	*lun;
short	*count;
short 	*ier;
{
	int i;
	i=1;
	return(i);
}

/* below is identical to above except _ added */

frdrec_(lun,count,ier)
short   *lun;
short   *count;
short   *ier;
{
	int i;
	i=1;
	return(i);
}


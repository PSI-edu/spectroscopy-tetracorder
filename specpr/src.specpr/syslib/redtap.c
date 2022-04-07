/* 02/29/2002 Randall Dailey
 *     Changed ../tape.h to tape.h
 */
/* fortran interface to read specpr format tapes */

#include "tape.h"

/*  machine dependency for HPUX added 3/27/85. Roger Clark */


redtap(idev,str,strln)
char *str;
long strln;
{
	int i;
	i=1;
	return(i);
}

/* below is identical to above except _ added */

redtap_(idev,str,strln)
char *str;
long strln;
{
	int i;
	i=1;
	return(i);
}


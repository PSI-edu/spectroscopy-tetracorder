/* fortran interface to write specpr tapes */

#include "../tape.h"

/*  machine dependency for HPUX added 3/27/85. Roger Clark */


wrttap_(idev,str,strln)
short *idev;
char *str;
long strln;
{
	return;
}

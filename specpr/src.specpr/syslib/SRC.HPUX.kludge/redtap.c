/* fortran interface to read specpr format tapes */

#include "../tape.h"

/*  machine dependency for HPUX added 3/27/85. Roger Clark */


redtap(idev,str,strln)
char *str;
long strln;
{
	return;
}

/* below is identical to above except _ added */

redtap_(idev,str,strln)
char *str;
long strln;
{
        return;
}


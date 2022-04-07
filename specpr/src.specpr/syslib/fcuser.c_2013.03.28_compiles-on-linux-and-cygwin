#include <stdio.h>

/* get the user id name, and pass it to the calling program which
   is normally a fortran subroutine
   Roger N. Clark 6/22/89
 */

long fcuser(uname)

char *uname;
{
	char *buff;
#ifdef WIN
        /*  buff = GetUserName(uname,256);  *** does not work yet  */
#else
	buff = cuserid(uname);   /* get userid */
#endif
	if (buff == NULL) {
		return(-1);
	} else {
		return(0);
	}
}


long fcuser_(uname)

char *uname;
{
	return(fcuser(uname));
}

#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <pwd.h>

/* get the user id name, and pass it to the calling program which
   is normally a fortran subroutine
   Roger N. Clark 6/22/89
 */

long fcuser(uname)

char *uname;
{
#if 0
	char *buff;
#endif
        struct passwd *uentry;
#ifdef WIN
        /*  buff = GetUserName(uname,256);  *** does not work yet  */
#else
#if 0
	buff = cuserid(uname);   /* get userid */
#endif
     	if ((uentry = getpwuid(getuid())) == NULL) {
		return(-1);
	} else {
        	strcpy(uname, uentry->pw_name);
		return(0);
	}

#if 0
	if (buff == NULL) {
		return(-1);
	} else {
		return(0);
	}
#endif
#endif
}


long fcuser_(uname)

char *uname;
{
	return(fcuser(uname));
}

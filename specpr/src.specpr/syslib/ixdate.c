/*
 *	fortran callable routine to get system date
 */

#include <time.h>

#include <sys/time.h>

/*  machine dependency added 3/27/85. Roger Clark */

ixdate(string,len)

char *string;
long len;
{
	long	clock;
	char 	buf[27];

	time (&clock);
	strcpy(string,ctime(&clock));
}

/* below is identical to above except _ added */

ixdate_(string,len)

char *string;
long len;
{
	long	clock;
	char 	buf[27];

	time (&clock);
	strcpy(string,ctime(&clock));
}

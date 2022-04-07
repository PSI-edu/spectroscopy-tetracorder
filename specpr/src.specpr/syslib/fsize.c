#include <sys/types.h>
#include <sys/stat.h>

/* fortran interface to get file size in bytes */

/*  machine dependency added 3/27/85. Roger Clark */

long fsize(name,len)

char *name;
long len;
{
	int i;
	char buf[256];
	struct stat	statbuf;

	for (i=0; i<(sizeof buf)-1 && name[i]!=' ' &&  i<len; i++) 
		buf[i] = name[i];

	buf[i] = '\0';

	if (stat(buf,&statbuf)==-1) return(-1);
	else return((long) statbuf.st_size);
}


long fsize_(name,len)

char *name;
long len;
{
	return(fsize(name,len));
}

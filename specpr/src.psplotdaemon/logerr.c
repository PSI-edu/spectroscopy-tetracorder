#include 	"psdaemon.h"

/*
 *	logerr.c
 */

int	DEBUG=1;

logerr(s1,s2,s3)
char *s1,*s2,*s3;
{
	FILE *fd;
	long i,time();
	char *ctime();

	if (DEBUG==0) return;
	fd = fopen(logfile,"a+");
	if (fd==NULL) return;
	i = time(0);
	fprintf(fd,"%.24s: (%05d) ",ctime(&i), getpid());
	fprintf(fd,s1,s2,s3);
	fclose(fd);
}

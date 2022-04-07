/* fortran 77 interface to link system call */

/* RED - Added to following include on RH5 64bit to eliminate TLS mismatch error */
#include <errno.h>

int syserr_;
extern errno;

/*  machine dependency added 3/27/85. Roger Clark */

ilink(f1,f2,l1,l2)

char *f1,*f2; long l1,l2;
{
	char buf1[256],buf2[256];
	long i;

	for (i=0; i<sizeof(buf1)-1 && f1[i]!=' ' && i<l1; i++)
		buf1[i] = f1[i];
	buf1[i] = '\0';

	for (i=0; i<sizeof(buf2)-1 && f2[i]!=' ' && i<l2; i++)
		buf2[i] = f2[i];
	buf2[i] = '\0';

#ifdef WIN
        i = 1;  /* set error number to 1; link function missing in mingw */
#else
	i = link(buf1,buf2);
#endif
	if (i!=0) syserr_ = errno;
	else syserr_ = 0;
	return (i);
}

/* below is identical to above except _ added */

ilink_(f1,f2,l1,l2)

char *f1,*f2; long l1,l2;
{
	char buf1[256],buf2[256];
	long i;

	for (i=0; i<sizeof(buf1)-1 && f1[i]!=' ' && i<l1; i++)
		buf1[i] = f1[i];
	buf1[i] = '\0';

	for (i=0; i<sizeof(buf2)-1 && f2[i]!=' ' && i<l2; i++)
		buf2[i] = f2[i];
	buf2[i] = '\0';

#ifdef WIN
        i = 1;  /* set error number to 1; link function missing in mingw */
#else
	i = link(buf1,buf2);
#endif
	if (i!=0) syserr_ = errno;
	else syserr_ = 0;
	return (i);
}

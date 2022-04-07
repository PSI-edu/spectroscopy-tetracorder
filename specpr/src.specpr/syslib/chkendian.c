#include <sys/types.h>
#include <sys/param.h>

int chkendian ()
{
  /* Return 0 if Big Endian (HPUX, SUN Solaris,..) */
  /* Return 1 then we are Little Endian (Intel)    */
  union
  {
    long l;
    char c[sizeof (long)];
  } u;
  u.l = 1;
  if (u.c[sizeof (long) - 1] == 1) return(0);
  return (1);
}


/*   fortran interface to the C sleep routine, with machine
     dependencies.  rewritten 3/25/85 by Roger Clark       */

#ifdef WIN
   #include <windows.h>
   #define sleep(a) Sleep(a * 1000)
#endif

fsleep(time)

unsigned long *time;
{
	sleep(*time);
}

/* below is identical to above except _ added */

fsleep_(time)

unsigned long *time;
{
	sleep(*time);
}

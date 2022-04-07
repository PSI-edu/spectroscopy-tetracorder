#include <stdlib.h>

initrand_()
{
	int i;
	i= (int) time(0);
#ifdef WIN
        /* do not use srand() peuso-random init */
#else
	srand48(i);
#endif
}
initrand()
{
	initrand_();
}

frandom(f)
float *f;
{
#ifdef WIN
        *f = (float)(rand())/RAND_MAX;
#else
	*f = drand48();
#endif
}

/* below is identical to above except _ added */

frandom_(f)
float *f;
{
#ifdef WIN
        *f = (float)(rand())/RAND_MAX;
#else
	*f = drand48();
#endif
}

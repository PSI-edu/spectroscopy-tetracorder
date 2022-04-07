#include "extdef.h"
#include "label1.h"
#include <math.h>

#define llim -1.23001e34
#define ulim -1.22999e34

notdel(x) 
float x;
{
	if (x>llim && x<ulim) return (0);
	else return (1);
}


add()
{
	register int i;

	for (i=0; i<numchans; i++)
		if(notdel(TOS->data[i]) && notdel(NOS->data[i])){
			NOS->data[i] += TOS->data[i];
			if (ERRORS)
				NOES->data[i] = hypot( (double)TOES->data[i],
					(double) NOES->data[i]);
		}
		else NOS->data[i] = DELETED;
	POP;
}

subtract()
{
	register int i;

	for (i=0; i<numchans; i++)
		if(notdel(TOS->data[i]) && notdel(NOS->data[i])) {
			NOS->data[i] -= TOS->data[i];
			if (ERRORS)
				NOES->data[i] = hypot( (double)TOES->data[i],
					(double) NOES->data[i]);
		}
		else NOS->data[i] = DELETED;
	POP;
}

multiply()
{
	register int i;
	float save;

	for (i=0; i<numchans; i++)
		if(notdel(TOS->data[i]) && notdel(NOS->data[i])) {
			save = NOS->data[i];
			NOS->data[i] *= TOS->data[i];
			if (ERRORS)
				if (TOS->data[i]==0.0 || save==0.0)
					NOES->data[i] = 0.0;
				else NOES->data[i] = hypot(
					(double)TOES->data[i]/TOS->data[i],
					(double)NOES->data[i]/save)*
						fabs(NOS->data[i]);
		}
		else NOS->data[i] = DELETED;
	POP;
}

divide()
{
	register int i;
	float save;

	for (i=0; i<numchans; i++)
		if(notdel(TOS->data[i]) &&
		   notdel(NOS->data[i]) &&
		   TOS->data[i]!=0.0) {
			save = NOS->data[i];
			NOS->data[i] /= TOS->data[i];
			if (ERRORS && TOS->data[i]!=0.0 && save!=0.0)
				NOES->data[i] = hypot(
					(double)TOES->data[i]/TOS->data[i],
					(double)NOES->data[i]/save)*
						fabs(NOS->data[i]);
		}
		else NOS->data[i] = DELETED;
	POP;
}

negate()
{
	register int i;

	for (i=0; i<numchans; i++)
		if(notdel(TOS->data[i]))
			TOS->data[i] = -TOS->data[i];
}

power()
{
	register int i;
	float save;

	for (i=0; i<numchans; i++)
		if(notdel(TOS->data[i]) && notdel(NOS->data[i])) {
			save = NOS->data[i];
			NOS->data[i] = (float) pow(NOS->data[i],TOS->data[i]);
			if (ERRORS) NOES->data[i] = fabs(NOS->data[i]) *
				hypot(TOS->data[i]/save*NOES->data[i],
				log(save)*TOES->data[i]);
		}
		else NOS->data[i] = DELETED;
	POP;
}

xexp()
{
	register int i;

	for (i=0; i<numchans; i++){
		
		if (notdel(TOS->data[i])) {
			if (TOS->data[i] > 82.893) 
				TOS->data[i] = 1.0e+36;
			else if (TOS->data[i] <(-82.893)) 
				TOS->data[i] = 1.0e-36;
			else
			    TOS->data[i] = exp(TOS->data[i]);
		}
	}
}
xln()
{
	register int i;

	for (i=0; i<numchans; i++){
		if (notdel(TOS->data[i])) {
			if (TOS->data[i] < 1.0e-36) 
				TOS->data[i] = -82.893;
			else
			    TOS->data[i] = log(TOS->data[i]);
		}
	}
}
xlog()
{
	register int i;

	for (i=0; i<numchans; i++){
		if (notdel(TOS->data[i])) {
			if(TOS->data[i] < 1.0e-36)
				TOS->data[i] = -36.0;
			else
			    TOS->data[i] = log10(TOS->data[i]);
		}
	}
}
xsin()
{
	register int i;

	for (i=0; i<numchans; i++){
		if (notdel(TOS->data[i]))
			TOS->data[i] = sin(TOS->data[i]);
	    if (ERRORS) TOES->data[i] = fabs(cos(TOES->data[i]));
	}
}
xcos()
{
	register int i;

	for (i=0; i<numchans; i++){
		if (notdel(TOS->data[i]))
			TOS->data[i] = cos(TOS->data[i]);
	}
}
xtan()
{
	register int i;

	for (i=0; i<numchans; i++){
		if (notdel(TOS->data[i]))
			TOS->data[i] = tan(TOS->data[i]);
	}
}
xinvcos()
{
	register int i;

	for (i=0; i<numchans; i++){
		if (notdel(TOS->data[i])){
			if (TOS->data[i] > 1.0) 
				TOS->data[i] = 1.0;
			else if (TOS->data[i] <(-1.0))
				TOS->data[i] = -1.0;
			else if (abs(TOS->data[i] < 1.0e-18))
				TOS->data[i] = 1.0e-18;
		    TOS->data[i] = acos(TOS->data[i]);
		}
	}
}
xinvsin()
{
	register int i;

	for (i=0; i<numchans; i++){
		if (notdel(TOS->data[i]))
			TOS->data[i] = asin(TOS->data[i]);
	}
}
xinvtan()
{
	register int i;

	for (i=0; i<numchans; i++){
		if (notdel(TOS->data[i]))
			TOS->data[i] = atan(TOS->data[i]);
	}
}

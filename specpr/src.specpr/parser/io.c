#include "label1.h"
#include "blank.h"
#include "extdef.h"
#include <stdio.h>

pop()
{
	history();
	return(1);
}


pushf(val)
float val;
{
	register i;
#ifdef DEBUG
	for (i=0; i<5; i++) printf("%f ",TOS->data[i]);
	printf("\n");
#endif
	stack[++tos] = malloc(sizeof (struct specrec));

	for (i=0; i<numchans; i++)
	      TOS->data[i] = val;
	if (ERRORS) {
		estack[tos] = malloc(sizeof (struct specrec));
		for (i=0; i<numchans; i++)
			TOES->data[i] = 0.0;
	}
#ifdef DEBUG
	for (i=0; i<5; i++) printf("%f ",TOS->data[i]);
	printf("\n");
	for (i=0; i<5; i++) printf("%f ",NOS->data[i]);
	printf("\n");
#endif
}

pushw(rec)
RECORD rec;
{
	int id,fd,count,ier;

	stack[++tos] = malloc(sizeof (struct specrec));
	if (ERRORS) {
		estack[tos] = malloc(sizeof (struct specrec));
		for (count=0; count<numchans; count++)
			TOES->data[count] = 0.0;
	}

	printf("pushing %c%d\n",rec.filename,rec.recno);

  	id=ihchar_(&rec.filename);
	wavlng_(&id,&rec.recno,&ier);
	ncopy(TOS->title,label1_.title,256);
	ncopy(TOS->data,blank_.dataa,1024);
	return(FALSE);
}

pushc(rec)
RECORD rec;
{
	int i,lun,ier;

	stack[++tos] = malloc(sizeof (struct specrec));
	if (ERRORS) estack[tos] = malloc(sizeof (struct specrec));

	printf("pushing %c%d\n",rec.filename,rec.recno);

	switch (rec.filename) {
	case 'v':
		lun = 8;
		break;
	
	case 'w':
		lun = 9;
		break;

	case 'u':
		lun = 3;
		break;

	case 'y':
		lun = 4;
		break;

	case 'd':
		lun = 7;
		break;
	}
	redfil_(&rec.recno,&lun,&ier);
	if (ier!=0) return(TRUE);
	ncopy(TOS->title,label1_.title,1536);
	if (ERRORS) {
		i = rec.recno+1;
		redfil_(&i,&lun,&ier);
		if (ier!=0) return(TRUE);
		ncopy(TOES->title,label1_.title,1536);
	}
#ifdef DEBUG
		for (i=0; i<numchans; i++) printf("%f ",TOS->data[i]);
#endif
	return(FALSE);
}

ncopy(dest,src,count)
register char *dest,*src;
register int count;
{
	while (count--) {
		*dest++ = *src++;
	}
}

yywrap()
{
return(1);
}

nextchar()
{
	if(nextc>=80) return(0);
	else return(inputline[nextc++]);
}

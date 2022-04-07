#include "label1.h"
#include "lbl4.h"
#include "lbl3.h"
#include "blank.h"
#include "extdef.h"
#include <stdio.h>
#include <sys/time.h>

history(rec)
RECORD rec;
{
	int j,i;
	char line[61];
	struct tm *tmp;
	long time(),gmt,jday,itime,isgn;

#ifdef DEBUG
	fprintf(stderr,"History called\n");
#endif
	time(&gmt);
	tmp = gmtime(&gmt);
	tojuld_(&tmp->tm_year,&tmp->tm_mon+1,&tmp->tm_mday,&jday);
	TOS->datea=jday;
	frdms_(&itime,24000,&tmp->tm_hour,&tmp->tm_min,&tmp->tm_sec,&isgn);
	TOS->cta=itime;

	TOS->revs = 1;
	TOS->filen = rec.recno;
	sprintf(line,"pars:");
	i = 5;
	if (index(&equation,'v') != 0) {
		sprintf(&line[i],"v=%-8.8s ",lbl4_.isavt);
		i += 11;
	}
	if (index(&equation,'w') != 0) {
		sprintf(&line[i],"w=%-8.8s ",lbl4_.iwdgt);
		i += 11;
	}
	if (index(&equation,'d') != 0) {
		sprintf(&line[i],"d=%-8.8s ",lbl4_.iwrkt);
		i += 11;
	}
	if (index(&equation,'u') != 0) {
		sprintf(&line[i],"u=%-8.8s ",lbl4_.inmu);
		i += 11;
	}
	if (index(&equation,'y') != 0) {
		sprintf(&line[i],"y=%-8.8s ",lbl4_.inmy);
		i += 11;
	}
	for (j=0; j<60; j++) TOS->ihist[j] = '\0';
	for (j=0; j<296; j++) TOS->mhist[j] = '\0';
	if (strlen(line)+strlen(equation)<59) {
		sprintf(TOS->ihist,"%s %s",line,equation);
		for (j=0; j<60; j++)
			if(TOS->ihist[j] == '\0') TOS->ihist[j]=' ';
		for (j=0; j<74; j++) {
			TOS->mhist[j] = ' ';
			TOS->mhist[j+74] = ' ';
			TOS->mhist[j+148] = ' ';
			TOS->mhist[j+222] = ' ';
		}
	} else {
		for (j=0; j<60 & line[j] != '\0'; j++)
			TOS->ihist[j] = line[j];
		for(; j<60; j++) TOS->ihist[j] = ' ';
#ifdef DEBUG
	for(i=0; i<5; i++) printf("%f ",TOS->data[i]);
	printf("\n");
#endif
		for (j=0; j<296; j++)
			if(TOS->mhist[j] == '\0') TOS->mhist[j]=' ';
		TOS->mhist[0] = '*';
		TOS->mhist[1] = '*';
		for (j=0; j<72; j++) {
			TOS->mhist[j+2] = equation[j];
			TOS->mhist[j+76] = equation[j+72];
			TOS->mhist[j+150] = equation[j+144];
			TOS->mhist[j+224] = equation[j+216];
		}
		if (TOS->mhist[76]==' ')
			TOS->mhist[74] = TOS->mhist[75] = '*';
		if (TOS->mhist[150]==' ')
			TOS->mhist[148] = TOS->mhist[149] = '*';
		if (TOS->mhist[224]==' ')
			TOS->mhist[222] = TOS->mhist[223] = '*';
	}
	TOS->nruns = 1;
	TOS->iwtrns = 1;

	ncopy(label1_.title,TOS->title,1536);
	ncopy(blank_.datac,label1_.data,1024);
#ifdef DEBUG
	for(i=0; i<10; i++) printf("%f ",blank_.datac[i]);
	printf("\n");
#endif
	if (ERRORS) ncopy(lbl3_.errors,TOES->data,1024);
}

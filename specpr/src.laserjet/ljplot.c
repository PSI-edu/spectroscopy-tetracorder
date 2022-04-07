#include <stdio.h>
#include <math.h>
#include <sys/file.h>
#include <limits.h>

#define PLOTFILE "/users/tmp/plotfile"
#define ESC '\033'
#define ST '\003'
#define SI '\016'
#define SO '\017'
#define LEN 256
#define YSCL 3000
#define XSCL 2400

struct vector{
short int y0;
short int x0;
short int y1;
short int x1;
short int width;
} v;

char logfile[] = "/users/log/gplot";

#include "logerr.c"

int vf,tmpf;

FILE *tf,*hp;

extern errno;

main(ac,av)
int ac; char **av;
{

void plotvec(),initplot(),closeplot(),settext(),setsym(),puttext();
char obuf[LEN],tbuf[LEN],label[LEN],type;
int er1,er2,x,y,size,orient,tflag=0,sflag=0;
float temp;

	if (ac != 3) {
		logerr(" Bad arg count.\n");
		exit(1);
	}

	vf = open(av[1],0);
	er1 = errno;
	tf = fopen(av[2],"r");
	er2 = errno;

	if (vf == -1 || tf == NULL) {
		logerr("can't open plot files verr=%d, terr=%d\n",er1,er2);
		logerr("\tvfile=%s,tfile=%s\n",av[1],av[2]);
		logerr("\tvf=%d,tf=%d\n",vf,tf);
		exit(2);
	}

	while((hp = fopen(PLOTFILE,"w")) == NULL){
		logerr("\nCan't open output file.");
		exit(2);
	}


	initplot(hp);         /* Initialize printer and put in into HPGL mode */

	while((read(vf,&v,sizeof v)) == sizeof v)  /* Scan and plot vectors */
		plotvec(&v,hp);

	while((fgets(tbuf,256,tf)) != NULL){       /* Scan and plot text file */
		sscanf(tbuf,"%c%d%d%d%d%[^\n]",&type,&y,&x,&size,&orient,label);
		if(type == 'T'){		/* check for text vs. symbol */
			if(sflag) fputs("SS;\n",hp); /* Last--symbol or text?*/
			if(orient == 0){           /* Horizonatl Text */
				fputs("DR1,0LO1;\n",hp);
				if(size != 16){    /* Set point size of text */
					temp = size / 16. * 12.;
					fprintf(hp,"SD4,%7.4f;\n",temp);
				}
				else
					fputs("SD4,12.\n",hp);
				
			}
			else if(orient == 1)  	    /* Vertical Text */
					fputs("DR0,1LO1;\n",hp);
			settext(x,y,size,orient,label,obuf);
			tflag = 1;
		}
		else if (size > 0){                 /* Deal with symbols */
			if(tflag){                  /* Last--text or symbol? */
				fputs("SADR1,0LO15;\n",hp);
				tflag = 0;
			}
			fprintf(hp,"AD4,%7.4f;\n",(float)((size-1) * 3.0 + 6.));
			setsym(x,y,orient,label,obuf);
			sflag = 1;
		}
		puttext(hp,obuf);	            /* Plot text */
	}

	closeplot(hp);
	logerr("normal exit\n");
	exit(0);
}

void initplot(a)
char *a;
{
	fputs("\033E\033&l2a0o0E",a);
	fputs("\033*p00yc5760x7920yc0T\033%1BIN;SP1;SC0,2400,0,3000;PU0,0;",a);
	fputs("SD1,277,2,1,4,12.,7,4148;AD1,269,2,1,4,12.,7,4148;LO1;\n",a);
}

void plotvec(v,hp)
struct vector *v;
FILE *hp;
{
float w;
	w = (float) v->width * 0.127;
	fprintf(hp,"PA%hd,%hdPW%4.2f,1;PD%hd,%hdPU\n",v->x0,(YSCL - v->y0),w,
							v->x1,(YSCL - v->y1));
}

void closeplot(a)
char *a;
{
	fputs("\033%0A\033E",a);
}

void settext(x,y,size,orient,label,buf)
int x,y,size,orient;
char *buf,*label;
{
void special();
int i;
char *a;
	a = label;
	for(i=0;a[i] != '\0';i++)
		if(a[i] == '!' || a[i] == '\\' || a[i] == '|' || a[i] == '{' 
								|| a[i]=='}'){
			special(label,i);
			break;
		}
	
	for(i=0;a[i] != ' ';i++);
	if(a[++i] >= '0' && a[i] <= '9'){
		sprintf(buf,"PA%d,%dLB%s\003PU",(x+24),(YSCL - y),label);
	}
	else{
		sprintf(buf,"PA%d,%dLB%s\003PU",x,(YSCL - y),label);
	}
}

void setsym(x,y,kind,label,buf)
int x,y,kind,label;
char *buf;
{
int type;

	switch (kind){

		case 0:
			sprintf(buf,"PA%d,%dLB\300\003PU",x,(YSCL - y));
			break;

		case 1:
			sprintf(buf,"PA%d,%dLB\301\003PU",x,(YSCL - y));
			break;

		case 2:
			sprintf(buf,"PA%d,%dLB\302\003PU",x,(YSCL - y));
			break;

		case 3:
			sprintf(buf,"PA%d,%dLB\303\003PU",x,(YSCL - y));
			break;

		case 4:
			sprintf(buf,"PA%d,%dLB\304\003PU",x,(YSCL - y));

		default:
			sprintf(buf,"PA%d,%dLB\300\003PU",x,(YSCL - y));
			break;
	}
}

void special(label,i)
char *label;
int i;
{
char greek(),math();
int j;


	for(j=i;label[j] != '\0';j++){
		switch (label[j]){
			case '!': 
				label[j] = SI;
				++j;
				for(;label[j] != '!';j++){
					label[j] = greek(label[j]);
				}
				label[j] = SO;
				break;
	
			case '|':
				label[j] = SI;
				++j;
				for(;label[j] != '|';j++)
					label[j] = math(label[j]);
				label[j] = SO;
				break;
		}
	}
	
}

char greek(c)
char c;
{
	switch (c){

		case 'g':
			return('c');

		case 'G':
			return('C');

		case 'z':
			return('f');

		case 'Z':
			return('F');

		case 'y':
			return('g');

		case 'Y':
			return('G');

		case 'k':
			return('j');
		
		case 'K':
			return('J');
		
		case 'l':
			return('k');
		
		case 'L':
			return('K');
		
		case 'm':
			return('l');
		
		case 'M':
			return('L');
		
		case 'n':
			return('m');
		
		case 'N':
			return('M');
		
		case 'c':
			return('n');
		
		case 'C':
			return('N');
		
		case 'r':
			return('q');
		
		case 'R':
			return('Q');
		
		case 's':
			return('r');
		
		case 'S':
			return('R');
		
		case 't':
			return('s');
		
		case 'T':
			return('S');
		
		case 'u':
			return('t');
		
		case 'U':
			return('T');
		
		case 'f':
			return('u');
		
		case 'F':
			return('U');
		
		case 'x':
			return('v');
		
		case 'X':
			return('V');
		
		case 'w':
			return('x');
		
		case 'W':
			return('X');
		
		default:
			return(c);
	}
}

char math(c) 
char c;
{
	switch(c){
		case 'a':
			return(c);
		default:
			return(c);
	}
}

void puttext(p,buf)
char *buf;
FILE *p;
{
	fputs(buf,p);
}


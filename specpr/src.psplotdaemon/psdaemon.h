#include <stdio.h>
#include <math.h>

/*
 *	psdaemon.h
 */


int	plotter;

extern	FILE	*text;
extern	FILE	*vector;
int	copies;
int	wait_count;
int	curveto;
extern	int	max_down;
extern	int	max_across;
char		temp[1024];


int iwtmp;
char intmp;
#define logfile "/usr/log/psplotdaemon"

#define	WRITE(a,b,c)		/*logerr("WRITE: (%d) %s\n",c,b);*/\
							write(a,b,c)

#define READ(a,b,cerr)  cerr = read(a,b,sizeof b);\
                        if (cerr == -1) {\
                            logerr("READ: (%d) %s\n",cerr,b);\
                           }



#define POINT	((1.0/72.0)*2.54)*0.5		/* 1-point = 1/72 inch */

#define	PLOTOFF			fprintf(stdout,"showpage\ngrestore\nflush\n")

#define STROKE			fprintf(stdout,"stroke\n")

#define	SCPTS(w,x,y,z)		sprintf(temp,"IP%d,%d,%d,%d;\n",w,x,y,z);\
							WRITE(plotter,temp,strlen(temp))

#define	SCALE(w,x,y,z)		sprintf(temp,"SC%d,%d,%d,%d;\n",w,x,y,z);\
					WRITE(plotter,temp,strlen(temp))

#define	WINDOW(w,x,y,z)		sprintf(temp,"IW%d,%d,%d,%d;\n",w,x,y,z);\
					WRITE(plotter,temp,strlen(temp))


#define TEXT(str)		fprintf(stdout,"%s",str)

#define TEXTLP			fprintf(stdout,"(")

#define TEXTRP			fprintf(stdout,")")

#define SHOWNL			fprintf(stdout," show\n")


#define GCS(a,x,v,h,str)	sprintf(temp,"\033%%1A\033\046a%dP\033(10U\033(s1P\033(s%dV\033\046a+%dV\033\046a-%dH%s\033\046a-%dV\033\046a+%dH\033%%0B",a,x,v,h,str,v,h);\
					WRITE(plotter,temp,strlen(temp))

#define TEXTSIZE(x)		fprintf(stdout,"/fsize %d def\n",x)

#define COURIER90		fprintf(stdout,"/Courier findfont fsize scalefont setfont 90.000000 rotate\n")

#define COURIER180		fprintf(stdout,"/Courier findfont fsize scalefont setfont 180.000000 rotate\n")

#define SYMBOL90		fprintf(stdout,"/Symbol findfont fsize scalefont setfont 90.000000 rotate\n")

#define SYMBOL180		fprintf(stdout,"/Symbol findfont fsize scalefont setfont 180.000000 rotate\n")

#define ROTATE90		fprintf(stdout," -90.000000 rotate\n")

#define ROTATE180		fprintf(stdout," -180.000000 rotate\n")


#define LINETYPE(x)		if (x<0) {\
					WRITE(plotter,"LT;\n",3);\
				} else {\
					sprintf(temp,"LT%d;\n",x);\
					WRITE(plotter,temp,strlen(temp));\
				}



#define	VS(x)			sprintf(temp,"VS%d;\n",x);\
					WRITE(plotter,temp,strlen(temp))

#define	MOVETO(x,y)		fprintf(stdout,"%d %d moveto\n",x,y);\
					if (x>max_down) max_down = x;\
					if (y>max_across) max_across = y

#define LINETO(x,y)		fprintf(stdout,"%d %d lineto\n",x,y);\
					if (x>max_down) max_down = x;\
					if (y>max_across) max_across = y

#define CURVETO(x1,y1,x2,y2,x3,y3)  fprintf(stdout,"%d %d %d %d %d %d curveto\n",x1,y1,x2,y2,x3,y3);\
                                        if (x1>max_down) max_down = x1;\
					if (x2>max_down) max_down = x2;\
					if (x3>max_down) max_down = x3;\
                                        if (y1>max_across) max_across = y1;\
					if (y2>max_across) max_across = y2;\
					if (y3>max_across) max_across = y3

#define SETWIDTH(x)		fprintf(stdout,"%d setlinewidth\n[] 0 setdash\n",x)

struct	vector	{
	short	df;	/*  down  coordinate of from point */
	short	af;	/* across coordinate of from point */
	short	dt;	/*  down  coordinate of  to  point */
	short	at;	/* across coordinate of  to  point */
	short	w;	/*       width of vector	   	   */
};

#define	OFFLINE			1
#define	OUT_OF_PAPER	2
#define BAD_OPEN		3
#define BAD_ARG_COUNT	4
#define BAD_INPUT_FILE	5
#define BAD_STATUS_READ	6

#include <stdio.h>
#include <math.h>

/*
 *	@(#)hpdaemon.h	2.10 10/20/87 13:50:47
 */

static char SCCSID[]="@(#)hpdaemon.h	2.10 10/20/87 13:50:47";

int	plotter;

extern	FILE	*text;
extern	FILE	*vector;
int				copies;
int				wait_count;
extern	int		max_down;
extern	int		max_across;
char			temp[1024];


int iwtmp;
char intmp;
#define logfile "/usr/log/hpdaemon"

#define	WRITE(a,b,c)		/*logerr("WRITE: (%d) %s\n",c,b);*/\
							write(a,b,c)


#ifdef HPIB

#define READ(a,b,cerr)	iwtmp = 0;\
			while(iwtmp <= sizeof b) {\
				cerr = read (a,&intmp,1);\
				if (cerr != -1) {\
					/*printf ("%c", intmp);*/\
					if (intmp == '\r') {\
						b[iwtmp] = '\0';\
						/*printf ("\n");*/\
						break;\
					}\
					b[iwtmp] = intmp;\
					iwtmp++;\
				} else {\
					logerr("READ: (%d) %s\n",cerr,b);\
				}\
			}

#endif 


#ifdef RS232

#define READ(a,b,cerr)  cerr = read(a,b,sizeof b);\
                        if (cerr == -1) {\
                            logerr("READ: (%d) %s\n",cerr,b);\
                           }

#endif


#define POINT	((1.0/72.0)*2.54)*0.5		/* 1-point = 1/72 inch */

#define PLOTON			WRITE(plotter,"\033.(",3)

#define	PLOTOFF			WRITE(plotter,"\033.)",3)

#define	HSHK2			WRITE(plotter,"\033.I;;17:",8)

#define	XHSHK			WRITE(plotter,"\033.N;19:",7)

#define	ALARM			alarm(120)

#define	INITIALIZE		WRITE(plotter,"DF;",3)

#define	SCPTS(w,x,y,z)		sprintf(temp,"IP%d,%d,%d,%d;",w,x,y,z);\
							WRITE(plotter,temp,strlen(temp))

#define	SCALE(w,x,y,z)		sprintf(temp,"SC%d,%d,%d,%d;",w,x,y,z);\
					WRITE(plotter,temp,strlen(temp))

#define	WINDOW(w,x,y,z)		sprintf(temp,"IW%d,%d,%d,%d;",w,x,y,z);\
					WRITE(plotter,temp,strlen(temp))

#define PEN(x)			sprintf(temp,"PU;SP%d;",x-(x/10)*10);\
					WRITE(plotter,temp,strlen(temp));\
					if (x < 10) { \
						WRITE(plotter,"LT;",3);\
					} else if (x < 20) { \
						sprintf(temp,"LT%d,0.4;",x/10);\
						WRITE(plotter,temp,strlen(temp));\
					} else if (x < 30) { \
						sprintf(temp,"LT%d,2.0;",x/10);\
						WRITE(plotter,temp,strlen(temp));\
					} else if (x < 40) { \
						sprintf(temp,"LT%d,3.0;",x/10);\
						WRITE(plotter,temp,strlen(temp));\
					} else if (x < 50) { \
						sprintf(temp,"LT%d,4.0;",x/10);\
						WRITE(plotter,temp,strlen(temp));\
					} else if (x < 60) { \
						sprintf(temp,"LT%d,5.0;",x/10);\
						WRITE(plotter,temp,strlen(temp));\
					} else { \
						sprintf(temp,"LT%d,6.0;",x/10);\
						WRITE(plotter,temp,strlen(temp));\
					}

#define TEXT(a,str)		sprintf(temp,"DI%f,%f;LB%s\003",\
					cos(a),sin(a),str);\
					WRITE(plotter,temp,strlen(temp))

#define UC(str)			sprintf(temp,"UC%s",str);\
					WRITE(plotter,temp,strlen(temp))

#define	TEXTSIZE(x)		sprintf(temp,"SI%f,%f;",\
					((double)x)*POINT,((double)x)*POINT*1.3);\
					WRITE(plotter,temp,strlen(temp))

#define CP(x,y)			sprintf(temp,"CP%f,%f;",x,y);\
					WRITE(plotter,temp,strlen(temp))

#define LINETYPE(x)		if (x<0) {\
					WRITE(plotter,"LT;",3);\
				} else {\
					sprintf(temp,"LT%d;",x);\
					WRITE(plotter,temp,strlen(temp));\
				}

#define	GOUTOPT(y, x)		WRITE(plotter,"OO;",3);\
					READ(plotter,x,y)

#define GERR(y, x)			WRITE(plotter,"OE;",3);\
					/*printf ("getting errors\n");*/\
					READ(plotter,x,y);\
					/*printf ("errors gotten\n")*/

#define WAIT(x)				ALARM;\
					WRITE(plotter,"OE;",3);\
					/*printf ("waiting\n");*/\
					READ(plotter,x,wait_count);\
					/*printf ("waiting complete\n");*/\
					if (wait_count<0) {\
						logerr("WAIT: can't read\n");\
					} else {\
						x[wait_count] = '\0';\
						logerr("WAIT: got %s\n",x);\
					}

#define	PAPER_FULL		WRITE(plotter,"AF;",3)

#define PAPER_HALF		WRITE(plotter,"AH;",3)

#ifdef CUTTER

#define	ENABLE_CUTTER		WRITE(plotter,"EC",3) 

#endif

#ifdef NOCUTTER

#define	ENABLE_CUTTER		WRITE(plotter,"EC0;",4) /* cutter
							disabled for 
							bgphp1 because 
							it doesn't work
							enable cutter really
							is: "EC;" */
#endif

#define	DISABLE_CUTTER		WRITE(plotter,"EC0;",4)

#define	VS(x)			sprintf(temp,"VS%d;",x);\
					WRITE(plotter,temp,strlen(temp))

#define	MOVETO(x,y)		ALARM;\
					sprintf(temp,"PU;PA%d,%d;",x,y);\
					WRITE(plotter,temp,strlen(temp));\
					if (x>max_down) max_down = x;\
					if (y>max_across) max_across = y

#define DRAWTO(x,y)		ALARM;\
				 	sprintf(temp,"PD;PA%d,%d;",x,y);\
					WRITE(plotter,temp,strlen(temp));\
					if (x>max_down) max_down = x;\
					if (y>max_across) max_across = y

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

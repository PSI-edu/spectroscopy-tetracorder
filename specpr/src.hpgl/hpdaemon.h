#include <stdio.h>
#include <math.h>

/*
 *	@(#)hpdaemon.h	2.18 01/17/96 17:20:30
 */

static char SCCSID[]="@(#)hpdaemon.h	2.18 01/17/96 17:20:30";

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

#define READ(a,b,cerr)  cerr = read(a,b,sizeof b);\
                        if (cerr == -1) {\
                            logerr("READ: (%d) %s\n",cerr,b);\
                           }



#define POINT	((1.0/72.0)*2.54)*0.5		/* 1-point = 1/72 inch */

#define	PLOTOFF			WRITE(plotter,"\033.)\n",3)

#define	SCPTS(w,x,y,z)		sprintf(temp,"IP%d,%d,%d,%d;\n",w,x,y,z);\
							WRITE(plotter,temp,strlen(temp))

#define	SCALE(w,x,y,z)		sprintf(temp,"SC%d,%d,%d,%d;\n",w,x,y,z);\
					WRITE(plotter,temp,strlen(temp))

#define	WINDOW(w,x,y,z)		sprintf(temp,"IW%d,%d,%d,%d;\n",w,x,y,z);\
					WRITE(plotter,temp,strlen(temp))

#define PEN(x)			sprintf(temp,"PU;SP%d;\n",x-(x/10)*10);\
					WRITE(plotter,temp,strlen(temp));\
					if (x < 10) { \
						WRITE(plotter,"LT;\n",3);\
					} else if (x < 20) { \
						sprintf(temp,"LT%d,0.4;\n",x/10);\
						WRITE(plotter,temp,strlen(temp));\
					} else if (x < 30) { \
						sprintf(temp,"LT%d,2.0;\n",x/10);\
						WRITE(plotter,temp,strlen(temp));\
					} else if (x < 40) { \
						sprintf(temp,"LT%d,3.0;\n",x/10);\
						WRITE(plotter,temp,strlen(temp));\
					} else if (x < 50) { \
						sprintf(temp,"LT%d,4.0;\n",x/10);\
						WRITE(plotter,temp,strlen(temp));\
					} else if (x < 60) { \
						sprintf(temp,"LT%d,5.0;\n",x/10);\
						WRITE(plotter,temp,strlen(temp));\
					} else { \
						sprintf(temp,"LT%d,6.0;\n",x/10);\
						WRITE(plotter,temp,strlen(temp));\
					}

#define TEXT(a,str)		sprintf(temp,"DI%f,%f;LB%s\003\n",\
					cos(a),sin(a),str);\
					WRITE(plotter,temp,strlen(temp))

#define UC(str)			sprintf(temp,"UC%s\n",str);\
					WRITE(plotter,temp,strlen(temp))

				/*              (\046 = &, \033 = escape)
					\033%1A           = enter PCL mode
					\033\046a%dP      = print direction (%d = value)
					\033(10U\033(s1P  = symbol set 10U, spacing=proportional
					\033(s19V%s       = height (=19 points), %s= string to print
					\033%%0B          = enter HPGL mode
				 */

#define GC(a,str)		sprintf(temp,"\033%%1A\033\046a%dP\033(10U\033(s1P\033(s19V%s\033%%0B",a,str);\
					WRITE(plotter,temp,strlen(temp))

				/*               (\046 = &, \033 = escape)
					\033%1A           = enter PCL mode
					\033\046a%dP      = print direction (%d = value)
					\033(10U\033(s1P  = symbol set 10U, spacing=proportional
					\033(s%dV         = height (%d in points),
					\033\046a%dV      = relative vertical position
								%d in 1/720 inch units
					\033\046a%dH      = relative horizontal position
								%d in 1/720 inch units
					%s		  %s= string to print
					\033%%0B          = enter HPGL mode
				 */

#define GCS(a,x,v,h,str)	sprintf(temp,"\033%%1A\033\046a%dP\033(10U\033(s1P\033(s%dV\033\046a+%dV\033\046a-%dH%s\033\046a-%dV\033\046a+%dH\033%%0B",a,x,v,h,str,v,h);\
					WRITE(plotter,temp,strlen(temp))

#define	TEXTSIZE(x)		sprintf(temp,"SI%f,%f;\n",\
					((double)x)*POINT,((double)x)*POINT*1.3);\
					WRITE(plotter,temp,strlen(temp))

#define CP(x,y)			sprintf(temp,"CP%f,%f;\n",x,y);\
					WRITE(plotter,temp,strlen(temp))

#define LINETYPE(x)		if (x<0) {\
					WRITE(plotter,"LT;\n",3);\
				} else {\
					sprintf(temp,"LT%d;\n",x);\
					WRITE(plotter,temp,strlen(temp));\
				}



#define	VS(x)			sprintf(temp,"VS%d;\n",x);\
					WRITE(plotter,temp,strlen(temp))

#define	MOVETO(x,y)		sprintf(temp,"PU;PA%d,%d;\n",x,y);\
					WRITE(plotter,temp,strlen(temp));\
					if (x>max_down) max_down = x;\
					if (y>max_across) max_across = y

#define DRAWTO(x,y)		sprintf(temp,"PD;PA%d,%d;\n",x,y);\
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

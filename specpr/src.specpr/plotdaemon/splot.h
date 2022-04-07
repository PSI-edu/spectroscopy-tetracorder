#include <stdio.h>
#include <math.h>
/*
 *	%W% %G% %U%
 */

#ifdef MAIN
static	char	SCCSID[] = "%W% %G% %U%";
#endif

int	plotter;

extern	FILE	*text;
extern	FILE	*vector;
int				copies;
extern	int		max_down;
extern	int		max_across;
char			temp[1024];

#define logfile "/usr/log/vpdaemon"


#define WPL     264        /* bytes per gould line */
#define BPL     WPL*8      /* bits per line */
#define NLINES  8192       /* number of lines in buffer */
#define OFF     4096       /* slop */

char bbuf[NLINES*WPL];  /* raster buffer */
char cbuf[512];         /* character buffer */

struct	vector	{
	short	df;		/*  down  coordinate of from point */
	short	af;		/* across coordinate of from point */
	short	dt;		/*  down  coordinate of  to  point */
	short	at;		/* across coordinate of  to  point */
	short	w;		/*       width of vector	   	   */
};

#define	OFFLINE			1
#define	OUT_OF_PAPER	2
#define BAD_OPEN		3
#define BAD_ARG_COUNT	4
#define BAD_INPUT_FILE	5
#define BAD_STATUS_READ	6

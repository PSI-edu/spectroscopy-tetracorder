#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <stdio.h>

extern  Display *SDisplay;
extern	Window	SWindow;
extern	GC	SGC;

xwrite(x,y,line)
int *x,*y;
char *line;
{
	int i;
	short int c;
	int xhold;

	xhold = *x;
	for(i = 0 ; i < 80 ; i++) {
		c = (short int) *line++;
		if (c == '\0') break;
		xtext(x,y,&c);
	}
	/* send a newline to increment y */
	c = 20;
	xtext(x,y,&c);
	*x = xhold;
	XFlush(SDisplay);
}


xwrite_(x,y,line)
int *x,*y;
char *line;
{
	xwrite(x,y,line);
}

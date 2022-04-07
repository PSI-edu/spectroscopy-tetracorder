#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <stdio.h>

extern  Display *SDisplay;
extern	Window	SWindow;
extern	GC	SGC;
extern	int	SWidth,SHeight;

xdraw(x1,y1,x2,y2)
int *x1,*x2,*y1,*y2;
{
	int y3,y4,x3,x4;

	y3 = SHeight - (*y1 * SHeight / 780);
	y4 = SHeight - (*y2 * SHeight / 780);
	x3 = (*x1) * SWidth / 1024;
	x4 = (*x2) * SWidth / 1024;
	XDrawLine( SDisplay, SWindow, SGC, x3,y3,x4,y4);
}


xdraw_(x1,y1,x2,y2)
int *x1,*x2,*y1,*y2;
{
	xdraw(x1,y1,x2,y2);
}

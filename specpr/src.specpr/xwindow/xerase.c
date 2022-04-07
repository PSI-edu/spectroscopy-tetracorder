#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <stdio.h>

extern  Display *SDisplay;
extern	Window	SWindow;
extern	GC	SGC;
extern	int	SWidth,SHeight;

xerase(x1,y1,x2,y2)
int *x1,*x2,*y1,*y2;
{
	int x,y,width,height;
	int y3,y4,x3,x4;


	y3 = SHeight * (*y1) / 780;
	y4 = SHeight * (*y2) / 780;
	x3 = SWidth * (*x1) / 1024;
	x4 = SWidth * (*x2) /1024;
	y = y3;
	x = x3;
	width = x4-x3;
	height = y3-y4;
	if (x3 > x4) {
		x=x4;
		width = x3-x4;
	}
	if (y3 < y4)  {
		y=y4;
		height = y4-y3;
	}
	y = SHeight - y;
	XClearArea(SDisplay,SWindow,x,y,width,height,True);
}


xerase_(x1,y1,x2,y2)
int *x1,*x2,*y1,*y2;
{
	xerase(x1,y1,x2,y2);
}

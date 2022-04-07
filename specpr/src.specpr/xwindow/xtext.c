#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <stdio.h>

extern  Display *SDisplay;
extern	Window	SWindow;
extern	GC	SGC;
extern	XFontStruct	*SFontStruct;
extern	int	SWidth,SHeight;

/* don't forget, we have to invert y */

xtext(x1,y1,c)
int *x1,*y1;
short int *c;
{
	XTextItem	xtext;
	char text[2];
	int width, height,y2,y,x;

	y = SHeight - ((*y1) * SHeight / 780);

      /*  Below is the code pre 8/16/2011.
       *  Changed to fixed so different sized X-windows
       *  would keep the same font spacing.
       *  However, if user rescales the window size, then
       *     the font spacing will still change
       *            Roger N. Clark  8/16/2011
       *
       *	width = SFontStruct->max_bounds.rbearing - 
       *		SFontStruct->min_bounds.lbearing;
       *	height = SFontStruct->ascent + SFontStruct->descent;
       */

	width = 13;
	height = 20;
	y2 = y - SFontStruct->ascent;

	if (*c == 20) {
		/* this is a newline.  Just add and return */
		*x1 = 1;
		*y1 -= height;
		return(0);
	}

	if (*c == -1) {
		/* this is a backspace */
		*x1 -= width;
		if (*x1 < 1) *x1 = 1;
		x = (*x1) * SWidth / 1024;
		XClearArea(SDisplay, SWindow, x, y2, width, height, False);
		return(0);
	}

/* Clear out this space */

	x = (*x1) * SWidth / 1024;
	XClearArea(SDisplay, SWindow, x, y2, width, height, False);

	text[0]=(char) *c;
	text[1]='\0';

	xtext.chars = text;
	xtext.nchars = 1;
	xtext.delta = 0;
	xtext.font = NULL;

	XDrawText(SDisplay, SWindow, SGC, x, y, &xtext, 1);

	*x1 += width;
}


xtext_(x1,y1,c)
int *x1,*y1;
short int *c;
{
	xtext(x1,y1,c);
}

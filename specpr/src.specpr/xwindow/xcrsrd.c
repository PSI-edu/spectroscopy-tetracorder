#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <stdio.h>

extern  Display *SDisplay;
extern	Window	SWindow;
extern	GC	SGC;
extern	int	SWidth,SHeight;

#define EV_MASK (ButtonPressMask)

xcrsrd(x,y)
int *x,*y;
{
	XEvent	event;

	XSelectInput(SDisplay, SWindow, EV_MASK);
	XNextEvent( SDisplay, &event);

	if (event.xbutton.button == Button3) {
		*x = -1;
		*y = -1;
	} else {
		*x = 	event.xbutton.x * 1024 / SWidth;
		*y = 	780 - (event.xbutton.y * 780 / SHeight);
	}
}


xcrsrd_(x,y)
int *x,*y;
{
	xcrsrd(x,y);
}

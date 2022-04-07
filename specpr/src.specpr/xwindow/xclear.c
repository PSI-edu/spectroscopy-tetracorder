#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <stdio.h>

extern  Display *SDisplay;
extern	Window	SWindow;
extern	int	SWidth,SHeight;

xclear()
{

	XWindowAttributes	windowattributes;

	XGetWindowAttributes(SDisplay, SWindow, &windowattributes);
	SHeight = windowattributes.height;
	SWidth = windowattributes.width;

	XClearWindow(SDisplay, SWindow);
	XFlush(SDisplay);
}


xclear_()
{
	xclear();
}

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <stdio.h>

extern  Display *SDisplay;

xflush()
{
	XFlush(SDisplay);
}


xflush_()
{
	XFlush(SDisplay);
}

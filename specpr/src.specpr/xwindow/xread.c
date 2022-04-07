#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/keysym.h>
#include <stdio.h>

extern  Display *SDisplay;
extern	Window	SWindow;
extern	GC	SGC;

#define EV_MASK (KeyPressMask)

xread(c)
short int *c;
{
	XEvent	event;
	int count;
	char buffer[64];
	int bufsize=65;
	KeySym keysym;
	XComposeStatus compose;
	int length;

	XSelectInput(SDisplay, SWindow, EV_MASK);

	*c = 0;

	while(1) {
		XNextEvent( SDisplay, &event);
		printf("got here 1\n");
		count = XLookupString(&event, buffer, bufsize,
			&keysym, &compose);
		printf("got here 2\n");
		if((keysym == XK_Return || keysym == XK_KP_Enter) ||
			(keysym == XK_Linefeed)) {
				*c = (short int) 10;
				return(0);
		} else  if (((keysym >= XK_KP_Space) 
			&& (keysym <= XK_KP_9))
			|| ((keysym >= XK_space) 
			&& (keysym <= XK_asciitilde))) {
				*c = (short int) buffer[0];
				return(0);
		} else if ((keysym == XK_BackSpace) || (keysym == XK_Delete)) {
				*c = (short int) -1;
				return(0);
/*		} else if (((keysym >= XK_Shift_L) 
			&& (keysym <= XK_Hyper_R)) 
			|| ((keysym >= XK_F1) 
			&& (keysym <= XK_F35))) {
				; */
		}
	}
}


xread_(c)
short int *c;
{
	xread(c);
}

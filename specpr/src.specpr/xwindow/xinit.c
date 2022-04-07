#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <stdio.h>

#define BORDER_WIDTH	2
#define FONT1	"6x10"
#define FONT2	"8x13"
#define FONT3	"9x15"

Display 	*SDisplay;
int		SScreen,SDepth;
Window		SWindow;
GC		SGC;
XFontStruct	*SFontStruct;
int		SWidth,SHeight;
int blackColor;
int whiteColor;
Colormap cmap;
XColor red, green, blue, orange, cyan, magenta, purple, brown, gray;

initx()
{

	SDisplay = XOpenDisplay(NULL);
	if (SDisplay == NULL) {
		fprintf(stderr, "Cannot make connection to the X server %s\n",
			XDisplayName(NULL));
		return(0);
	}

	SScreen = DefaultScreen( SDisplay );
	SDepth = DefaultDepth( SDisplay, SScreen );
	return(1);
}

Window
openwindow(x,y,width,height,flag,gc)
int x,y,width,height,flag;
GC *gc;
{

	XSetWindowAttributes	WindowAttributes;
	XSizeHints		SizeHints;
	unsigned long		WindowMask;
	Window			window;

	WindowAttributes.border_pixel = BlackPixel(SDisplay,SScreen);
	WindowAttributes.background_pixel = WhitePixel(SDisplay,SScreen);
	WindowAttributes.override_redirect = False;

	/* original: (comment this out and add the next 2 lines)
	WindowMask = CWBackPixel | CWBorderPixel */ /* | CWOverrideRedirect */;

	/* the following added to do backing store,
			from Brad Dalton, 11/27/2001 */
	WindowMask = CWBackPixel | CWBorderPixel | CWBackingStore;
	WindowAttributes.backing_store = True;

	window = XCreateWindow(SDisplay, RootWindow(SDisplay, SScreen), 
		x, y, width, height, BORDER_WIDTH, SDepth, 
		InputOutput, CopyFromParent, WindowMask, &WindowAttributes);

	SizeHints.flags 	= PPosition | PSize;
	SizeHints.x		= x;
	SizeHints.y		= y;
	SizeHints.width 	= width;
	SizeHints.height	= height;
	
	XSetNormalHints( SDisplay, window, &SizeHints);

	if (createGC( window, gc) == 0) {
		XDestroyWindow(SDisplay, SWindow);
		return((Window) 0);
	}
		
	XMapWindow( SDisplay, window);	
	XFlush(SDisplay);
	return(window);
	SWidth = x;
	SHeight = y;
}

createGC( window, gc)
Window window;
GC *gc;    
{
	XGCValues	xgcvalues;
		
	*gc = XCreateGC( SDisplay, window, (unsigned long) 0, &xgcvalues);
	if (*gc == 0) {
		return(0);
	} else {
		XSetForeground( SDisplay, *gc, BlackPixel(SDisplay, SScreen));
		XSetBackground( SDisplay, *gc, WhitePixel(SDisplay, SScreen));

		blackColor = BlackPixel(SDisplay, DefaultScreen(SDisplay));
		whiteColor = WhitePixel(SDisplay, DefaultScreen(SDisplay));
		cmap = DefaultColormap( SDisplay, DefaultScreen(SDisplay));

		XAllocNamedColor( SDisplay, cmap, "gray",    &gray,    &gray);    /*  1 */
		XAllocNamedColor( SDisplay, cmap, "red",     &red,     &red);     /*  2 */
		XAllocNamedColor( SDisplay, cmap, "blue",    &blue,    &blue);    /*  3 */
		XAllocNamedColor( SDisplay, cmap, "green",   &green,   &green);   /*  4 */
		XAllocNamedColor( SDisplay, cmap, "orange",  &orange,  &orange);  /*  5 */
		XAllocNamedColor( SDisplay, cmap, "cyan",    &cyan,    &cyan);    /*  6 */
		XAllocNamedColor( SDisplay, cmap, "magenta", &magenta, &magenta); /*  7 */
		XAllocNamedColor( SDisplay, cmap, "purple",  &purple,  &purple);  /*  8 */
		XAllocNamedColor( SDisplay, cmap, "brown",   &brown,   &brown);   /*  9 */

		return(1);
	}
}

xgetfont()
{
	int fontheight, fontwidth;
	SFontStruct = XLoadQueryFont(SDisplay, FONT1);
	if (SFontStruct == NULL) {
		fprintf(stderr,"Cannot load %s font\n", FONT1);
		SFontStruct= XLoadQueryFont(SDisplay, FONT2);
		if (SFontStruct == NULL) {
			fprintf(stderr,"Cannot load %s font\n",FONT2);	
			SFontStruct= XLoadQueryFont(SDisplay, FONT3);
			if (SFontStruct == NULL) {
				fprintf(stderr,"Cannot load %s font\n",FONT3);	
				fprintf(stderr,"Giving up\n");
				return(0);
			}
		}
		fprintf(stderr,"Had to load larger font.");
		fprintf(stderr,"  Things might get ugly\n");
	}
	return(1);
}

xinit(igrmod, i)
int *igrmod;
int *i;
{
	*i = 1;
	/* printf ("DEBUG: in xinit.c\n");
	 * printf ("DEBUG: igrmod in xinit.c= %d\n", *igrmod);
         */
	if(initx() == 0) {
		fprintf(stderr,"aborting.\n");
		*i = 0;
		return(0);
	}
	if (SWindow == NULL) {
		if (*igrmod == 52 ) {     /* xterm double size */

			SWindow = openwindow(900,600,1024,780,0,&SGC);
				/* x,y,width,height,popup */

		} else if (*igrmod == 53 ) {  /* xterm triple size */
		
			SWindow = openwindow(1350,900,1536,1170,0,&SGC);

				/* x,y,width,height,popup */
		
		} else {                       /* xterm, hpterm standard (original) size */

			SWindow = openwindow(450,300,512,390,0,&SGC); 
				/* x,y,width,height,popup */
		
		}
		if (SWindow == (Window) 0) {
			fprintf(stderr,"Can't open window.\n");
			*i =0;
			return(0);
		}
	}
	if (xgetfont() == 0) {
		*i = 0;
		return(0);
	}
	XSetFont(SDisplay, SGC, SFontStruct->fid);
	sleep(1);
}


xinit_(igrmod, i)
int *igrmod;
int *i;
{
	xinit(igrmod, i);
}

xset_color(ixcolor)
int *ixcolor;
{
    int j=*ixcolor;
	/* printf ("DEBUG: xset_color = %d\n",j); */
    if (*ixcolor == 0) {
        XSetForeground(SDisplay, SGC, blackColor);
    }
    if (*ixcolor == 10) {
        XSetForeground(SDisplay, SGC, whiteColor);
    }
    if (*ixcolor == 1) {
        XSetForeground(SDisplay, SGC, gray.pixel);
    }
    if (*ixcolor == 2) {
        XSetForeground(SDisplay, SGC, red.pixel);
    }
    if (*ixcolor == 3) {
        XSetForeground(SDisplay, SGC, blue.pixel);
    }
    if (*ixcolor == 4) {
        XSetForeground(SDisplay, SGC, green.pixel);
    }
    if (*ixcolor == 5) {
        XSetForeground(SDisplay, SGC, orange.pixel);
    }
    if (*ixcolor == 6) {
        XSetForeground(SDisplay, SGC, cyan.pixel);
    }
    if (*ixcolor == 7) {
        XSetForeground(SDisplay, SGC, magenta.pixel);
    }
    if (*ixcolor == 8) {
        XSetForeground(SDisplay, SGC, purple.pixel);
    }
    if (*ixcolor == 9) {
        XSetForeground(SDisplay, SGC, brown.pixel);
    }
}
xset_color_(ixcolor)
int *ixcolor;
{
	xset_color(ixcolor);
}

/*
 *    http://www.ddj.com/cpp/184402365?pgno=4
 *
 *      xlib1.c
 *      An X Window (Xlib)-based program
 *      written for the C Users Journal.
 *
 *      Link with the X library, e.g.,
 *
 *              cc -o xlib1 xlib1.c -lX11
 *
 *      Define SYSV if you have malloc()
 *      declared in stdlib.h.
 *
 */

#include <stdio.h>

#ifdef SYSV
#include <stdlib.h>
#endif

#include <X11/Xlib.h>
#include <X11/Xutil.h>

/*
 *      We have a hard-coded size and location
 */
#define X_LOCATION      10
#define Y_LOCATION      20
#define WIDTH           200
#define HEIGHT          200


main( argc, argv )

int     argc;
char    *argv[];

{      /* main */
      Display        *display;
      int            screen;
      Window         rootwindow;
      Window         window;
      XSizeHints     *sizehints;
      GC             gc;
      XEvent         event;
      int            done;

      /*
       * Connect to an X server.
       */
      display = XOpenDisplay( (char *) NULL );

      if ( display == (Display *) NULL )
             {
             fprintf( stderr, "Error opeing in display\n" );
             exit( 1 );
             }
      screen     = DefaultScreen( display );
      rootwindow = RootWindow( display, screen );

      /*
       * Create a window
       */
      window = XCreateSimpleWindow( display,
                    rootwindow,    /* parent */
                    X_LOCATION, Y_LOCATION,
                    WIDTH, HEIGHT,
                    1,             /* border width */
                    BlackPixel( display, screen ),
                    WhitePixel( display, screen ) );

      /*
       * Set up hints about the window
       */
      sizehints = (XSizeHints *) malloc(sizeof(XSizeHints) );

      sizehints->x      = X_LOCATION;
      sizehints->y      = Y_LOCATION;
      sizehints->width  = WIDTH;
      sizehints->height = HEIGHT;
      sizehints->flags  = PPosition | PSize;

      /*
       * Use XSetWMProperties() in R4
       *
       */
      XSetStandardProperties( display, window,
             "Xlib1",        /* window name */
             "Xlib1",        /* icon name */
             (Pixmap) None,  /* Icon pixmap */
             argv, argc,
             sizehints );

free( sizehints );

      /*
       * Ask for Expose and mouse (Button) input
       */
      XSelectInput( display, window,
             ButtonPressMask | ExposureMask );

      /*
       * Create a graphics
       * context to draw with.
       */
      gc = XCreateGC( display, window,
             OL, (XGCValues *) NULL );

      XSetForeground( display, gc,
             BlackPixel( display, screen ) );

      /*
       * Make Window appear on the screen
       */
      XMapWindow( display, window );
      XFlush( display );

      done = False;

      while( !done )
             {
             XNextEvent( display, &event );

             if ( event.type == ButtonPress )
                    {
                    XCloseDisplay( display );

                    exit( 0 );
                    }

             if ( event.type == Expose )
                    {
                    /*
                     * Only redraw when all
                     * Expose events are in
                     */
                    if ( event.xexpose.count == 0 )
                           {
                           Redraw( display, window, gc,
                                  0, 0,
                                  WIDTH, HEIGHT );
                           }
                    }
             }
}       /* main */

Redraw( display, window, gc, x, y, width, height )

Display *display;
Window  window;
GC      gc;
int     x, y;
int     width, height;

{       /* Redraw */
      int      i, x1, y1;

      if ( ( width == 0 ) || ( height == 0 ) )
             {
             return(0);
             }

      x1 = x;
      y1 = y;

      for( i = 0; i < 10; i++ )
             {
             x1 += width / 10;

             XDrawLine( display, window, gc,
                    x1, y + 5,
                    x1, y + 10 );

             y1 += height / 10;

             XDrawLine( display, window, gc,
                    x + 5, y1,
                    x + 10, y1 );
             }

      for( i = 0; i < 10; i++ )
             {
             x += width / 10;
             y += height / 10;

             XFillRectangle( display, window, gc,
                    x, y,
                    6, 6 );
             }

      XFlush( display );

}       /* Redraw */

/* end of file */

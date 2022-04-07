#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <stdio.h>


extern  Display *SDisplay;
extern	Window	SWindow;
extern	GC	SGC;
extern	int	SWidth,SHeight;

xsetline(i)
int *i;
{

	static int lines_length[] = {
		0,0,0,0,4,2,2,2,4,6,6
	};


	int line=*i;
	unsigned int width = 0;
	int line_style = LineSolid;
	int cap_style = CapButt;
	int join_style = JoinMiter;
	int dash_offset = 0;

	static unsigned char lines4[] = { 3,4,3,8 };
	static unsigned char lines5[] = { 14,4 };
	static unsigned char lines6[] = { 6,6 };
	static unsigned char lines7[] = { 2,2 };
	static unsigned char lines8[] = { 10,3,2,3 };
	static unsigned char lines9[] = { 5,3,2,3,2,3 };
	static unsigned char lines10[] = { 8,3,3,3,3,3 };

	static unsigned char *lines[] = {
		0,0,0,0,
		lines4,lines5,lines6,lines7,lines8,lines9,lines10
	};

	if (line > 3) {
		line_style=LineOnOffDash;
		XSetDashes(SDisplay, SGC, dash_offset, lines[line],
			lines_length[line]);
	}
	XSetLineAttributes(SDisplay,SGC,width,line_style,
		cap_style, join_style);

}


xsetline_(i)
int *i;
{
	xsetline(i);
}

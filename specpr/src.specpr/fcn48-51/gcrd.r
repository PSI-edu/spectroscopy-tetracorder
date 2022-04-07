	subroutine gcrd (nchan,ydata,xdata,ixx,xpos,imatch,iopcon,irtn)
	implicit integer*4(i-n)

#ccc  version date: 16-Dec-88
#ccc  author(s): roger clark; modified by wendy calvin for function 50
#ccc  language:  ratfor
#ccc
#ccc  short description:
#ccc            this subroutine reads the graphics cursor position
#ccc            and prints the x, y, error data values
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc  argument list description:
#ccc     argumrnts: none
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  notes:
#ccc

	include "../common/lbl3"
	include "../common/hptrm"
	include "../common/lundefs"
	include "../common/alphabet"
	include "../common/dscrch"

	common /plot1/xmax, xmin, lbnd, diff

	real*4 xdata(4864), ydata(4864), lbnd
	equivalence (xdata(1),datsc5(1))
	equivalence (ydata(1),datsc6(1))

	character*80 atemp, iopcon
	character*1 cntrlq
	character*1 escape

	axl = 56.
	axh = 500.
	ayl = 46.
	ayh = 276.
#
#     determine constants to scale data
#
	if (diff == 0.) diff = 0.1e-36
	dy = (ayh-ayl)/diff
	an = xmax - xmin
	if (an <= 0) an = 0.1e-36
	dx = (axh-axl)/an
#
	cntrlq = char(17)
	escape = char(27)
	atemp = iopcon

1	read (ttyin,2,end=2000,err=2000) iopcon
2	format (a)
	i = 1
	call wjfren (i,x,il)
	if (il == ihe || il == ihx) {
		irtn = il
		return
	}

# send escape sequence to get cursor position

	call cursrd(ixx,iyy)
	x = float (ixx)
	y = float(iyy)

	if (y > ayh || y < ayl || x > axh || x < axl) {
		call serase(0,310,511,348)
		call movabs (0,338)
		call sb(0)
		write (ttyout, 30) ixx,iyy
30		format (20x,'OUT OF BOUNDS ('i4,','i4')')
		call movabs (ixx, iyy)
		go to 1
	}
#
# calculate x and y postion in data space
#
	xpos = (x-axl)/dx + xmin
	ypos = (y-ayl)/dy + lbnd

	imatch =0
	xmatch = 0.9e37
	do jj = 1, nchan {  # find closest channel
		tstmch = abs(xpos - xdata(jj))
		if (tstmch < xmatch) {
			imatch = jj
			xmatch = tstmch
		}
	}
	if (imatch == 0) {
		call serase(0,310,511,348)
		call movabs (0,338)
		call sb(0)
		write (ttyout, 40) ixx,iyy
40		format (12x,'CANNOT FIND A CLOSE CHANNEL ('i4,','i4')')
		call movabs (ixx, iyy)
		go to 1
	}
	call serase(0,310,511,348)
	call movabs (0,338)
	call sb(0)
	write (ttyout, 50) ixx,iyy,xpos,ypos,imatch,
				xdata(imatch),ydata(imatch)
50	format ('pixel coordinates:       x=',i4,'     y=',i4,/,
		'data coordinates:        x=',1pe13.6,'     y=',1pe13.6,/,
		'closest channel=',i6,'   x=',1pe13.6,'     y=',1pe13.6)
	call movabs (ixx,iyy)
	call sb(0)


2000	iopcon = atemp
	return
	end

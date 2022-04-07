	subroutine gcrpos (ixx,iyy,xpos,ypos,xmax,xmin,lbnd,diff,iopcon,ier)
	implicit integer*4(i-n)

#ccc  version date: 07/01/86
#ccc  author(s): roger clark
#ccc  language:  ratfor
#ccc
#ccc  short description:
#ccc            this subroutine reads the graphics cursor position
#ccc            and returns the x and y screen and scaled data values.
#ccc		iix,iiy = screen space -- xpos, ypos = data space
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

	include "../common/spmaxes"   # max parameters, must be first

include "../common/lbl3"
include "../common/hptrm"
include "../common/lundefs"
include "../common/alphabet"

	character*80 atemp, iopcon
	real*4 lbnd

#
# clear error variable.  its set for gcchan to use if necessary
#
	ier = 0

#	axl = 56.   # original standard size
#	axh = 500.
#	ayl = 46.
#	ayh = 276.

	axl = 112.   # 2x size
	axh = 1000.
	ayl = 92.
	ayh = 552.

	atemp = iopcon

#
#     determine constants to scale data
#
	if (diff == 0.) diff = 0.1e-36
	dy = (ayh-ayl)/diff
	an = xmax - xmin
	if (an <= 0) an = 0.1e-36
	dx = (axh-axl)/an

#
# this read is to 1) check for e or x, and 2) allow the user to position
# the graphics cursor, and then hit return to enter the spot
#
#
# flush graphics
#
	call sb(0)

1	if (igrmod < 50 | igrmod > 53) {   # not x-windows
		read (ttyin,2,end=2000,err=2000) iopcon
2		format (a)
		i = 1
		call wjfren (i,x,il)
		if (il == ihe || il == ihx) {
			iopcon = atemp
			ier = il
			return
		}
	}
	
#
# get cursor position
#

	call cursrd(ixx,iyy)

	if (ixx == -1 && iyy == -1) {
		ier = ihe
		return
	}

	x = float (ixx)
	y = float(iyy)

	if (y > ayh || y < ayl || x > axh || x < axl) {
		#call serase(0,318,511,348)
		#call movabs (0,338)
		call serase(0,636,1022,696)
		call movabs (0,676)
		call sb(0)
		write (ttyout, 30) ixx,iyy
		call movabs (ixx, iyy)
		go to 1
	}
#
# calculate x and y postion in data space
#
	xpos = (x-axl)/dx + xmin
	ypos = (y-ayl)/dy + lbnd


%20	format (a,'*s3^',a,$)
# RED Added commas just before each i6 and after last i6 in 30 format statement 
30	format (20x,'OUT OF BOUNDS (',i6,',',i6,')')

2000	iopcon = atemp
	return
	end

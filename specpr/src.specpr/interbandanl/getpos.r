subroutine getpos(ixx,iyy,imatch,nchans,xmax,xmin,lbnd,diff,xdata,ydata,iopcon)

#ccc  name:
#ccc  version date:
#ccc  author(s): R. Clark and M. Klejwa
#ccc  language:
#ccc
#ccc  short description:
#ccc
#
# This subroutine is bascally a copy of gcrpos it returns 
# information about a selected point
# except that it returns the channel of the nearest non deleted
# point
#ccc  algorithm description:
#ccc  system requirements:
#ccc  subroutines called:
#ccc  argument list description:
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc

	include "../common/spmaxes"   # max parameters, must be first

	implicit integer*4 (i-n)

	include "../common/lbl3"
	include "../common/hptrm"
	include "../common/lundefs"
	include "../common/alphabet"
	include "../common/cmd"

	real*4 xdata(SPMAXCHAN), ydata(SPMAXCHAN), lbnd


	character*80 atemp, iopcon
	character*1 cntrlq
	character*1 escape

# set defaults in case routine is exited
	ixx=0
	iyy=0
	imatch=1

# flush graphics buffer
	call sb(0)

	axl = 56.*2.0   # new default graphics area 08/16/2011
	axh = 500.*2.0
	ayl = 46.*2.0
	ayh = 276.*2.0
#
#     determine constants to scale data
#
	if (diff == .1e-14) diff = 0.1e-14
	dy = (ayh-ayl)/diff
	an = xmax - xmin
	if (an <= .1e-14) an = 0.1e-14
	dx = (axh-axl)/an
#
	cntrlq = char(17)
	escape = char(27)
	atemp = iopcon

1	read (ttyin,2,end=2000,err=2000) iopcon
	cndx=0
	i = 1
	call wjfren (i,x,il)
	if (il == ihe || il == ihx) {
		iopcon = atemp
		call movabs (150*2,327*2)
		call sb(0)
		write (ttyout, 10)
		return
	}

# send escape sequence to get cursor position

	write (ttyout,20) escape, cntrlq
	read (ttyin,2,end=2000,err=2000) iopcon
	cndx=0
2	format (a)

	i =2
	call wjfren (i,x,il)
	i=i+1
	call wjfren (i,y,il)

	y = y+10   # the carriage return subtracted 10 from y

	if (x <   0.) x =   0.
	if (x > 512.*2.0) x = 512.*2.0
	if (y <   0.) y =   0.
	if (y > 359.*2.0) y = 359.*2.0

	ixx = x
	iyy = y

	if (y > ayh || y < ayl || x > axh || x < axl) {
		call movabs (0,338*2)
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

	imatch =0
	xmatch = 0.9e37
	do jj = 1, nchans {  # find closest channel
		tstmch = abs(xpos - xdata(jj))
	        if (ydata(jj)!=-1.23e34) {	
			if (tstmch < xmatch) {
				imatch = jj
				xmatch = tstmch
			}
		}
	}
	if (imatch == 0) {
		call movabs (0,338*2)
		call sb(0)
		write (ttyout, 40) ixx,iyy
		call movabs (ixx, iyy)
		go to 1
	}
	call movabs (ixx,iyy)
	call sb(0)

10	format ('                                ')
%20	format (a,'*s3^',a,$)
30	format (79(' '),/,20(' '),'OUT OF BOUNDS ('i4,','i4')',
			35(' '),/,79(' '))
40	format (79(' '),/,12(' '),'CANNOT FIND A CLOSE CHANNEL (',
              i4,','i4')', 20(' '),/,79(' '))

2000	iopcon = atemp
	return
	end


	subroutine gcchan(ixx,iyy,xpos,ypos,imatch,nchans,xdata,ydata,ier)
	implicit integer*4(i-n)

#ccc  version date: 07/01/86
#ccc  author(s): roger clark
#ccc  language:  ratfor
#ccc
#ccc  short description:
#ccc            this subroutine finds the closest channel to x and y, and
#ccc            prints the data and error values.
#ccc            Used under the graphics cursor read routines.
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

	character*80 outline	# for xwindows writes
	real*4 xdata(SPMAXCHAN), ydata(SPMAXCHAN)
#
#  This means that gcrpos didn't exit normally, or the user quit
#
	if (ixx == -1 && iyy == -1) ier = ihe
	if (ier == ihe || ier == ihx || ier == -1) return

	imatch =0
	xmatch = 0.9e37
	do jj = 1, nchans {  # find closest channel
		tstmch = abs(xpos - xdata(jj))
		if (tstmch < xmatch) {
			imatch = jj
			xmatch = tstmch
		}
	}
	if (imatch == 0) {
		call serase(0,636,1022,696)
		call movabs (0,676)
		call sb(0)
		write (ttyout, 40) ixx,iyy
		call movabs (ixx, iyy)
		ier = -1
		return
	}
	call serase(0,636,1022,696)
	call movabs (0,676)
	call sb(0)
	write (outline, 50) ixx,iyy,char(0)
	call gwrite(outline)
	write (outline, 51) xpos,ypos,char(0)
	call gwrite(outline)
	write (outline, 52)  imatch,xdata(imatch),ydata(imatch),char(0)
	call gwrite(outline)
	call movabs (ixx,iyy)
	call sb(0)

#RED Added commas just before each i4 and after last i4 in 40 format statement
40	format(12x,'CANNOT FIND A CLOSE CHANNEL (',i4,',',i4,')')
50	format('pixel coordinates:       x=',i4,'     y=',i4,a1)
51	format('data coordinates:        x=',1pe13.6,'     y=',1pe13.6,a1)
52	format('closest channel=',i6,'   x=',1pe13.6,'     y=',1pe13.6,a1)

	return
	end

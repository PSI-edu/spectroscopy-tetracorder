	subroutine bcchan(ixx,iyy,xpos,ypos,imatch,nchans,xdata,ydata,ier)
	implicit integer*4(i-n)

#ccc  version date: 07/01/86
#ccc  author(s): roger clark
#ccc  language:  ratfor
#ccc
#ccc  short description:
#ccc		SAME as crtp/gcchan except, normally no output.
#ccc            this subroutine finds the closest channel to x and y
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

	real*4 xdata(SPMAXCHAN), ydata(SPMAXCHAN)
#
#  This means that gcrpos didn't exit normally
#
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
		call serase(0,318*2,511*2,348*2)
		call movabs (0,338*2)
		call sb(0)
		write (ttyout, 40) ixx,iyy
		call movabs (ixx, iyy)
		ier = -1
		return
	}
	call serase(0,318*2,511*2,348*2)
	call movabs (0,338*2)
	call sb(0)
	call sb(0)

40	format (12x,'CANNOT FIND A CLOSE CHANNEL (',i4,',',i4,')')
	return
	end

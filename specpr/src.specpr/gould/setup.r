	subroutine setup (errlun,plotfi,disp,height,width,error)
#####################################################################
#
#       Gould Plot setup routine
#
#       Arguments:
#
#       errlun:         Logical unit to which plotting error messages
#                       will go.
#       plotfi:       name of the plotfi.
#       disp:           disposition of the plotfi. 'O' means
#                       replot an existing file. anything else
#                       will cause a new file to be created and
#                       any existing file to be replaced.
#       height:         the vertical dimension of the plot in cm.
#                       0<=height<=200.
#       width:          the horizontal dimension of the plot in cm.
#                       0<=width<=27.
#
#####################################################################
#
	integer*4		error
	integer*2		errlun

	character*2	disp
	character*(*)	plotfi
	real*4            height,width

	include "fdefs.h"
	include "../common/plot01"
	include "../common/plot02"
#RED
	integer*4 iopenp      # function iopenp

	lunmsg = errlun

    # yes it is in fdefs.h above FACTOR = 1.0  # not initialized why? RNC 2/25/93

	if ( (height <= 0.) ||
			(height > 200.) ||
			(width <= 0.) ||
			(width > 27.) ) {

		write (lunmsg,10) width, height
		error = 1
		return
	}


	amax = width;   dmax = height
	amin = 0;       dmin = 0
	ascale = 1.;    dscale = 1.
	oascle = 1.;    odscle = 1.;
	pwidth = width * factor
	pheigh = width * factor


	vplot = plotfi
	tplot = plotfi

	vlun = 1
	tlun = 2
	error = iopenp(vlun,vplot,'v',disp)
	error = error + iopenp(tlun,tplot,'t',disp)

	return

10	format (' ***** plot size not in range, 0<width<=27.',
			' 0<height<=200. width = ',g9.4,' height = ',
			g9.4,/)
	end

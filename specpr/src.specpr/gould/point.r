	subroutine point(down, across, width)
	implicit integer*4 (i-n)
#ccc    name:
#ccc    version date:
#ccc    author(s):
#ccc    language:
#ccc
#ccc    short description:
#ccc       this routine plots a square, 'width' pixels wide at
#ccc       location 'down,across'
#ccc
#ccc    algorithm description:
#ccc    system requirements:
#ccc    subroutines called:
#ccc    argument list description:
#ccc    parameter description:
#ccc    common description:
#ccc    message files referenced:
#ccc    internal variables:
#ccc    file description:
#ccc    user command lines:
#ccc    update information:
#ccc    NOTES:
#ccc
	integer*4 width
	real*4 down, across


	include "fdefs.h"
	include "../common/plot01"
	include "../common/plot02"

	i = width
	call tocent(down,across,d,a,i)

	if (d<dmin || d>dmax || a<amin || a>amax) {
		write(lunmsg,100) down,across
100		format(' ***** the point (',1pg11.4,',',1pg11.4,') is ',
			'outside the plot.')
		return
	}

	dmid = d + float(iabs(width))/factor/2.
	acmax = a + float(iabs(width))/factor
	if (a < 0.) a = 0.

	call line(dmid,a,dmid,acmax,-iabs(width))

	return
	end

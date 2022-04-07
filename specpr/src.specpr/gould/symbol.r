	subroutine symbol(down,across,height,string,angle)
	implicit integer*4 (i-n)
	integer*4 height
	character*(*) string
########################################################################
#       this routine prints string on the plot, starting at the position
#       (down,across). iabs(height) determines the character size used.
#       if height<0 then down and across are in centimeters. otherwise
#       they are in the users units.
#       angle is the angle at which the characters are written. only
#       0. deg. or 90. deg. are allowed.
########################################################################

	include "fdefs.h"
	include "../common/plot01"
	include "../common/plot02"

	i = height
	call tocent(down,across,d,a,i)

	nd = d * factor + .5
	na = a * factor + .5

	write (tlun,200) nd,na,iabs(height),int(angle-45.0)/90,string
200	format('T',4i6,1x,a)

	return
	end

	subroutine lowp(x,ilast,inext,z)
	implicit integer*4 (i-n)
	real*4 x(inext),l
	integer*4 ilast,z,inext

#ccc  name: lowp
#ccc  version date: May 27, 1987
#ccc  author(s): Noel Gorelick
#ccc  language: ratfor
#ccc
#ccc  short description: Finds lowest value between ilast and inext
#ccc			on datar-array
#ccc
#ccc  algorithm description:
#ccc  system requirements:
#ccc  subroutines called:
#ccc  argument list description: x == datar-array
#ccc				 ilast == start value
#ccc				 inext == end value
#ccc				 z == output channel value
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc
#	initialize
	j=0
	l=1.0e25
	do j=ilast,inext {
		if (x(j)==-1.23e34) next
		if (x(j)<l) {
			l=x(j)
			z=j
		}
	}
	return
	end

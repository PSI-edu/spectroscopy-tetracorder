	subroutine pfind(x,number,ilast,inext)
	implicit integer*4 (i-n)
	integer*4 x(number)
	integer*4 ilast,inext,j,number

#ccc  name: pfind
#ccc  version date: May 27, 1987
#ccc  author(s): Noel Gorelick
#ccc  language: ratfor
#ccc
#ccc  short description: finds two idat=1 peaks
#ccc
#ccc  algorithm description:  x == idat array
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
#	initialize
	do i=ilast+1,number-1 {
		if (x(i)==1) {
			ilast=i
		} else {
			break
		}
	}
1	j=ilast+1
51	if (x(j)==1) {
		inext=j
	} else {
		if (j<number) {
			j=j+1
			go to 51
		} else {
			inext=j
		}
	}
	if (inext-ilast<2) {
		if (j<number) {
			j=j+1
			go to 51
		} else {
			inext=j
		}
	}
##	start=ilast+1
#	do j=start,number-1 {
#		if (x(j)==1) {
#			ilast=j
#		} else {
#			do k=j,number-1 {
#				if (x(k)==1) {
#					if (k>j+1) {
#						inext=k
#						return
#					} else {
#						ilast=k
#						break
#					}
#				}
#			}
#			inext = number
#			return
#		}
#	}
#	inext=number
	return

	end

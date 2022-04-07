	real*4 function round(value)
	implicit integer*4 (i-n)
#ccc    name:
#ccc    version date:
#ccc    author(s):
#ccc    language:
#ccc
#ccc    short description:
#ccc            converts value to 1, 2, or 5 times 10**n
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


	character*8 dummy

	write(dummy,1) value
	read(dummy,5)isign,ibase,iexp

	if (ibase>=6) {
		ibase=1
		iexp=iexp+1
	}
	else if (ibase>2) ibase=5

	write(dummy,5)isign,ibase,iexp
	read(dummy,1)temp

	round = temp
	return

1	format(1pe8.0)
5	format(1x,a,i1,'.e',i3)
	end

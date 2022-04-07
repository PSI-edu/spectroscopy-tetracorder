	subroutine line(fromd,froma,tod,toa,width)
	implicit integer*4 (i-n)
#ccc    name:
#ccc    version date:
#ccc    author(s):
#ccc    language:
#ccc
#ccc    short description:
#ccc     this routine draws a line from (fromd,froma) to
#ccc     (tod,toa) of width abs(width). if 'width' < 0 then
#ccc     fromd,froma,tod,toa are measured in cemtimeters.
#ccc     if 'width' > 0 then they are in the user coordinates
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
	logical flag

	include "fdefs.h"
	include "../common/plot01"
	include "../common/plot02"

# the following equivalence method forces integer*2 to take up only
# 2 bytes for HP-UX.  Should be compatable with all machines

	character*10 ichdum
	integer*2 infd, infa, intd, inta, iwidth
	equivalence (infd, ichdum(1:1)), (infa,ichdum(3:3))
	equivalence (intd, ichdum(5:5)), (inta,ichdum(7:7))
	equivalence (iwidth, ichdum(9:9))

#

	inc = width
	call tocent(fromd,froma,fd,fa,inc)
	inc = width
	call tocent(tod,toa,td,ta,inc)
	nfd = fd * 78.74015748 + .5
	nfa = fa * 78.74015748 + .5
	ntd = td * 78.74015748 + .5
	nta = ta * 78.74015748 + .5

	infd = nfd
	infa = nfa
	intd = ntd
	inta = nta
	iwidth = inc

	write(vlun,rec=vecrec, iostat= ier) ichdum
	vecrec=vecrec+1
	return
	end

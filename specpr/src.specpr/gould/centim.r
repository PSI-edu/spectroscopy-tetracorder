	real*4 function centim(j)
	implicit integer*4 (i-n)

#ccc    name: centim
#ccc    version date: %W% %U% %G%
#ccc    author(s): Jeff Hoover
#ccc    language: RATFOR
#ccc
#ccc    short description: 
#ccc		Determines the number of characters in j characters
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

	include "../common/spmaxes"   # max parameters, must be first
 
	include "../common/pltcnt"

	if (penplt == 0) {
		centim = 0.4064 * float(j)
		return
	}
	else {
		centim = 0.5 * float(j)
		return
	}

	end

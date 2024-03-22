	subroutine cxband
	implicit integer*4 (i-n)

	include "../../src.specpr/common/spmaxes"   # max parameters, must be first


# compute band depths in input comparison spectrum

	include "defs.h"
	include "convh.h"
	include "lmrefl.h"

	do i = 1, nminer {

		do j = 1, naband(i) {

			if (xcontr(i,j) < 0.1e-6) {
				write (6,"('ERROR xcontr too small')")
				xcontr(i,j) = 0.1e-6
			}
#check the following to see if correct
			xbandd(i,j) = 1.0 - xi(ibandn(i,j,2))/xcontr(i,j)

		}
	}

	return
	end

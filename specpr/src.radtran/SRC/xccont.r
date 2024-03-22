	subroutine xccont
	implicit integer*4 (i-n)

	include "../../src.specpr/common/spmaxes"

# compute continuum reflectance for each defined band in comparison spectrum

	include "defs.h"
	include "convh.h"
	include "lmrefl.h"

	do i = 1, nminer {

		do j = 1, naband(i) {

			xcontr(i,j) = (xi(ibandn(i,j,1)) +
						xi(ibandn(i,j,3)))/2.0

		}
	}

	return
	end

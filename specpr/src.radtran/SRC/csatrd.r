	subroutine csatrd

	implicit integer*4 (i-n)


	include "../../src.specpr/common/spmaxes"   # max parameters, must be first

# compute saturation points in terms of d for each mineral

	include "defs.h"
	include "convh.h"
	include "lmrefl.h"

	real*4 dinit(9)
	integer*4 ich(3)

	do i = 1,nminer {
		dinit(i) = d(i)
	}

	bdlast = 0.0

	do i = 1,nminer {
		do j = 1,naband(i) {

			d(i) = 1.0/(xk(i,ibandn(i,j,2)) * 1000.0)
			dlast = 0.0
			bdlast = 0.0
			xx = 1.

			do kk = 1, 3 {
				ich(kk) = ibandn(i,j,kk)
			}
10			call bdrefl(i,ich,bdpth)

			if (bdpth > bdlast) {
				bdlast = bdpth
				bdlst2 = bdlast
				dlast = d(i)
				dlast2 = dlast
				d(i) = d(i)*(1.0+10.0/xx)
				go to 10
			}

			bdlast = bdlst2
			dlast = dlast2
			write (6,"('peak passed')")
100			if (abs(d(i) - dlast) < 0.1e-5) {
				saturd(i,j) = d(i)
				write (6,20) i,j,saturd(i,j)
20				format ('saturd(',i2,',',i2,')= ',f8.6)
				next
			} else {
				xx = xx * 10.
				d(i) = dlast*(1.0+10.0/xx)
				go to 10
			}
			
#			d(i) = (dlast + dtemp)*.5
#			call bdrefl(i,ich,bdpth)
#			bdpth1=bdpth

#			d(i) = (dtemp + dlast2)*.5
#			call bdrefl(i,ich,bdpth)
#			bdpth2=bdpth
#
#			if (bdpth2 > bdpth1) {
#				dlast = dtemp
#				go to 100
#			} else {
#				d(i) = (dlast + dtemp)*.5
#				dlast = dtemp
#				go to 100
#			}

		}
	}
	
	do i=1,nminer {
		d(i) = dinit(i)
	}

	return
	end		


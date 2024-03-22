	subroutine cnvgbr (pturb, fitcri)
	implicit integer*4 (i-n)
#
#  main convergence routine
#

	include "../../src.specpr/common/spmaxes"   # max parameters, must be first

	include "defs.h"
	include "lmrefl.h"
	include "convh.h"

	iter = 0

#	do i = 1, nminer {
#		wdirls(i) = 0
#		ddirls(i) = 0
#	}

	call csatrd
	call cxband
	call xccont

#10	val1 = evalbr(sum1, sum1b)
#
#	iter = iter +1
#
#	if (val1 < fitcri) go to 1000      # Done !
#
#	call mvctrt                        # move and maybe contract
#
#	go to 10

1000	write (6,1010) iter, val1
1010	format ('CONVERGENCE in', i6, ' iterations with a fit of',
		1pe13.6)

	return
	end

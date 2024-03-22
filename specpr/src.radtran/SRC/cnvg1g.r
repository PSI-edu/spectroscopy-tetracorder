	subroutine cnvg1g (sum1a, sum2a)
	implicit integer*4 (i-n)
#
#  iterate to converge on best grain size for one mineral
#

	include "../../src.specpr/common/spmaxes"   # max parameters, must be first

	include "defs.h"
	include "lmrefl.h"
	include "convh.h"
	include "../../src.specpr/common/lblg"

	real*4 sum1, sum2

	dmove = 1.0
	iter = 0
	dlast = ddflt(1)*1000.

10	dsave = ddflt(1)
	call eval1g(sum1a,sum2a)   # evaluate present position
	
	write (6,20) ddflt(1), sum1a, sum2a, dmove
20	format(' d=',1pe12.5,'cm, fit=',1pe12.5,' sq(sumsq)=',
		1pe12.5,' factor=',1pe12.5)
22	if (dmove < 0.1e-5) { # converged!
		write (6,25)
25		format ('CONVERGED:')
		write (6,20) ddflt(1), sum1a, sum2a, dmove
		return
	} 

	ddflt(1) = ddflt(1) * (1.0+dmove)

	call eval1g(sum1b,sum2b)   # evaluate larger grain size

	if (sum2b < sum2a) { # larger is better
		dlast = dsave
		go to 10
	}

	ddflt(1) = ddflt(1) /(1.0+dmove)**2

	call eval1g(sum1c,sum2c)   # evaluate smaller grain size

	if (sum2c < sum2a) { #smaller is better
		dlast = dsave
		go to 10
	}

	if (sum2a <= sum2b && sum2a <= sum2c) { # present pos best, contract
		ddflt(1) = dsave
		dmove = dmove/2.0
		go to 22
	}
	

	return
	end

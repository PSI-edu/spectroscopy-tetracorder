	subroutine eval1g (sum1, sum2)
	implicit integer*4 (i-n)
#
#  iterate to converge on best grain size
#

	include "../../src.specpr/common/spmaxes"   # max parameters, must be first

	include "defs.h"
	include "lmrefl.h"
	include "convh.h"
	include "../../src.specpr/common/lblg"

	real*4 xxk(NMINL), xxn(NMINL), sum1, sum2

	real*4 r1

	sum1 = 0.0
	sum2 = 0.0
	numch = 0

	do ichn= 1, nchans {

		if (imask(ichn) == 0) go to 350

		do ifl= 1,nminer {
			xxn(ifl) = xn(ifl,ichn)
			xxk(ifl) = xk(ifl,ichn)
		}
		currch = 0		#turn off albedo calculations

		do ifl = 1, nminer {
			call dwcomp (ddflt(ifl),df,w0,wav(ichn),d(ifl))
		}

                #### redo!
                #### This call is out of date.  the 
                #### newer mrefl3 has 3 more variables.  this won't work
		call mrefl3 (nminer,wav(ichn),xxn,xxk,
			d,weight,dens,mu0,mu,r(ichn),
			wsmean(ichn),r1,g,wband,currch,sd,inrmlc,0,iflgse)
#
		numch = numch +1
		x = (1.0 - ((r(ichn) +0.25)/(xi(ichn) +0.25)))*1.25
		sum1 = sum1 + x
		sum2 = sum2 + x**2

		go to 400
#
#****************************************************************
#
350		r(ichn) = -1.23e34

400		continue

	}

	xnumch = float(numch)
	if (xnumch == 0) {
		write (6,500)
500		format('ALL CHANNELS DELETED! STOP')
		stop
	}

	sum1 = sum1 /xnumch
	sum2 = sqrt(sum2 /xnumch)

	return
	end

	subroutine bdrefl (iminer, ich, bdpth)

	implicit integer*4 (i-n)

#ccc  name: bdrefl
#ccc  version date: 2/6/86
#ccc  author(s): Roger N. Clark
#ccc  language: Ratfor
#ccc
#ccc  short description: Evaluate reflectance at three channels and
#ccc				compute band depth
#ccc
#ccc  algorithm description:
#ccc  system requirements: none
#ccc  subroutines called:
#ccc  argument list description:
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced: none
#ccc  internal variables:
#ccc  file description: none
#ccc  user command lines: none
#ccc  update information:
#ccc  NOTES:
#ccc

	include "../../src.specpr/common/spmaxes"   # max parameters, must be first

	include "defs.h"
	include "lmrefl.h"
	include "convh.h"
	include "../../src.specpr/common/lblg"

	real*4 xxk(9), xxn(9), bdpth
	integer*4 ich(3), iminer

	do kk = 1, 3 {

		k = ich(kk)
		if (k < 1 || k > nchans) {
			write (6,"('channel out of range:',i5)") k
		}
		xxn(iminer) = xn(iminer,k)
		xxk(iminer) = xk(iminer,k)


		call refl(iminer,wav(k),xxn,xxk,d,weight,dens,mu0,mu,
			r(k),wsmean(k),g)	

	}

	xu =  (r(ich(1)) + r(ich(3)))/2.0
	xd = r(ich(2))

	if (xu <= 0.0) {
		write (6,"('ERROR: continuum reflectance zero')")
		bdpth = 0.0
		return
	}
	bdpth = 1.0 - xd/xu

	write (6,1000) iminer, bdpth, d(iminer), r(ich(1)),r(ich(2)),r(ich(3))
1000	format (i2,' bdpth=',f8.6,'  d=',f10.7,3(5x, f8.6))
	return
	end

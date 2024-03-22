   subroutine kiter(gmalb, astep, sd, iminr)
#
#     iterates to get best value of k for
# 	abscf and unmix routines
#
	implicit integer*4 (i-n)

	include "../../src.specpr/common/spmaxes"   # max parameters, must be first

	include "../../src.specpr/common/lundefs"
	include "../../src.specpr/common/alphabet"
	include "../../src.specpr/common/lblg"
	include "defs.h"
	include "lmrefl.h"

	real*4 xxn(9),xxk(9)   # temporary holding arrays for
				#   passing info to mrefl sub.

        real*4 xemn1  # index of refraction of the matrix (at one wavelength)
        real*4 xemk1  # absorption coefficient in inverse cm of the matrix (at one wavelength)

	real*4 rx,r1

# the following is set as default: no matrix material

	xemn1 = 1.0  # index of refraction for a vacuum
	xemk1 = 0.0  # absorption coefficient for a vacuum

#****************************************************************
# XXXXXXXX
	write (ttyout,"('Working...')")
	do i= 1, nchans {
		if (mod(i,100) == 0) {
			write (ttyout,322) i
322			format (1x,'Just starting',i6,'th channel')
		}

#**debug write statement
#**		write (ttyout,311) w(i), r(i), xn(iminr,i)
#**311		format (1x, 'input: w=' f8.4, '  ra=', f7.4, '  xn=', f8.4)
#
		rlast = 1.
		xxklast = 1000000.
		xmnlast = 0.001
		iterat = 0

		if ((xn(iminr,i) <= 1e-30) || (r(i) <= 0.1e-30)) go to 400
		if (r(i) > 1.0) go to 400
		if ((wav(i) > -1.231e34) & (wav(i) < -1.229e34)) go to 400
#
#******** do iterative computation to find xk ************************
#
#		start guess xk = 100.0, itterate until r and ra agree to
#			better than 0.0001
		xk(iminr,i) = 100.0
		xmin = 0.001
		xmax = 100000.
#
		do ifl= 1,nminer {
			xxn(ifl) = xn(ifl,i)
			xxk(ifl) = xk(ifl,i)
		}

320		call calcr(i,rx,r1,xxn,xxk,xemn1,xemk1,gmalb,astep,sd)
#
		iterat = iterat+1
#**debug write statement
#**		write (ttyout,323) i, rx, r(i), xxk(iminr), xmax, xmin
#**323		format (1x,'ch=',i6,2(f7.4, 1x), 3(f12.4, 1x))
#
#        check if converged or out of range, is then quit
#
		ratiok = xxk(iminr)/xxklast
		ratiox = xmax/xmin
		if (r(i) == -1.23e34) go to 400
		if ( (abs(r(i)-rx) <= 0.00001) & 
			(.999 <= ratiok & ratiok <= 1.001) ) go to 390
		if ( (.9999 <= ratiox & ratiox <= 1.0001)  || 
			(.9999 <= ratiok & ratiok <= 1.0001) )  {
			write (ttyout,325) i, rx, r(i), xxk(iminr)
325			format (' CAN NOT reach reflectance level:',/,
				' channel',i4,':',
				' computed refl=',f7.6,' given refl=',
				f7.6,'  closest abs. coef=',f12.5)
			go to 390
		}
# if r increasing as k increasing move to  other side of curve
#
		if (rx > rlast & xxk(iminr) > xxklast) {
			rlast = rx
			xxklast = xxk(iminr)
			xmin = xmnlast
			go to 370
		}

		if (xxk(iminr) < 0.000011) {
			write (ttyout,330) i
330			format (' min k reached at channel',i5,
					' DELETED')
			go to 400
		}
		if (xxk(iminr) > 9000000.) {
			write (ttyout,335) i
335			format (' max k reached at channel',i5,
					' DELETED')
			go to 400
		}
#
		xxklast = xxk(iminr)
		rlast = rx
		xmnlast = xmin
#
#		if r .gt. rx (computed r) then xxk is too large
#
		if (r(i) < rx) go to 380
370			xmax = xxk(iminr)
			xxk(iminr) = (xxk(iminr)+xmin)/2.0
		go to 320
#
#		xxk too small
#
380		xmin = xxk(iminr)
		xxk(iminr) = (xmax+xxk(iminr))/2.0
		go to 320
#
#****************************************************************
#
390		xk(iminr,i) = xxk(iminr)
		next
#
400		r(i) = -1.23e34
		xk(iminr,i) = -1.23e34
#
	}

500	return
	end

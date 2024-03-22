   subroutine getdefs(sd,il)

#	gets the normalization and grain scattering
#		parameters


	implicit integer*4 (i-n)

	include "../../src.specpr/common/spmaxes"   # max parameters, must be first

	include "../../src.specpr/common/lundefs"
	include "../../src.specpr/common/alphabet"
	include "defs.h"
	include "lmrefl.h"

#
# ask if want to normalize computation
#
	write (ttyout,200)
200	format (/,/,'============',/,'Do ',
'you want to normalize the computation to ideal reflectance',/,
'standard at the same viewing geometry (enter y for yes)',/,
'NOTE: for proper geom albed values must normalize' )
	i=1
	call crtin
	call wjfren (i,x,il)
	if (il == ihe || il == ihx) go to 10000
	inrmlc = 0
	if (il == ihy) inrmlc = 1
#
####add s entering option#######
#
210		write (ttyout,220)
220		format ('Enter s value. (Default equals .3)')

	call crtin
	i = 1
	call wjfren (i,x,il)
	if (il == ihe || il == ihx) go to 10000
	if (il != 0) {
		call what(i)
		go to 210
	}
	if (i >= 80) {
		sd=.3
	} else {
		sd=x
	}
	do itemp = 1, nminer {
		scoef(itemp) = sd
	}
#
#  enter grain size wavelength dependence parameters
#
250	write (ttyout,255)
255	format (/,'Enter grain size dependence parameters:',/,
	10x, 'The grain size dependence equation has the form:',//,
	20x, 'd = ddflt/(1.0 + df * (w0 - wavelength))',//,
	10x, ' where wavelength and w0 are in microns.',/,
	10x, 'Enter df and w0  (example: df, w0= 0.1 2.5),',
		' default = 0.0 2.5.')
	call crtin
	i = 1
	call wjfren (i,x,il)
	if (il == ihe || il == ihx) go to 10000
	if (il != 0) {
		call what(i)
		go to 250
	}
	if (i >= 80) {
		df = 0.0
		w0 = 1.0
	} else {
		df = x

		call wjfren (i,x,il)
		if (il == ihe || il == ihx) go to 10000
		if (il != 0) {
			call what(i)
			go to 250
		}
		if (i >= 80) {
			w0 = 2.5
		} else {
			w0 =x 
		}
	}
#
10000	return
	end

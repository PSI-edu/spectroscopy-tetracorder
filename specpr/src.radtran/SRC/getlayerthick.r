   subroutine getlayerthick(il)

#	gets the layer thicknesses

	implicit integer*4 (i-n)

	include "../../src.specpr/common/spmaxes"   # max parameters, must be first

	include "../../src.specpr/common/lundefs"
	include "../../src.specpr/common/alphabet"
	include "defs.h"
	include "lmrefl.h"

	real*4 x, xx, xmin
	integer*4 il, jlyr, mlayers, i

130	write (ttyout, 135) nlayers
135	format (/,/,'============',/,
                ' This is a ',i3,
		' layer model.',/)

	mlayers = 1            # temp variable for layers in actual use
	if ( nlayers > 1 ) {
		mlayers = nlayers - 1

		do jlyr = 1, mlayers {
		
			if (jlyr == 1) {
				write (ttyout, *) ' Laye 1: top layer'
			}
			write (ttyout, 10) jlyr
10			format (/,' Type in the layer thickness in centimeters for layer', i3)
			call crtin
			i = 1
			call wjfren (i,x,il)
			if (il == ihe || il == ihx) go to 10000
			if (i >= 80 || il != 0) {
				call what(i)
				go to 130
			}
			xmin = 0.000000000001
			if (x < xmin) {
				call what(i)
				write (ttyout,140) xmin
140				format (/,' value cannot be LESS THAN ', f18.15)
				go to 130
			}
			tlyr(jlyr) = x

		}

	} else {

		write (ttyout, *) '  getlayerthick: ???  ONLY ONE LAYER (should not see this here)'
		tlyr(1) = 1.0e25  # one layer, ~ infinitely thick
	}
	

10000	return
	end


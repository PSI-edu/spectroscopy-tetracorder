   subroutine getmu(diangl,deangl,gmalb,astep,il)
#
# read in mu and mu0
#
	implicit integer*4 (i-n)

	include "../../src.specpr/common/spmaxes"

	include "../../src.specpr/common/lundefs"
	include "../../src.specpr/common/alphabet"
	include "defs.h"
	include "lmrefl.h"

	real*4 gmalb


150	write (ttyout, 155)
155	format (/,'============',/,
                ' Type in the angle of INCIDENCE, the angle of',
                   ' EMISSION and the PHASE angle in decimal degrees',/,
		' or g for GEOMETRIC ALBEDO followed by the degree',
		'  increment',/,
		'  or <cr> for default increment')
	call crtin
	gmalb = 0.
	i = 1
	call wjfren (i,x,il)
	if (il == ihe || il == ihx) go to 10000
	if (il == ihg) {
		gmalb = 1.
		g = 0.		# zero phase
		call wjfren (i,x,il)
		if (il != 0) {
			call what(i)
			goto 150
		}
		if ( x == 0 & il == 0 ){
			astep = 10.	#accuracy ~ +- .3%
					# 5 deg incr, ~ +- .1%
		} else {
			astep = x
		}
	} else {

# get incidence angle
		if (i >= 80 || il != 0) {
			call what(i)
			go to 150
		}
		if (x < -89.999 || x > 89.999) {
			call what(i)
			write (ttyout,160)
160			format (' value must be within the range:',
				' -89.999 to 89.999',/,
				'REENTER')
			go to 150
		}
		xiangl = x/57.29578
		diangl = x

# get emission angle
		call wjfren (i,x,il)
		if (il == ihe || il == ihx) go to 10000
		if (il != 0) {
			call what(i)
			go to 150
		}
		if (x < -89.999 || x > 89.999) {
			call what(i)
			write (ttyout,160)
			go to 150
		}
		xeangl = x/57.29578
		deangl = x

# get phase angle
		call wjfren (i,x,il)
		if (il == ihe || il == ihx) go to 10000
		if (il != 0) {
			call what(i)
			go to 150
		}
		if (x < 0 || x > 179.999) {
			call what(i)
			write (ttyout,161)
161			format (' Phase value must be within the range:',
				' 0 to 179.999',/,
				'REENTER')
			go to 150
		}
		g = x/57.29578

		mu0 = cos(xiangl)
		mu= cos(xeangl)
	}

10000	return
	end

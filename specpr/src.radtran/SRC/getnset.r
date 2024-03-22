   subroutine getnset(il)

#	gets the number of optical constant sets
#
        implicit integer*4 (i-n)

	include "../../src.specpr/common/spmaxes"   # max parameters, must be first

	include "../../src.specpr/common/lundefs"
	include "../../src.specpr/common/alphabet"
	include "defs.h"
	include "lmrefl.h"



10	write (ttyout, 15) NMINL
15	format (/,/,'============',/,
                ' type in the number of components in the ',
					'intimate mixture.',/,
		'      Maximum =', i6,/,
		'      You will need to input n and k for ',
					'each component.')

	call crtin
	i = 1
	call wjfren (i,x,il)
	#write (ttyout,'("DEBUG: x=",f9.5)') x
	if (il == ihe || il == ihx) go to 10000
	if (il != 0 || x < 1 || x > 9) {
		call what(i)
		write (ttyout, 20)
20		format (' input error, reenter')
		go to 10
	}
	nminer = x
	write (ttyout, 30) nminer
30	format (1x, i6, ' optical index sets allowed',
		' in this computation')

10000	return
	end 

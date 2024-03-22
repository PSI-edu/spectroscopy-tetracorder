        subroutine getnst2(il)

#	gets the number of optical constant sets
#
        implicit integer*4 (i-n)

	include "../../src.specpr/common/spmaxes"   # max parameters, must be first

	include "../../src.specpr/common/lundefs"
	include "../../src.specpr/common/alphabet"
	include "defs.h"
	include "lmrefl.h"


     if ( nlayers < 2 ) {

10	write (ttyout, 15) NMINL, NMMIX
15	format (/, '============================',/,
		' This is a single layer of semi-infinite thickness model.',/,
                ' Type in the number of input optical index sets in ',
                ' the intimate mixture (max sets =',i3,')',/,
                ' Note each set can be a molecular mix of',i3,
                     ' components.',/)

	call crtin
	i = 1
	call wjfren (i,x,il)
	#write (ttyout,'("x=",f9.5)') x
	if (il == ihe || il == ihx) go to 10000
	if (il != 0 || x < 1 || x > NMINL) {
		call what(i)
		write (ttyout, 20)
20		format (' input error, reenter')
		go to 10
	}
	nminer = nint(x)
	write (ttyout, 30) nminer
30	format (1x, i6, ' optical index sets allowed',
		' in this computation',/)

     } else {    # multiple layers

	
	for (jlyr=1; jlyr<=nlayers; jlyr=jlyr+1) {
110		write (ttyout, 115) jlyr, NMINL, NMMIX
115		format (/, '============================',/,
		 ' For LAYER ', i3,':',/,
       	         ' Type in the number of input optical index sets in ',
       	         ' the intimate mixture (max sets =',i3,')',/,
       	         ' Note each set can be a molecular mix of',i3,
                     ' components.',/)

		call crtin
		i = 1
		call wjfren (i,x,il)
		#write (ttyout,'("x=",f9.5)') x
		if (il == ihe || il == ihx) go to 10000
		if (il != 0 || x < 1 || x > NMINL) {
			call what(i)
			write (ttyout, 120)
120			format (' input error, reenter')
			go to 110
		}
		nminer = nint(x)
		nminerlyr(jlyr) = nminer
		write (ttyout, 130) nminer, jlyr
130		format (1x, i6, ' optical index sets allowed',
			' in layer ',i3,' in this computation',/)
	}

     }

10000	return
	end 

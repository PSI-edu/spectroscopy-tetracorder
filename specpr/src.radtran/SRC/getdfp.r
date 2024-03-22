   subroutine getdfp(iminr,il)

#	gets the grain size diameter, weight fraction, density

	implicit integer*4 (i-n)

	include "../../src.specpr/common/spmaxes"   # max parameters, must be first

	include "../../src.specpr/common/lundefs"
	include "../../src.specpr/common/alphabet"
	include "defs.h"
	include "lmrefl.h"

	real*4 totw, x, xx
	integer*4 inminer, il, i, j

130	write (ttyout, 135) iminr
135	format (/,/,'============',/,
                ' For optical index data set',i3,
		' type in the following (on the same line):',/,
		2x,'grain size in centimeters',
		2x,'weight fraction',
		2x,'and density')

	if (iminr == nminer && nminer > 1) {
		write (ttyout, 60)
60		format (/,' Because this is the last optical index set, you may enter',
				'  w  for the weight and',/,
				'    the program will figure out the weight')

	}
	call crtin
	i = 1
	call wjfren (i,x,il)
	if (il == ihe || il == ihx) go to 10000
	if (i >= 80 || il != 0) {
		call what(i)
		go to 130
	}
	if (x < 0) {
		call what(i)
		write (ttyout,140)
140		format (/,' value cannot be LESS THAN ZERO')
		go to 130
	}
	d(iminr) = x
	ddflt(iminr) = x

	call wjfren (i,x,il)
	if (il == ihe || il == ihx) go to 10000
	if (i >= 80 || il != 0) {
		call what(i)
		go to 130
	}
	if (x < 0) {
		call what(i)
		write (ttyout,140)
		go to 130
	}
	if (x > 1.0) {
		call what(i)
		write (ttyout,145)
145		format (/,' value cannot be GREATER THAN 1.0')
		go to 130
	}
	
	if (iminr > 1) {
		inminer=iminr-1
		totw=0.0
	        do j= 1,inminer {
        	        totw = totw + weight(j)
        	}
		if (totw > 1.000001) {
			write (ttyout,155), totw
			go to 130
		}
		if (il == ihw) {  # find weight.
			 x=1.0-totw   # set weight, is set to weight(iminr) below
		}
	}
	weight(iminr) = x
	totw=0.0
        do j= 1,iminr {
       	        totw = totw + weight(j)
       	}
	xx = abs(totw-1.0)
	if (totw > 1.000001 ) {
		write (ttyout,155), totw
155		format (/,' ERROR: total weight is >1:', f15.7)
		call what(1)
		call crtin
		go to 130
	}

	call wjfren (i,x,il)
	if (il == ihe || il == ihx) go to 10000
	if (i >= 80 || il != 0) {
		call what(i)
		go to 130
		}
	if (x < 0) {
		call what(i)
		write (ttyout,140)
		go to 130
	}
	dens(iminr) = x
#	write(ttyout,"(' DEBUG: density=',f12.6,/)") dens(iminr)

	write (ttyout, 460) totw
460     format (/,' Total weight fraction is now =',f12.6,/)
	

10000	return
	end


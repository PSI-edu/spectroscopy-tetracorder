	subroutine abpeak (xx,iminr)

	implicit integer*4 (i-n)

	include "../../src.specpr/common/spmaxes"   # max parameters, must be first

	include "defs.h"
	include "lmrefl.h"
	include "../../src.specpr/common/lblg"

	real*4 xx(SPMAXCHAN), slope1, slope2, half(SPMAXCHAN), maxval(SPMAXCHAN)
	integer*4 max(256), min(256), maxtmp
	real*4 temp(SPMAXCHAN), maxvtp
	equivalence (temp(1),max(1))
#
# smooth input data
#
	write (6,3) nchans
3	format(' nchans= ',i4)
#	do i = 2, nchans-1 {
#		temp(i) = (0.5*xx(i-1) + xx(i) + 0.5*xx(i+1))/2.0
#	}
#	temp(1)=(xx(1)+xx(2))/2.0
#	temp(nchans) = (xx(nchans)+xx(nchans-1))/2.0
#	do i = 1, nchans {
#		xx(i) = temp(i)
#		temp(i)=0.0
#	}

	k = 1

	do j=1,nchans-2 {
		slope1 = (xx(j+1) - xx(j))
		slope2 = (xx(j+2) - xx(j+1))

		if ((slope1 >= 0.0) & (slope2 < 0.0)) {
			if (k > 1) {
				if (max(k-1) != 0) next
			}
			maxval(k) = xx(j+1)
			max(k) = j+1
			imask(j+1) = 1
			write (6,1) k,max(k)
1			format (' max # ',i4,' at channel ',i4)
			k = k + 1
		} else if ((slope1 <= 0.0) & (slope2 > 0.0)) {
			if (k > 1) {
				if (min(k-1) != 0) next
			}
			min(k) = j+1
			imask(j+1) = 1
			write (6,2) k,min(k)
2			format (' min # ',i4,' at channel ',i4)
			k = k + 1
		}
	}

	if (max(1) == 0) {
		l=1
	} else {
		l=0
	}
	
	do i=1,k {
		max(i) = max(i+l)
		maxval(i) = maxval(i+l)
		l=l+1
	}

	do j=2,l {
		i=j
50		if (maxval(i-1) < maxval(i)) {
			maxtmp = max(i)
			max(i) = max(i-1)
			max(i-1) = maxtmp

			maxvtp = maxval(i)
			maxval(i) = maxval(i-1)
			maxval(i-1) = maxvtp

			if (i != 2) {
			        i=i-1
				go to 50
			}
		}
	}

	if (l < 10) {
		npeaka(iminr) = l
	}else{
		npeaka(iminr) = 10
	}

	n=npeaka(iminr)

	do i=1,n {
		ipeaka(iminr,i) = max(i)
		write (6,"('peak(',i3,')=',i5)") i,ipeaka(iminr,i)
	}

	do i=1,k {
		half(i) = (max(i) - min(i)) / 2.0
	}

	end

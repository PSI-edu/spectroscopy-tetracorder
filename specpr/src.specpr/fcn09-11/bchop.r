	subroutine bchop(x,m)
	implicit integer*4 (i-n)

	include "../common/spmaxes"   # max parameters, must be first

	include "../common/blank"
	include "../common/lblg"
	include "../common/lundefs"
#
#     this subroutine finds the channel number corresponding to
#     the inputted wavelength or wave number.
#
	mm = 0
	do  m=1,nchans {
		mm = mm +1
		if(x>=dataa(mm) && x<dataa(mm+1)) {
			if(x>=((dataa(mm)+dataa(mm+1))/2)) mm=mm+1
			break
		}
		if(mm>=256) write(ttyout,50)
	}
	return
50      format(' match points out of range')
	end

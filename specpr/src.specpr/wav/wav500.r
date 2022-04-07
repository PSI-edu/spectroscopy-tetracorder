	subroutine wav500
	implicit integer*4 (i-n)
#cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
#                                                               c
#       this routine changes the number of channels in a        c
#       wavelength record.                                      c
#                                                               c
#cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
#
	include "../common/spmaxes"   # max parameters, must be first

	include "../common/blank"
	include "../common/lbl7"
	include "../common/lbl4"
	include "../common/label1"
	include "../common/label3"
	include "../common/lblg"
	include "../common/lundefs"



	repeat {
		write(ttyout,10)
		call crtin
		i = 1
		call wjfren(i,x,il)
		if (il==0) {
			if (x!=-1.0) {
				if (x>256.0) x = 256.0
				if (x<2.0) x = 2.0
			}
			call wjfren(i,a,il)
			if (il==0 && a>=1.0 && a<=99.0) break 1
		}
	}
	itrol(2) = a
	iwavfl = itrol(2)
	call redhed(0,iwavfl,ier)
	nchans = x
	call wrihed(iwavfl)
	return

10  format(
' type in the number of channels and the wavelength record #',/,
' set channels = -1 to only change the wavelength record #',/)

	end

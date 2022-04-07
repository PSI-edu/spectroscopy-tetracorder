	subroutine wav400
	implicit integer*4 (i-n)


	include "../common/spmaxes"   # max parameters, must be first

	include "../common/blank"
	include "../common/lbl7"
	include "../common/lbl4"
	include "../common/label1"
	include "../common/label3"
	include "../common/lblg"
	include "../common/lundefs"
	include "../common/alphabet"


	call hreset(1)
	write(ttyout,20)
	do j = 1,256 {
		write(ttyout,30)j
		repeat {
			call crtin
			i = 1
			call wjfren(i,x,il)
			if (il==ihn) go to 10
			if (il==ihx) return
			if (il==0) break 1
			write(ttyout,40)j
		}
		dataa(j) = x
	}
	j = j+1
10      nchans = j-1
	if (j>=256) nchans = 256
	if (j<256)
		do j = j,256
			dataa(j) = -1.0
	call wavfil(iwavfl)
	call chginw(iwavfl)
	call wriwav(iwavfl)
	call wrihed(iwavfl)
	itrol(2) = iwavfl
	return

20      format(
' to type in your own wavelengths, simply type the number and return',/,
' when you are through type  n.',/,
' the number of channels will be set automatically',/,
' type  x  to stop input and not write the wavelengths to the file',/)

30      format(' type in channel',i4,/)

40      format(' error in input, retype channel',i4,/)

	end

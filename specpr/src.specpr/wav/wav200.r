	subroutine wav200
	implicit integer*4 (i-n)
#cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
#                                                               c
#       this routine reads a record from w,v,y,d,or u and       c
#       writes it into the wavelength file.                     c
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
	include "../common/alphabet"



	repeat {
		write(ttyout,10)
		call crtin
		i = 1
		call wjfren(i,x,il)
		if (il==ihx) return
		idev = 0
		call devlun(4,il,idev)
		if (idev!=0) {
			call devsta(idev,ista,0,iprt)
			if (ista>0) {
				call wjfren(i,x,il)
				if (il==0) {
					ifil = x
					if (ifil>=1 &&
					    ifil<=2000 &&
					    ifil>iabs(iprt) &&
					    iprt!=-1) {
						write(ttyout,20)
						call crtin
					} else {
						call wjfren(i,x,il)
						if (il==0) {
							nchans = x
							if (nchans<2) nchans = 2
							if (nchans>256) nchans = 256
							call redfil(ifil,idev,ier)
							if (ier==0) break 1
						}
					}
				}
			}
		}
	}
	do i = 1,256
		dataa(i) = data(i)
	call wavfil(iwavfl)
	call wriwav(iwavfl)
	call wrihed(iwavfl)
	itrol(2) = iwavfl
	return

10      format(' type in the file id, record number, ',
			'a space and the number of channels',/,
		' the wavelengths will then be written to the ',
			'wavelength file',/,
		' type x  to exit this section',/)

20      format(' protection violation...')


	end

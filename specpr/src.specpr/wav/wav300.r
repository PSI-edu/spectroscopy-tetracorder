    subroutine wav300
    implicit integer*4 (i-n)
#cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
#                                                               c
#       this routine writes a wavelength set to a file          c
#                                                               c
#cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

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
	call er
	write(ttyout,10)
	call crtin
	i = 1
	call wjfren(i,x,il)
	idev = 0
	if (il==ihx) break 1
	call devlun(4,il,idev)
	if (idev!=0) {
	    call devsta(idev,ista,0,iprt)
	    if (ista>0) {
		call wjfren(i,x,il)
		if (il==0) {
		    ifil = x
		    if (iprt!=-1 || ifil>0)
			if (ifil>=0&&ifil<=maxrec) {
			    if (iprt!=-1 && iprt<=-2) {
				write(ttyout,20)
				call crtin
				next 1
			     } else {
				if (ifil==0)  ifil = iprt+1
				if (ifil!=iprt+1) {
				    write(ttyout,30)
				    call crtin
				    next 1
				}
			    }
			    ichan = nchans
			    call wavfil(iwavfl)
			    call redhed(0,iwavfl,ier)
			    call wavlng(iwavfl)
			    nchans = ichan
			    do j = 1,256
				data(j) = dataa(j)
			    call wrifil(ifil,idev,ier)
			    if (idev==8&&ier==2) ifilex = ifilex+1
			    if (ier==2) break 1
			}
		}
	    }
	}
    }
    return
10  format(' type in the file id (v, d, u, or y) ',
		'and the record number',/,
		'     where you wish to write the wavelengths',/,
		' type  x  to exit this section',/)
20  format(' read only device',/)
30  format(' protection violation',/)
    end

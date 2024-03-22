   subroutine spcwri(xrec,il)

	implicit integer*4 (i-n)
	include "defs.h"
	include "../../src.specpr/common/spmaxes"
	include "../../src.specpr/common/lundefs"
	include "../../src.specpr/common/alphabet"
	include "../../src.specpr/common/label1"
	include "../../src.specpr/common/lblg"
	include "../../src.specpr/common/lbl4"
	include "lmrefl.h"

	real*4 xrec(SPMAXCHAN)

50	format('ERROR record number out of range, reenter')
#
# write results to file
#
700	call whedr
        write (ttyout,701)
701	format ('Type in the file id and record number ',
		'     of where to write the results')
	call crtin
	i = 1
	call wjfren(i,x,id)
	if (id == ihe || id == ihx) {
		il = id
		go to 10000
	}
	call wjfren (i,x,il)
	if (il != 0) {
		call what(i)
		go to 700
	}
	if (x < 0 || x > maxrec) {
		write (ttyout,50)
		go to 700
	}
	irecou = x
	call devlun(4,id,lun)
	call devsta(lun,ista,ier,iprt)
	if (irecou == 0 && iprt >= 0) irecou = iprt+1
	if (iprt < -1) {   # read only device
		write (ttyout,704)
704		format ('READ ONLY Device, REENTER',/)
		go to 700
	}
	if (iprt != -1 && irecou != iprt+1) {
		write (ttyout,705)
705		format ('illegal output record number specified',/,
			'REENTER',/)
		go to 700
	}
	itmp = irecou
	do i = 1, nchans {
		 data(i) = xrec(i)
	}
	call sptime
	call wrifil(itmp,lun,ier)
	
10000	return
	end

   subroutine spctwri(il)

	implicit integer*4 (i-n)

	include "../../src.specpr/common/spmaxes"

	include "defs.h"
	include "lmrefl.h"

	include "../../src.specpr/common/lundefs"
	include "../../src.specpr/common/alphabet"
	include "../../src.specpr/common/label1"
	include "../../src.specpr/common/lblg"
	include "../../src.specpr/common/lbl4"
	include "../../src.specpr/common/inputhistory"

50	format('ERROR record number out of range, reenter')
#
# write results to file
#
700     itothch=inhistch-1
        write (ttyout,701) itothch
701	format ('Type in the file id and record number ',
		'of where to write the HISTORY results',/,
		'     NOTE: it should be just after the data record',/,
		'     input history =',i8,' characters')
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

	# tinphist and inhistch from the inputhistory common block
	call sptime

	itext(1:inhistch-1) = tinphist(1:inhistch-1)
	itxtpt=0
	itxtch=inhistch-1

#                             1111111111222222222233333333334
#                    1234567890123456789012345678901234567890
	ititl(1:40)="command history to previous data        "

	ibit=1
	call setbit(icflag, ibit)   # set text bit

	call wrifil(itmp,lun,ier)
	
10000	return
	end

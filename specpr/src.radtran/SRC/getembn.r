   subroutine getembn(il)

#  	gets index of refraction from specpr file

	implicit integer*4 (i-n)
	include "../../src.specpr/common/spmaxes"
	include "../../src.specpr/common/lundefs"
	include "../../src.specpr/common/alphabet"
	include "../../src.specpr/common/label1"
	include "../../src.specpr/common/lblg"
	include "defs.h"
	include "lmrefl.h"

50	format ('ERROR record number out of range, reenter')

100	write (ttyout, 105) 
105	format ('type in the file ids and record numbers of the ',
		' real index of refraction,',/,
		' for the MATRIX material ')

	call crtin
	i = 1
	call wjfren (i,x,id)
	call wjfren (i,x,il)
	if (id == ihe || id == ihx) {
		il = id
		go to 10000
	}
	if (il == ihe || il == ihx) go to 10000
	if (il != 0) {
		call what(i)
		go to 100
	}
	if (x < 1 || x > maxrec) {
		write (ttyout, 50) 
		go to 100
	}
	irecxen = x    #record number for xn set iminr
	idxen = id
	call devok(4,id,irecxen,lun, ier)
	if (ier != 0) go to 100
	itmp = irecxen
	call redfil (itmp,lun,ier)
	if (ier != 0) go to 100
	write (ttyout,110) iminr, ititl, itchan
110	format (/, ' Matrix index of refraction set',/,5x,i2, 5x,
		a, 5x, 'channels=',i6,/)
	do j = 1, nchans {
		xemn(j) = data(j)
	}
	xentitl = ititl


10000	return
	end 

   subroutine getembk(il)

#	get absorption coefficient from specpr file

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
105	format ('type in the file id and record numbers of the ',
		' absorption coefficient,',/,
		' for the MATRIX material ')

	call crtin
	i=1
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
	irecxek = x    #record number for xrerrD set iminr
	idxek = id
	call devok(4,id,irecxek,lun, ier)
	if (ier != 0) {
		write (ttyout,120)
120		format ('REENTER')
		go to 100
	}
	itmp = irecxek
	call redfil (itmp,lun,ier)
	if (ier != 0) {
		write (ttyout,120)
		go to 100
	}
	write (ttyout,125) iminr, ititl, itchan
125	format (/,' Matrix absorption coefficient set',/,5x,i2, 5x,
		a, 5x, 'channels=',i6,/)
	do j = 1, nchans {
		xemk(j) = data(j)
	}
	xektitl = ititl

10000	return
	end 

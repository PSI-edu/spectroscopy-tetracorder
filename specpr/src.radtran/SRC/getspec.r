   subroutine getspec(il)

#	gets spectrum to unmix from specpr file

	implicit integer*4 (i-n)
	include "../../src.specpr/common/spmaxes"
	include "../../src.specpr/common/lundefs"
	include "../../src.specpr/common/alphabet"
	include "../../src.specpr/common/label1"
	include "../../src.specpr/common/lblg"
	include "defs.h"
	include "lmrefl.h"


50	format ('ERROR record number out of range, reenter')
120	format ('REENTER')

80	write (ttyout, 85)
85	format('type in the file id and record number of the ',
		'reflectance spectrum ')
		
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
		go to 80
	}
	if (x < 1 || x > maxrec) {
		write (ttyout, 50) 
		go to 80
	}
	irecxk(nminer) = x    #record number for reflectance data
	idxk(nminer) = id
	call devok(4,id,irecxk(nminer),lun, ier)
	if (ier != 0) {
		write (ttyout,120)
		go to 80
	}
	itmp = irecxk(nminer)
	call redfil (itmp,lun,ier)
	if (ier != 0) {
		write (ttyout,120)
		go to 80
	}
	write (ttyout,90) ititl, itchan
90	format ('Reflectance spectrum:',/,5x,
		a, 5x, 'channels=',i6,/,79('*'))
	do j = 1, nchans {
		r(j) = data(j)
	}
	xktitl(nminer) = ititl

10000	return
	end 

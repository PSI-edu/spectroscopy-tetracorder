	subroutine getns(nguess,ntop,nbot)
	implicit integer*4 (i-q)

	include "defs.h"
	include "../../src.specpr/common/spmaxes"
	include "../../src.specpr/common/lblg"
	include "../../src.specpr/common/label1"
	include "../../src.specpr/common/lbl4"
	include "../../src.specpr/common/lundefs"
	include "../../src.specpr/common/alphabet"

	real*4 nguess(SPMAXCHAN),ntop(SPMAXCHAN),nbot(SPMAXCHAN)

	i = 1
	call wjfren (i,x,id)
	call wjfren (i,x,il)
	if (id == ihe || id == ihx) {
		ier=id
		return
	}
	if (il == ihe || il == ihx) {
		ier=il
		return
	}
	if (il != 0) {
		call what(i)
		ier=1
		return
	}
	if (x < 1 || x > maxrec) {
		write (ttyout, 66) 
		ier=1
		return
	}
	call devok(4,id,x,lun, ier)
	if (ier != 0) {
		write (ttyout,72)
72			format ('REENTER')
		ier=1
		return
	}
	call redfil (x,lun,ier)
	if (ier != 0) {
		write (ttyout,72)
		ier=1
		return
	}
	write (ttyout,74) ititl, itchan
74		format ('Guesses at N:',/,5x,
		a, 5x, 'channels=',i6,/,79('*'))
	do j = 1, nchans {
		nguess(j) = data(j)
	}
	call wjfren (i,x,id)
	call wjfren (i,x,il)
	if (il != 0) {
		call what(i)
		ier=1
		return
	}
	if (x < 1 || x > maxrec) {
		write (ttyout, 66) 
		ier=1
		return
	}
	call devok(4,id,x,lun, ier)
	if (ier != 0) {
		write (ttyout,72)
		ier=1
		return
	}
	call redfil (x,lun,ier)
	if (ier != 0) {
		write (ttyout,72)
		ier=1
		return
	}
	write (ttyout,84) ititl, itchan
84		format ('Maximum N file:',/,5x,
		a, 5x, 'channels=',i6,/,79('*'))
	do j = 1, nchans {
		ntop(j) = data(j)
	}
	call wjfren (i,x,id)
	call wjfren (i,x,il)
	if (il != 0) {
		call what(i)
		ier=1
		return
	}
	if (x < 1 || x > maxrec) {
		write (ttyout, 66) 
		ier=1
		return
	}
	call devok(4,id,x,lun, ier)
	if (ier != 0) {
		write (ttyout,72)
		ier=1
		return
	}
	call redfil (x,lun,ier)
	if (ier != 0) {
		write (ttyout,72)
		ier=1
		return
	}
	write (ttyout,94) ititl, itchan
94		format ('Minimum N file:',/,5x,
		a, 5x, 'channels=',i6,/,79('*'))
	do j = 1, nchans {
		nbot(j) = data(j)
	}
66		format (' ERROR record number out of range, reenter')
	return
	end

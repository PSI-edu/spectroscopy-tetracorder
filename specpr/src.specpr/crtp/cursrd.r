	subroutine cursrd(ix,iy)
	implicit integer*4 (i-q)

	include "../common/spmaxes"   # max parameters, must be first

	include "../common/lbl3"
	include "../common/lbl4"
	include "../common/hptrm"
	include "../common/lundefs"
	include "../common/alphabet"

	real*4 qx,qy
	character*1 cntrlq,escape

	cntrlq = char(17)
	escape = char(27)

	if (igrmod < 20 ) { #HP2623A
		write (ttyout,20) escape, cntrlq
		

		read (ttyin,2,end=2000,err=2000) iopcon
		i=2
		call wjfren (i,qx,il)
		i=i+1
		call wjfren (i,qy,il)

		ix=qx
		iy=qy

		iy = iy+10   # the carriage return subtracted 10 from y

		#if (ix < 0) ix = 0      # orig
		#if (ix > 512) ix = 512
		#if (iy < 0) iy = 0
		#if (iy > 359) iy = 359

		if (ix < 0) ix = 0
		if (ix > 1024) ix = 1024
		if (iy < 0) iy = 0
		if (iy > 718) iy = 718
	} else if (igrmod >= 50 & igrmod <= 53 ) {
#XWIN           call xcrsrd(ix,iy)
	}
2	format (a)
%20	format (a,'*s3^',a,$)
2000	return
	end

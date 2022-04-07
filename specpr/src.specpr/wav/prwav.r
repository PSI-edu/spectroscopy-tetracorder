	subroutine prwav(lun,st,fin)
	implicit integer*4 (i-n)

	include "../common/spmaxes"   # max parameters, must be first

	include "../common/blank"
	include "../common/lbl7"
	include "../common/lbl4"
	include "../common/label1"
	include "../common/label3"
	include "../common/lblg"


	integer*2 st,fin

	call setspo
	if (lun==6) call er
	if (lun==12) write(lun,10)

	write(lun,20)
	do i = st,fin {
		ierr = 0
		call redhed(0,i,ierr)
		if (ierr==0) {
			write(lun,30)i,ititl,nchans,datea
			if (lun==12) write(lun,40)ihist
		}
	}
	call crtin
	call dumpsp
	return

10	format('1')
20	format(10x,'wavelength file summary',/,10x,'***********************',/)
30	format(1x,'file:',i2,3x,a40,' ch=',i3,3x,'date: ',a2,'/',a2,'/',a2)
40	format(5x,'history: ',a60)

	end

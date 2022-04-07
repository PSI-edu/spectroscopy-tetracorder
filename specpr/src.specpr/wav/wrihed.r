	subroutine wrihed(key)
	implicit integer*4 (i-n)
#**********************************************************************
# this routine is for writing the header information of the wavelength
# file in the file called '.wvhed' lun 14.
#**********************************************************************
#
	include "../common/spmaxes"   # max parameters, must be first

	include "../common/label1"
	include "../common/lblg"
	include "../common/lundefs"

	integer*2 ibuf(257)
	integer*4 ier

	equivalence (ibuf,ititl)

	if (key<=0||key>99) key = 1
	lun = 14
	if (nchans>256) nchans = 256
	if (nchans<2) nchans = 2
	nchsav = nchans
	isave = ibuf(257)
	ibuf(257) = nchsav
	write(lun,rec=key,iostat=ier)ibuf
	ibuf(257) = isave
	if (ier!=0) write(ttyout,10) ier
	return

10      format(' write error..ier=',i10)

	end

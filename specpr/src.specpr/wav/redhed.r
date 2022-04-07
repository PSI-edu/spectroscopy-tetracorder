	subroutine redhed(n,key,ier)
	implicit integer*4(i-n)
#
#***********************************************************************
#   this routine is for reading the header information of the wavelength
# 	file in the file called '.wvhed' lun wvhlun.
#       if n != 1 then read header and set nchans
#       if n == 1 then only set nchans.
#	ier is an error flag.
#		ier == 0 if no error
#		ier == -1 if eof
#		else if ier<100 it is the UNIX error number
#		else ier is the error number returned by the FORTRAN i/o lib.
#**********************************************************************
#
	include "../common/spmaxes"   # max parameters, must be first

	include "../common/blank"
	include "../common/label1"
	include "../common/lblg"
	include "../common/lundefs"

	integer*4 ier

	integer*4 ihbuf(256),ibuf(257)
	equivalence (ihbuf,ititl)
	equivalence (ibuf,datab)

	if (key<=0 || key>99) key = 1
	ier = 0
	read(wvhlun,rec=key,iostat=ier)ibuf
	if (ier==0) {
		if (n!=1)
			do i = 1,256
				ihbuf(i) = ibuf(i)
		nchans = ibuf(257)
	}
	return
	end

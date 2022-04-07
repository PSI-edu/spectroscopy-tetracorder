	subroutine setwpt (idv,iwv,irecw)
	implicit integer*4 (i-n)

#ccc  name: setwpt
#ccc  version date: 6/22/89
#ccc  author(s): Roger N. Clark
#ccc  language: ratfor
#ccc
#ccc  short description: given a data set to be written, check which
#ccc                     file, check wich wavelength file, and if
#ccc                     both are the same, set the wavelength pointer
#ccc                     in the data set header to equal the wavelength
#ccc                     record number.
#ccc
#ccc  algorithm description:
#ccc  system requirements:
#ccc  subroutines called:
#ccc  argument list description: idv = data set file id
#ccc                             iwv = wavelength set file id
#ccc                             irecw = wavelength set record number
#ccc  parameter description:
#ccc  common description: label1
#ccc  message files referenced: none
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc

	include "../common/spmaxes"   # max parameters, must be first

	include "../common/label1"
	include "../common/alphabet"
	include "../common/lundefs"

	integer*4 idv, iwv, irecw

	irwav = 0
	if ( (idv == ihv) & (iwv == ihcv) ) {
		irwav = irecw
		write (ttyout, 10) irwav
	}
	if ( (idv == ihw) & (iwv == ihcw) ) {
		irwav = irecw
		write (ttyout, 10) irwav
	}
	if ( (idv == ihd) & (iwv == ihcd) ) {
		irwav = irecw
		write (ttyout, 10) irwav
	}
	if ( (idv == ihu) & (iwv == ihcu) ) {
		irwav = irecw
		write (ttyout, 10) irwav
	}
	if ( (idv == ihy) & (iwv == ihcy) ) {
		irwav = irecw
		write (ttyout, 10) irwav
	}

10	format (' Wavelength pointer set to',i7)

	return
	end

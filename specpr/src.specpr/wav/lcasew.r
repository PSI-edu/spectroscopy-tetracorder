	subroutine lcasew (iwd)
	implicit integer*4 (i-n)

#ccc  name: lcasew
#ccc  version date: 4/16/85
#ccc  author(s): Roger N. Clark
#ccc  language: Ratfor
#ccc
#ccc  short description:change upper case wave id to lower case
#ccc
#ccc  algorithm description:
#ccc  system requirements: none
#ccc  subroutines called: none
#ccc  argument list description: iwd= integer value equal to letter code
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc

	include "../common/alphabet"
	include "../common/lundefs"


	if (iwd == ihcv) iwd = ihv
	if (iwd == ihcw) iwd = ihw
	if (iwd == ihcd) iwd = ihd
	if (iwd == ihcu) iwd = ihu
	if (iwd == ihcy) iwd = ihy
	if (iwd == 0 ) {
		write (ttyout,17)iwd
17		format ('invalid wavelength file id: ', a, /)
		return
	}

	return
	end

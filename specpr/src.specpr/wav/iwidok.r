	integer*4 function iwidok (it)
	implicit integer*4 (i-n)

#ccc  name: iwidok
#ccc  version date: 4/16/85
#ccc  author(s): Roger N. Clark
#ccc  language: Ratfor
#ccc
#ccc  short description: checks to see if filid id is ok for wavelengths
#ccc
#ccc  algorithm description:
#ccc  system requirements: none
#ccc  subroutines called: none
#ccc  argument list description: it= integer value equal to letter code
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

	iwidok = 0

	if ((it==ihcv)|(it==ihcw)|(it==ihcd)|(it==ihcu)|
					(it==ihcy)|(it==ihcc)) iwidok = 1

	return
	end

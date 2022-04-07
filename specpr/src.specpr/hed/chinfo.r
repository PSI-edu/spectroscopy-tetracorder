	subroutine chinfo(inxt)
	implicit integer*4 (i-n)

#ccc  name: chinfo
#ccc  version date: 5/10/85
#ccc  author(s): Roger N Clark
#ccc  language: Ratfor
#ccc
#ccc  short description: controls calling of subroutines for header
#ccc				information change
#ccc
#ccc  algorithm description: none
#ccc  system requirements: none
#ccc  subroutines called:
#ccc  argument list description: inxt
#ccc  parameter description:
#ccc  common description: none
#ccc  message files referenced: none
#ccc  internal variables:
#ccc  file description: none
#ccc  user command lines: none
#ccc  update information: none
#ccc  NOTES:
#ccc

	include "../common/spmaxes"   # max parameters, must be first

	include "../common/label1"
	include "../common/alphabet"

	integer*2 chkbit, ibit

1	inxt = 0
	call chginf(inxt)
	if (inxt == 2) call chgin1(inxt)

	ibit = 1
	if (chkbit(icflag,ibit) == 0) {
		if (inxt == 2) call chgin2(inxt)
		if (inxt == 3) call chgin3(inxt)
		if (inxt == 4) call chgin4(inxt)
	} else {
		if (inxt == 1) call chgtxt(inxt)
		if (inxt == 2) call chgin1(inxt)
	}

	if (inxt == ihe | inxt == ihx | inxt == ihg) return
	go to 1

	end

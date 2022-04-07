	subroutine getiopconchar(j,iichar)
	implicit integer*4 (i-n)

#ccc  version date: 12/04/1998
#ccc  author(s): Roger Clark
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc                   This subroutine gets a character from iopcon array
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc  argument list description:
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc

	include "../common/lbl4"

	integer*4 j
	character*1 iichar

	if (j < 1) {
		iichar=' '
	} else if (j > 80) {
		iichar=' '
	} else {
		iichar=iopcon(j:j)
	}
	return
	end

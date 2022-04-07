	subroutine sb (i)
	implicit integer*4 (i-n)

#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc                    texmod,vgrmod
#ccc  argument list description:
#ccc       argument: i
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc

	include "../common/hptrm"
	include "../common/xf"

	if (igrmod  >=  99) return
	if (igrmod >= 50 && igrmod <= 53) {
		xfwin=1
#XWIN		call xflush
		return
	}

	call texmod
	if (i == 0) return

	call vgrmod

	return
	end

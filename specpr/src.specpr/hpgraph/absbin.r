	subroutine absbin(inx,iny,ioutx,iouty)
	implicit integer*4(i-n)
#cc  version date: 02/04/86
#cc  author(s): roger clark & jeff hoover
#cc  language:  fortran
#cc
#cc  short description:
#cc
#cc  algorithm description: none
#cc  system requirements:   none
#cc  subroutines called:    none
#cc  argument list description:
#cc     arguments: inx,iny,ioutx,iouty
#cc  parameter description:
#cc  common description:
#cc  message files referenced:
#cc  internal variables:
#cc  file description:
#cc  user command lines:
#cc  update information:
#cc  notes:
#cc
	character*2 ioutx,iouty

	iix = ichar(' ')
	ioutx(2:2) = char(iix+mod(inx,32))
	ioutx(1:1) = char(iix+mod(inx/32,32))
	iouty(2:2) = char(iix+mod(iny,32))
	iouty(1:1) = char(iix+mod(iny/32,32))

	return
	end

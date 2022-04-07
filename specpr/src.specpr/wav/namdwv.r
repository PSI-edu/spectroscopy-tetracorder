	subroutine namdwv(idv,inam)
	implicit integer*4(i-n)
#cc  version date: 09/10/85
#cc  author(s): Roger N. Clark
#cc  language:  fortran
#cc
#cc  short description:
#cc                   this subroutine finds the tape name of a
#cc			wavelength file and puts it in inam
#cc  algorithm description: none
#cc  system requirements:   none
#cc  subroutines called:
#cc                    none
#cc  argument list description:
#cc        arguments: idv,inam
#cc  parameter description:
#cc  common description:
#cc  message files referenced:
#cc  internal variables:
#cc  file description:
#cc  user command lines:
#cc  update information:
#cc  notes:
#cc


	include "../common/lbl4"
	include "../common/alphabet"

	character*8 inam


	if (idv==ihcv) write(inam,10)isavt
	else if (idv==ihcw) write(inam,10)iwdgt
	else if (idv==ihcu) write(inam,10)inmu
	else if (idv==ihcy) write(inam,10)inmy
	else if (idv==ihcc) write(inam,12)
	else if (idv==ihcd) write(inam,10)iwrkt
	else inam = '        '

	return

10      format(a8)
12      format('Channel ')

	end

	subroutine namdev(idv,inam)
	implicit integer*4(i-n)
#cc  version date: 06/01/83
#cc  author(s): roger clark & jeff hoover
#cc  language:  fortran
#cc
#cc  short description:
#cc                   this subroutine does encodings
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


	if (idv==ihv) write(inam,10)isavt
	else if (idv==ihw) write(inam,10)iwdgt
	else if (idv==ihu) write(inam,10)inmu
	else if (idv==ihy) write(inam,10)inmy
	else if (idv==ihc) write(inam,12)
	else if (idv==ihs) write(inam,13)
	else if (idv==ihd) write(inam,10)iwrkt
	else inam = '        '

	return

10      format(a8)
12      format('constant')
13      format('starpack')

	end

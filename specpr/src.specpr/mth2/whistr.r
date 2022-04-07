	subroutine whistr(iper,cx)
	implicit integer*4(i-n)
#ccc  name: whistr
#ccc  version date: 6/3/85
#ccc  author(s): Roger N Clark
#ccc  language: Ratfor
#ccc
#ccc  short description: Derive history for simple math operations
#ccc
#ccc  algorithm description:
#ccc  system requirements:
#ccc  subroutines called:
#ccc  argument list description:
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  notes:
#ccc

	include "../common/spmaxes"   # max parameters, must be first

	include "../common/blank"
	include "../common/lbl6"
	include "../common/lbl4"
	include "../common/lblg"
	include "../common/label1"
	include "../common/alphabet"

	character*8 inam,inam2
	character*1 iper


	call namdev(idv1,inam)
	call namdev(idv2,inam2)
	if (idv2!=ihc)
		write(ihist,161)inam,ifl1,iper,inam2,ifl2,nchans
	else {
		write(ihist,163)inam,ifl1,iper,inam2,cx,nchans
		if (cx>0.9e+07||cx<0.9e-03)
			write(ihist,166)inam,ifl1,iper,inam2,cx,nchans
	}
	if (idv2==ihs) {
		write(ihist(32:35),201) ifl2
		ihist(36:60) = ihistc(1:25)
	}
#
#     starpack title was put in ihistc in division routine.
#
	return

161     format (a8,' rec',i6,1x,a1,a8,' rec',i6, '  channels=',i5)

163     format (a8,' rec',i6,1x,a1,1x,a8,1x,f15.7, ' channels=',i5)

166     format (a8,' rec',i6,1x, a1,1x,a8,1x,1pe14.7,' channels=',i5)

201     format (i4)

	end

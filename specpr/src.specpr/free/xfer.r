	subroutine xfer(inchar,ier)
	implicit integer*4 (i-n)
#ccc  name:
#ccc  version date:
#ccc  author(s):
#ccc  language:
#ccc
#ccc  short description:
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
#ccc  NOTES:
#ccc
#################################################################
#       this routine transfers a "rolling" command to           #
#       a perminant command.                                    #
#################################################################

	include "../common/cmd"
	include "../common/lbl4"
	include "../common/lundefs"

	ier = 0
	call wjfren(inchar,rec,il)
	irec = rec
	call wjfren(inchar,rec,il)
	irec2=rec
	if (irec >20 | irec < 1 | irec2 <= 20 | irec2 >100) {
		call what(inchar)
		print *,' input error ...'
		ier = 1
		return
	}
	is = icoman - irec
	if (is<=0) is = is + 20
	read(cmdlun,rec=is)iopcon
	write(cmdlun,rec=irec2)iopcon
	return
	end

	subroutine typein(inchar,ier)
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
#       this routine allows the user to type in a line of       #
#       the command file.                                       #
#################################################################
	include "../common/cmd"
	include "../common/lbl4"
	include "../common/lundefs"

	character*80 dummy

	ier = 0
	call wjfren(inchar,rec,il)
	irec = rec
	if (irec<21 | irec>101) {
		call what(inchar)
		print *,' input error ...'
		ier = 1
		return
	}
	write (ttyout,'("?",$)')
	read (ttyin,'(a)') dummy
	write(cmdlun,rec=irec) dummy
	return
	end

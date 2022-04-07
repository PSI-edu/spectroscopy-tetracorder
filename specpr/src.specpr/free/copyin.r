	subroutine copyin(inchar)
	implicit integer*4 (i-n)
#ccc  name: copyin
#ccc  version date: 2/23/88
#ccc  author(s): Roger N. Clark
#ccc  language: Ratfor
#ccc
#ccc  short description:  get file name to copy commands to
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
###########################################################
###     this routine determines the name of the file    ###
###     to copy the terminal input into.                ###
###########################################################
	include "../common/cmd"
	include "../common/lundefs"
#RED
	integer*4 lnb       # function lnb

	integer*4 ier, idummy
	logical*4 good

	call getstr(inchar,cfile,ier)
	if (ier==2 & copy) {
		close(cpylun,iostat=idummy)
		copy=.false.
	} else if (copy)
		print *,' ALREADY IN COPY MODE'
	else if (ier!=2) {
		inquire(file=cfile,exist=good)
		if (good) {
			write(ttyout,*) 'ERROR: File already exists:',cfile
			write(ttyout,*) '       - will not copy over it'
			copy = .false.
			return
		}
		open(cpylun,file=cfile,iostat=ier)
		if (ier==0) copy = .true.
		else {
			write(ttyout,10) cfile(1:lnb(cfile))
			copy = .false.
		}
	}
	return

10	format('CAN''T OPEN ',a,' for command copy output')
	end

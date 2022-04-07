	subroutine intchg(i,ivar,ier)
	implicit integer*4 (i-n)

#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc                   This subroutine retrieves an integer from iopcon.
#ccc                   if nonnumeric, ier=1. if first character is r
#ccc                   ( return ), ier=2. otherwise, the field passed as
#ccc                   ivar is updated and ier=0
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc                    wjfren
#ccc  argument list description:
#ccc      arguments: i,ivar,ier
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc
########################################################################
#                                                                      #
#     this routine retrieves an integer from iopcon.  if nonnumeric,   #
#     ier=1.  if 1st character is r (return),ier= 2.  otherwise, the   #
#     field passed as ivar is updated and ier=0.                       #
#                                                                      #
########################################################################

	include "../common/lundefs"
	include "../common/alphabet"

	call wjfren(i,x,im)
	if (im==ihr) {
		ier=2
		return
	}
	if (im!=0 || x<0) {
		write(ttyout,11)
		ier = 1
	} else {
		ivar = x
		ier = 0
	}
	return

11  format (' *** error:  non-numeric or negative ***,'/)
	end

	subroutine rlchng(i,var,im)
	implicit integer*4 (i-n)

#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc      This subroutine retrieves a real number ( f or e
#ccc      format ) from iopcon. If format error, ier=1. If
#ccc      first cha racter is r (return), ier= 2 and routine
#ccc      exits. Otherwise, the field passed as var is updated
#ccc      and ier = 0.
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc                    wjfren
#ccc  argument list description:
#ccc     arguments: i,var,ier
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc
#####################################################################
#                                                                   #
#     this routine retrieves a real number (f or e format) from     #
#     iopcon.  if format error, ier = 1.  if 1st character is       #
#     r (return), ier = 2 and routine exits.  otherwise, the field  #
#     passed as var is updated and ier = 0.                         #
#                                                                   #
#####################################################################

	include "../common/alphabet"

	data ihplus/'+'/
	im = 0
	call wjfren(i,x,im)
	if (im == ihca || im == ihw) {
		return
	}
	else {
		if (im==ihplus) call wjfren(i,x,im)
		bn = x
		isav = i
		if (im == 0) {
			i = isav
			var = bn
			return
		}
		if (im!=ihe && im!=ihce) {
			ier=1
			var=0
			return
		}
		call wjfren(i,x,im)
		isav = i
		if (im==ihplus) call wjfren(i,x,im)
		if (im==0) {
			isav = x
			if (isav>38) var=0
			else var = bn*(10.0**isav)
			return
		}
		i = isav
	}
	return
	end

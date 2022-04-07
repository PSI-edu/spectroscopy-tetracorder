	subroutine getdt (m,d,y,er)
	implicit integer*4 (i-n)

#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc           This subroutine receive date from iopcon, checks
#ccc           for validity. if valis er=0 . if not er=1. if
#ccc           r ( return ) is entered as first character, er=2.
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc                    wjfren
#ccc  argument list description:
#ccc       arguments: m,d,y,er
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc
####################################################################
#                                                                  #
#     subroutine retrieves date from iopcon, checks                #
#     that m is between 0 and 12, d is between 0 and 31.           #
#     if valid, er=0.  if not, er=1.  if r (return) is entered     #
#     as 1st character, er=2.                                      #
#                                                                  #
####################################################################

	integer*4 m,d,y,er
	include "../common/lundefs"
	include "../common/alphabet"


	i=1
	call wjfren (i,x,im)
	if (im!=0) {
		if (im==ihr) {
			er=2
			return
		}
110		write(ttyout,111)
		er=1
		return
	}
	if (x<0.0 || x>12.0) go to 110
	m=x
	call wjfren (i,x,im)
	if (im!=0 || x<0.0 || x>31.0) go to 110
	d=x
	call wjfren (i,x,im)
	if (im!=0) go to 110
	y=x
	er=0
	return

111	format (' *** date error ***',/)

	end

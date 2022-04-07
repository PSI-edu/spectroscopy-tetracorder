	subroutine dtchng(ivar,ier)
	implicit integer*4 (i-n)
#	%W%		%G% %U%
#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Rarfor
#ccc
#ccc  short description:
#ccc         This subroutine modifies the data stored in ivar
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc          getdt,crtin
#ccc  argument list description:
#ccc       arguments: ivar,ier
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc

####################################################
#                                                  #
#       modify the date stored in ivar.            #
#                                                  #
#   subroutines used:  getdt,crtin                 #
####################################################
	character*6 ivar

	call crtin
	call getdt(imo,idd,iyy,ier)
	if (ier!=0) return
	write(ivar,102) imo,idd,iyy
	return

102	format (3i2.2)

	end

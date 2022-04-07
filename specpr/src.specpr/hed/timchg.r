	subroutine timchg(ivar,ier)
	implicit integer*4 (i-n)
#	%W%		%G% %U%
#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc         This subroutine modifies the time stored in
#ccc         ivar.
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc          crtin,getim
#ccc  argument list description:
#ccc      arguments: ivar,ier
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
#                                                               #
#       this routine modifies the time stored in ivar.          #
#   subroutines used: getime                                    #
#                                                               #
#################################################################

	character*6 ivar

	integer*2 jhh, imm, iss, ier

	call crtin
	call getime(jhh,imm,iss,ier)
	if (ier!=0) return
	write(ivar,102) jhh,imm,iss
	return

102 format (3i2.2)

	end

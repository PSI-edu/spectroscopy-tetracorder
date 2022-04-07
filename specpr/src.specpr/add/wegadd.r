	subroutine wegadd(id)
#################################################################
#                                                               #
#       this is the main routine for the addition routine.      #
#                                                               #
#       modified: JAH 03-31-83          convert to ratfor &     #
#                               fix exit values when e or x     #
#                                                               #
#################################################################
	implicit integer*4 (i-n)

#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc                   This subroutine is the main routine for the
#ccc                   addition routine.
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc                    rstart,runads
#ccc  argument list description:
#ccc     arguments: none
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc

	include "../common/spmaxes"   # max parameters, must be first

	include "../common/lbl3"
	include "../common/alphabet"

	call runads(id)
	if (ictrl!=ihe) ictrl= 2
	else ictrl= 3
	if (id==ihe | id==ihx) ictrl= 0
	call rstart(1)
	end

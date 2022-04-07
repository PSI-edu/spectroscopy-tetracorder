	subroutine erored (icnt, ier,i)
	implicit integer*4 (i-n)

#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language: Fortran
#ccc
#ccc  short description:
#ccc                   This subroutine calls another subroutine ertyp
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc                     ertyp
#ccc  argument list description:
#ccc        arguments: icnt,ier,i
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc
	call ertyp('erored',0,ier)
	return
	end

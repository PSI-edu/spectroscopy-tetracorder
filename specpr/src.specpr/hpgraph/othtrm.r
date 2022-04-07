	subroutine othtrm
	implicit integer*4 (i-n)

#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc                   This subroutine scrolls up a few lines when hp
#ccc                   terminal is not in use
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:    none
#ccc  argument list description: none
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc
######################################################################
#     calling this subroutine indicates the hp terminal is not in use
#     so just scroll up a few lines
######################################################################
	include "../common/lundefs"

	write (ttyout,'(//////)')

	return
	end

	subroutine hpplot
	implicit integer*4 (i-n)

#ccc  version date: 02/04/86
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc                   This subroutine puts the hp in the plotting mode
#ccc			or other plotting device.
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
#########################################################
#     this subroutine puts the hp into plotting mode.
#########################################################

	include "../common/hptrm"
	include "../common/xf"

	if (igrmod >= 99) return
	if (imode==0) return
	if (igrmod < 20 ) {  #HP2623A
#
#  		   send esc *pai   to begin plot mode.
#
		ihpout(1:5) = char(27) // '*pai'

		iot= 5

	} else if (igrmod >= 20 && igrmod <= 22) {  # Tektronix Plot-10
		ihpout(1:1) = char(29)
		iot= 1
	} else if (igrmod >= 50 && igrmod <= 53) {
		xfwin = 1
	}
	ipen=0
	imode=0

	return
	end

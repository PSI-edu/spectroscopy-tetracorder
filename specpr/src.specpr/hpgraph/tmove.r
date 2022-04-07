	subroutine tmove
	implicit integer*4 (i-n)

#ccc  version date: 02/04/86
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc                   This subroutine moves the cursor to the absolute
#ccc                   position ix,iy on the hp graphics terminal.
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc                    convrt
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
####################################################################
#
#     this subroutine moves the cursor to the absolute position
#     ix,iy on the hp graphics terminal.
#
#     0 <or= ix <or= 720,      0 <or= iy <or= 360
#
#     out of range is not checked
#
#     escape sequence:   esc *d  ix iy  oz
####################################################################

	include "../common/hptrm"

	if (igrmod  >=  99) return

	ix= ixlast
	iy=iylast

	if (igrmod < 20) {   # HP2623A
		ihpout(1:4) = char(27) // '*d '

		call convrt (ix, ihpout(5:10), nchars)
		call convrt (iy, ihpout(11:16), nchars)
		ihpout(17:18) = 'oZ'
		iot = 18
		ii = iwrite(1,iot,ihpout)
		iot=0

	} else if (igrmod >= 20 && igrmod <= 22) {  # Tektronix Plot-10
		iot=0
	}

	return
	end

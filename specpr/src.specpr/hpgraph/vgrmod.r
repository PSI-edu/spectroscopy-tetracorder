	subroutine vgrmod
	implicit integer*4 (i-n)

#ccc  version date: 02/04/86
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc                   This subroutine sets the vector graphics
#ccc                   drawing mode
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
#################################################################
#     this subroutine sets the vector graphics drawing mode:
#          send esc *m <parameter> az
#################################################################
	include "../common/hptrm"
	include "../common/xf"
#RED
	integer*4 iwrite     # function iwrite

	if (igrmod >= 99) return
	if (igrmod >= 50 && igrmod <= 53) {   #  X-windows
		xfwin=1
		return
	}
	ihpout(4:4) = '4'
	if (igrmod < 20 ) {  #HP2623
		if (igrmod > 10) {
#
#     send  esc *dbz to set graphics display
#
			ihpout(1:6) = char(27) // '*d4bZ'
			if (igrmod==11) ihpout(4:4) = '1'
			if (igrmod==13) ihpout(4:4) = '3'
			iot = 6
			ii = iwrite(1,iot,ihpout)
		}
		ihpout(1:6) = char(27) // '*m4aZ'
		if (igrmod==3) ihpout(4:4) = '3'
		if (igrmod==2) ihpout(4:4) = '2'
		iot=6
		ii = iwrite(1,iot,ihpout)
	} else if (igrmod >= 20 && igrmod <= 22) {  #Tektronix Plot-10
		ihpout(1:1)= char(29)     #cntrl ]  (GS)
		iot=1
		ii=iwrite(1,iot,ihpout)
	}

	iot = 0
	return
	end

	subroutine texmod
	implicit integer*4 (i-n)

#ccc  version date: 02/04/86
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc                    tmove
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
################################################################
#     send az to stop plot mode if pen down, or z  if pen up.
################################################################

	include "../common/hptrm"
	include "../common/xf"
#RED
	integer*4 iwrite     # function iwrite

	if (igrmod  >=  99 | imode == 1) return
	if (igrmod >= 50 && igrmod <= 53) {   #  X-windows
		xfwin = 1
		return
	}
	if (igrmod < 20) {  # HP2623A
		if (ipen==0) {
			iot= iot+1
			ihpout(iot:iot) = 'Z'
		} else {
			ihpout(iot+1:iot+2) = 'aZ'
			iot= iot+2
		}
	} else if (igrmod >= 20 && igrmod <= 22) {  # Tektronix Plot-10
		iot = iot+1
		ihpout(iot:iot) = char(31)  # cntrl _ (US)
	}
	ii = iwrite(1,iot,ihpout)
	iot= 0
	call tmove
	imode=1
	ipen=0
	ixlast = -1
	iylast = -1

	return
	end

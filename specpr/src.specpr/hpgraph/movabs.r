	subroutine movabs (ix,iy)
	implicit integer*4 (i-n)

#ccc  version date: 02/04/86
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc                   This subroutine sends move coordinates
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc                    hpabs
#ccc  argument list description:
#ccc      arguments: ix,iy
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc

	include "../common/hptrm"

	if (igrmod >= 99) return
#
#  if already there, return
#
	if ((ix==ixlast) & (iy==iylast)) return

	if (igrmod < 20) {   # HP2623A
		if (imode==1) call hpplot
		if (ipen != 0) {
#
#    		 send lift pen: a
#
			iot= iot+1
			ihpout(iot:iot)= 'a'
			ipen= 0
		}
		call hpabs (ix,iy)
		ixlast=ix
		iylast=iy
#
#     		after the move coords are sent, the terminal automatically
#    		 lowers the pen.
#
		ipen=1
	} else if (igrmod >= 20 && igrmod <= 22) {   # Tektronix Plot-10
		if (imode==1) call hpplot
		if (ipen != 0) {   #lift pen signal
			iot= iot+1
			ihpout(iot:iot)= char(29)
			ipen =0
		}
		call hpabs(ix*2,int(float(iy)*1.94)+66)
		ixlast=ix
		iylast=iy
		ipen =1    # pen lowered by move vector
	} else if (igrmod >= 50 && igrmod <= 53) {   #  X-windows
		ixlast = ix
		iylast = iy
	}

	return
	end

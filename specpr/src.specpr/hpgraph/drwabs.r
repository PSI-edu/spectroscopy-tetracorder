	subroutine drwabs (ix,iy)
	implicit integer*4 (i-n)

#ccc  version date: 02/04/86
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc         This subroutine draws a vector to absolute position
#ccc         ix,iy in plotting mode if ix,iy is different from
#ccc         ixlast,iylast and the pen was already down
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc          hpplot,hpabs
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
####################################################################
#
#     this subroutine draws a vector to absolute position ix,iy
#     in plotting mode.
####################################################################

	include "../common/hptrm"
	include "../common/xf"

	if (igrmod >= 99) return
#
# if already there and pen was down, nothing to do, so return
#
	if ((ix==ixlast) & (iy==iylast) & (ipen==1)) return
	if (imode==1) call hpplot
	if (igrmod < 20) {   # HP2623A
		if (ipen!=1) {
#
#    		 send b
#
#      		   :puts pen down.
#
			iot= iot+1
			ihpout(iot:iot) = 'b'
			ipen=1
		}
		call hpabs (ix,iy)
		ixlast=ix
		iylast=iy
	} else if (igrmod >= 20 && igrmod <= 22) {  #Tektronix Plot-10
		if(ipen != 1) {  #pen is up, send coords of last point
			call hpabs(ixlast*2,int(float(iylast)*1.94)+66)
			ipen=1
		}
		call hpabs (ix*2,int(float(iy)*1.94)+66)
		ixlast=ix
		iylast=iy

	} else if (igrmod >= 50 && igrmod <= 53) {
#XWIN		call xdraw(ixlast,iylast,ix,iy)
		ixlast = ix
		iylast = iy
	}

	return
	end

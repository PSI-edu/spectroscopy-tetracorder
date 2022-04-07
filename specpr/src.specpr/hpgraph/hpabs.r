	subroutine hpabs (ix,iy)
	implicit integer*4 (i-n)
#ccc  name: hpabs
#ccc  version date: 02/04/86
#ccc  author(s): Roger N. Clark
#ccc  language: Ratfor
#ccc
#ccc  short description:
#ccc
#ccc  algorithm description:
#ccc  system requirements:
#ccc  subroutines called:
#ccc  argument list description:
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
#RED
	integer*4 iwrite      # function iwrite

	if (igrmod < 20) {  # HP2623A

		call absbin (ix,iy, ihpout(iot+1:iot+2),ihpout(iot+3:iot+4))
		iot=iot+4

	} else if (igrmod >= 20 && igrmod <= 22) {  # Tektronix Plot-10

		call tabbin (ix,iy, ihpout(iot+1:iot+2),ihpout(iot+3:iot+4))
		iot = iot+4

	}
	if (iot < 74) return
	ii=iwrite(1,iot,ihpout)
#
# delay for machines and terminals that need it
#
#TERMDELAY 	d = 1.23456
#TERMDELAY 	do iot=1,150
#TERMDELAY 		x = atan(d)
#TERMDELAY


	iot= 0

	return
	end

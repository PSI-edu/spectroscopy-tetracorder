	subroutine initt (ib)
	implicit integer*4 (i-n)

#ccc  version date: 02/04/86
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc                   If ib>=99: this is not the hp terminal-send no
#ccc                   graphic command
#ccc			ib       terminal supported
#ccc			<20       HP2623A
#ccc			 20	  Tektronix Plot-10 compatible
#ccc			 21	  Tektronix Plot-10 compatible on VT240
#ccc			 99	  generic alpha terminal
#ccc			100	  Televideo 914 (alpha only)
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc                    othtrm,respag
#ccc  argument list description:
#ccc       argument: ib
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc
########################################################################
#    if ib >=99: this is not the hp terminal - send no graphics commands
########################################################################

	include "../common/hptrm"
#RED
	integer*4 iwrite     # function iwrite

	iot=0
	igrmod=ib
	if (igrmod >= 99) { #not graphics terminal
		call othtrm
		return
	} else if (igrmod < 20) { # HP2623A
		ihpout = ' '
		call respag
#
#    		 set line type.
#
		ihpout(1:6) = char(27) // '*m0bZ'
		iot=6
		ii=iwrite(1,iot,ihpout)
		iot = 0
	} else if (igrmod == 20) { # Tektronix Plot-10
		ihpout(1:2) = char(27) // char(12)    # esc cntrl L
		iot = 2
		ii = iwrite(1,iot,ihpout)
		iot=0
	} else if (igrmod == 21 || igrmod == 22) { # Tektronix Plot-10 on vt240
		call respag
	} else if (igrmod == 50 || igrmod == 51 || igrmod == 52 || igrmod == 53 || igrmod == 60) {
		i = 0
		#write (6,*) "DEBUG in initt: igrmod =", igrmod
#XWIN		call xinit(igrmod, i)
		if (i == 0) {
			ib = -1
		}
	}
	return
	end

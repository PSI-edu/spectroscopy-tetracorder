	subroutine hreset (i)
	implicit integer*4 (i-n)

#ccc  version date: 02/04/86
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc                   Send esc E to reset terminal if i=0 otherwise
#ccc                   turn off graphic display and text and turn on
#ccc                   alpha display
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc                    othtrm,texmod
#ccc  argument list description:
#ccc      argument: i
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc
#######################################################################
#     send  esc e to reset terminal if i=0  otherwise turn off graphics
#     display and text and turn on alpha display.
#######################################################################

	include "../common/hptrm"
	include "../common/xf"
#RED
	integer*4 iwrite    # function iwrite

	integer*4 lone
	data lone/2/

	if (igrmod >= 99)  {
	    call othtrm
	    return
	}
	if (igrmod < 20 ) {  # HP2623A
		iot = 1
		call texmod
		if (i == 0) {
		    ihpout(1:2) = char(27) // 'E'
		    iot=2
		}
		else {
#
#    		 send  esc*ddet z
#
		    ihpout(1:7) = char(27) // '*ddetZ'
		    iot=7
		}

		ii=iwrite(1,iot,ihpout)
		iot = 0
		if (i != 0) return
#
#    		 delay cpu
#
		call fsleep(lone)

	} else if (igrmod >= 20 && igrmod <= 22) {  # Tektronix Plot-10
		call eralph
	} else if (igrmod >= 50 && igrmod <= 53) {  # X-window term
		xfwin = 0;
	}


	return
	end

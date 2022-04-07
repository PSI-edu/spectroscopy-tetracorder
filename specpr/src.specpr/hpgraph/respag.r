	subroutine respag
	implicit integer*4 (i-n)

#ccc  version date: 02/04/86
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc                   This subroutine resets the graphics on the
#ccc                   hp terminal or tektronix terminal
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc                    othtrm,vgrmod
#ccc  argument list description:
#ccc       argument: none
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
#
#     graphics on (c), graphics clear (a), alpha off (f), zoom=1 (1i),
#     graphics cursor on (k), graphics text on (s), rubber band line
#     off (n), cursor at top left line on screen (0 350o), and programs
#     this into function button f5 for use as a page button.
#
#     escape sequence:
#            esc *dcfa1iks0 350onz
#
#     also program f5:  esc &f5k1a18l esc *dcfa1iks0 350onz
#     also program f6:  esc &f6k1a7l esc *ddetZ
#
#     imode =1 :text
#           =0 :plotting
#
#     ixlast,iylast= last position of the plotting cursor.
#                    in the text mode it is the position of the cursor.
#
#     ipen =0 pen up
#          =1 pen down in plotting mode.
#
#
#     iot= number of words in ihpout-1 which have been loaded with
#          grapgics vector drawing commands.
#######################################################################

	include "../common/hptrm"
	include "../common/xf"
#RED
	integer*4 iwrite     # function iwrite

	if (igrmod >= 99) {
	    call othtrm
	    return
	}

	if (igrmod < 20) {   #HP2623A
		ihpout(1:18) = char(27) // '*dcfa1iks0 350onZ'

		ihpout(19:28) = char(27) // '&f5k1a18L'

		ihpout(29:46) = ihpout(1:18)

		ihpout(47:62) = char(27) // '&f6k1a7L' // char(27) // '*ddetZ'
		iot = 62
		ii = iwrite(1,iot,ihpout)

		imode=1
		ixlast=0
		iot=0
		iylast=350
		ipen= 0
		call vgrmod

	} else if (igrmod == 20 || igrmod == 22) {  #Tektronix Plot-10
		ihpout(1:2) = char (27) // char(12)   #esc cntrl L
		iot = 2
		ii = iwrite(1,iot,ihpout)

		imode=1
		ixlast=0
		iot=0
		iylast=350
		ipen= 0
		call vgrmod
	} else if (igrmod == 21) {  # Tektronix Plot-10 on VT240
		ihpout(1:6) = char(27) // '[?38h'     # esc [?38h
		iot = 6
		ii = iwrite(1,iot,ihpout)

		imode=1
		ixlast=0
		iylast=350
		iot=0
		ipen= 0
		call system ('sleep 1' // char(0))
	} else if (igrmod >= 50 && igrmod <= 53) {  #  X-windows
		xfwin=1
#XWIN		call xclear
		iot =0
		ixlast = 0
		#iylast=350  #was
		iylast=700
		ipen=0
	}
	return
	end

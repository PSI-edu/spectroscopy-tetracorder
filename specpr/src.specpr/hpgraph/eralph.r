	subroutine eralph
	implicit integer*4 (i-n)


#ccc  name: eralph
#ccc  version date: 02/04/86
#ccc  author(s): Roger N Clark
#ccc  language: Ratfor
#ccc
#ccc  short description: Erase alpha memory on HP2623, 2648A terminals, 
#ccc                     as well as other terminals (specifically defined).
#ccc
#ccc  algorithm description: send escape sequence
#ccc  system requirements: none
#ccc  subroutines called: othtrm
#ccc  argument list description: none
#ccc  parameter description: none
#ccc  common description: hptrm
#ccc  message files referenced: none
#ccc  internal variables:
#ccc  file description: none
#ccc  user command lines: none
#ccc  update information:
#ccc  NOTES:
#ccc

###############################################################
#
# defined terminals:
#
#	 igrmod <  99: HP2623 (and compatable)
#		=  99: unknown
#		= 100: Televideo 914
#
###############################################################

	include "../common/hptrm"
	include "../common/xf"
#RED
	integer*4 iwrite      # function iwrite

# HP2623:

	if (igrmod < 20 ) {
		call hreset(1)
		ihpout(1:4) = char(27) // 'H' // char(27) // 'J'	

#		program f5 and f6 keys same as in respage subroutine:

		ihpout(5:14) = char(27) // '&f5k1a18L'
		ihpout(15:32) = char(27) // '*dcfa1iks0 350onZ'
		ihpout(33:48) = char(27) // '&f6k1a7L' // char(27) // '*ddetZ'

		iot = 48
		ii = iwrite(1,iot,ihpout)
		return
	} else if (igrmod == 20) {  # Tektronix Plot-10
		ihpout(1:2) = char(27) // char(12)    # esc cntrl L
		iot = 2
		ii = iwrite(1,iot,ihpout)
		iot=0
		return
	} else if (igrmod == 21) {  # Tektronix Plot-10 on VT240
		ihpout(1:6) = char(27) // '[?38l'     # esc [?38l
		iot = 6
		ii = iwrite(1,iot,ihpout)
		iot=0
		return
	} else if (igrmod == 22) {  # Plot-10 for SunView GTERM
               ihpout(1:3) = char(24)//char(10)//char(12) 
               iot = 3
               ii = iwrite(1,iot,ihpout)
               iot=0
               return
	} else if (igrmod == 50) {  # X-windows hpterm 2622 emulator
		xfwin=0
		ihpout(1:4) = char(27) // 'H' // char(27) // 'J'	
		iot = 4
		ii = iwrite(1,iot,ihpout)
		iot=0
		return
	} else if (igrmod >= 51 && igrmod <= 53) {   # X-windows xterm emulator
		xfwin=0
		ihpout(1:7) = char(27) // '[H' // char(27) // '[2J'	
		iot = 7
		ii = iwrite(1,iot,ihpout)
		iot=0
		return
	} else if (igrmod == 100) { # televideo 914
		ihpout(1:3) = char(27) // '*0'
		iot = 3
		ii = iwrite(1,iot,ihpout)
		iot=0
		return
	}

# put additional terminals here, before else

	else {
		call othtrm
	}

	return
	end

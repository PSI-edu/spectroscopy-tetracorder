	subroutine serase(upx,upy,lwx,lwy)
	implicit integer*4 (i-q)
	integer*4 upx,upy,lwx,lwy

#ccc  version date: July 22, 1987
#ccc  author(s): Noel Gorelick
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc                   The routine selectivly erases the passed portion 
#ccc                   of the screen
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
#     this subroutine selectivly clears the given portion of the screen
#################################################################
	include "../common/hptrm"
#RED
	integer*4 iwrite     # function iwrite

	if (igrmod >= 99) return
	call sb(0)
	if (igrmod < 20) {  #HP-Terminals
		ihpout(1:5)=char(27) // '*m1a'
		write(ihpout(6:20),110) upx,upy,lwx,lwy
		ihpout(21:21)='E'
		iot=21
		ii = iwrite(1,iot,ihpout)
		iot=0
110	format (i3,',',i3,' ',i3,',',i3)
		call vgrmod
	} else if (igrmod >= 50 && igrmod <= 53) { # X-windows
#XWIN		call xerase(upx,upy,lwx,lwy)
	}
	return
	end

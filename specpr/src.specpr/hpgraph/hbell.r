	subroutine hbell (i)
	implicit integer*4 (i-n)

#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc                   This subroutine rings the bell i times. If i is
#ccc                   le.0.0 there is a one second delay between beeps.
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc                    texmod
#ccc  argument list description:
#ccc     argument: i
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
#     this subroutine rings the bell i times.
#
#     if i is le.0.0 there is a 1 second delay between beeps.
################################################################

	include "../common/hptrm"
#RED
	integer*4 iwrite      # function iwrite

	if (igrmod >= 99) return

	call texmod
	m= iabs(i)
	if (m == 0) return
	ihpout(1:1) = char(7)
	iot=1
	if (i < 0)  {
	     ihpout(2:3)= char(27) // '@'
	     iot= 3
	}
	do  j= 1,m {
		ii=iwrite(1,iot,ihpout)
		if (i > 0)  {
#
#     delay cpu so bell can sound
#
			do  l= 1,100
				a= 1.1*1.1
		}
	}
	iot = 0

	return
	end

	subroutine scrn03
	implicit integer*4 (i-n)

#ccc  version date: 05/06/85
#ccc  author(s): Roger Clark
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc                   This subroutine prints the third screen full
#ccc                   of information change routine.
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc                    none
#ccc
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
#                                                               #
#       this routine prints the third screenful of             #
#       information in the information change routine.          #
#                                                               #
#################################################################

	include "../common/spmaxes"   # max parameters, must be first

	include "../common/blank"
	include "../common/lundefs"
	include "../common/label1"
	include "../common/label3"
	include "../common/lbl6"

	real*8 xiangl,xeangl, phase

	intsph = 2000000000

	call eralph
	write (ttyout,1) idv1, ifl1, ititl
	write (ttyout,2) itchan, irwav, irespt, itpntr

	xiangl = dble(siangl)/21600000.0d0
	xeangl = dble(seangl)/21600000.0d0
	phase  = dble(sphase)/5400000.0d0

	if (siangl == intsph) {
		write (ttyout, 103)
	} else {
		write (ttyout,3) xiangl
	}
	if (seangl == intsph) {
		write (ttyout, 104)
	} else {
		write (ttyout,4) xeangl
	}
	if (sphase == intsph) {
		write (ttyout, 105)
	} else {
		write (ttyout,5) phase
	}
	write (ttyout,6) tempd
	write (ttyout,10)

	return

1	format (
'              header information display and change routine',/,
	  15x, 45(1h-), /, 15x, 'display of: ', a1, i6, 2x, a, /,
' enter the letter code of the information you wish to change', /)

2	format (' a:  number of channels in spectrum= ',i6, /,
' b:  wavelength record pointer= ', i6, /,
' c:  resolution (horizontal error bar) record pointer= ', i6, /,
' d:  text record pointer= ', i6,/)

3	format (' f:  angle of incidence = ',f10.6, ' degrees')
4	format (' h:  angle of emission  = ',f10.6, ' degrees')
5	format (' i:  phase angle = ', f11.6, ' degrees')
6	format (' t:  temperature = ', g13.7, ' degrees Kelvin',/)

103	format (' f:  angle of incidence = integrating sphere')
104	format (' h:  angle of emission  = integrating sphere')
105	format (' i:  phase angle = integrating sphere')

10	format (' type  g  to exit to crt plot,  ',
		'e  to soft exit with write but no plot,',/,
		'       x  to hard exit,  r  to return to ',
		'beginning of information routine',/,
		' or return to go to next page', /)

	end

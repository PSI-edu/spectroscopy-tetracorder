	subroutine scrn02
	implicit integer*4 (i-n)

#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc                   This subroutine prints the second scrfeen full
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
#       this routine prints the second screenful of             #
#       information in the information change routine.          #
#                                                               #
#################################################################

	include "../common/spmaxes"   # max parameters, must be first

	include "../common/blank"
	include "../common/lundefs"
	include "../common/label1"
	include "../common/label3"
	include "../common/lbl6"

	call eralph
	write (ttyout,1) idv1, ifl1, ititl
	write (ttyout,2) iband,xnrm,revs,scatim,timint,nruns,iwtrns,itimch
	return

1	format ('              ',
	'header information display and change routine',/,
	  15x, 45(1h-), /, 15x, 'display of: ', a1, i6, 2x, a, /,
' enter the letter code of the information you wish to change', //)

2	format (
'     a:  band normalization factor [band=(',i4,',',i4,')]=  ',g12.5,//,
' **note the next 3 are related:  revs x scantime = total integrating time',/,
'     b:  no. of revolutions (spectrum scans) =  ',i9,/,
'     c:  scan time (seconds) =  ',g12.5,/,
'     d:  total integrating time (seconds) =  ',g12.5,//,
'     f:  no. of runs =  ',i9,/,
'     w:  weighted no. of runs =  ',i9,/,
'     h:  time observed per object per half chop =  ',i9,' milliseconds',//,
' type  g  to exit to crt plot,  e  to soft exit with write but no plot,',/,
'  x  to hard exit  r  to return to beginning of header change routine',/,
'     press return to go to next page',/)

	end

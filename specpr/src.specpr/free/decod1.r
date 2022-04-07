	subroutine decod1(ipcnx,mx)
	implicit integer*4 (i-n)

#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc                   This subroutine decodes a digit 0 thru 9
#ccc                   from ipcnx to mx. mx is set to -1 if the
#ccc                   character is not a digit
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:   none
#ccc  argument list description:
#ccc      arguments: ipcnx,mx
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
#     this subroutine decodes a digit 1 thru 9 from ipcnx to mx.   #
#       mx is set to -1 if the character is not a digit            #
####################################################################

	include "../common/lundefs"
	character*1 ipcnx,nhnum(10)
	data nhnum/'0','1','2','3','4','5','6','7','8','9'/

	mx = -1
	do mi = 1,10
		if (ipcnx==nhnum(mi)) mx = mi-1

	if (mx==(-1)) write(ttyout,10)
	return

10  format(' improper decode')
	end

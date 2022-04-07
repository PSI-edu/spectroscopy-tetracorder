	subroutine whedr2
	implicit integer*4 (i-n)
##########################################################
#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark and Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc                   This subroutine writes wavelenth file and
#ccc                   channels in use
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc                    none
#ccc  argument list description:
#ccc        arguments: none
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc
##################################################################
	include "../common/lbl7"
	include "../common/lblg"
	include "../common/lundefs"

	write(ttyout,10) itrol(1), itrol(2), nchans

	return


10      format (//,'wavelength file  ', a1, i5, ',', i5, 1x,
		'channels in use', /,
		74(1h-))
	end

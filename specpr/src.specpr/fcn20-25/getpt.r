	subroutine getpt(ifd,x,y,xmin,ymin)
	implicit integer*4 (i-n)

#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc                   This subroutine
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc                    getpix
#ccc  argument list description:
#ccc     arguments: ifd,x,y,xmin,ymin
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc
	include "../common/tablet"

	call getpix(ifd,iix,iiy)

	iix = iix - xzero
	iiy = iiy - yzero
	x = (iix * costh) + (iiy * sinth)
	y = -(iix * sinth) + (iiy * costh)
	if (xscale != 0.0) x = x / xscale + xmin
	if (yscale != 0.0) y = y / yscale + ymin
	return
	end

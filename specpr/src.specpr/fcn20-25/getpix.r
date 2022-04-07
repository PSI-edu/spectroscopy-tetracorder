	subroutine getpix(ifd,iix,iiy)
	implicit integer*4 (i-n)

#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc                   This subroutine
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:    none
#ccc  argument list description:
#ccc     arguments: ifd,iix,iiy
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

#        equivalence (ix(1),xmsb),(ix(2),xlsb),(ix(3),ymsb),(ix(4),ylsb)
		i=iread(ifd,ix,4)
		iix = xlsb
		iiy = ylsb
		if (iix < 0) iix = iix + 256
		if (iiy < 0) iiy = iiy + 256
		iix = xmsb * 256 + iix
		iiy = ymsb * 256 + iiy
	return
	end

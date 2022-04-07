	subroutine devol (idev,ivol)
	implicit integer*4 (i-n)

#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language: Ratfor
#ccc
#ccc  short description:
#ccc       This subroutone checks the logical unit number idev
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc                     none
#ccc  argument list description:
#ccc          arguments: idev, ivol
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc
	include "../common/lblvol"
	include "../common/filenames"

	character*9 anull

	data anull/NULL/

	ivol = 1

	if (idev == 8 &
		ivfl==anull) ivol = 10
	if (idev == 7 &
		idfl==anull) ivol = 10
	if (idev == 9 &
		iwfl==anull) ivol = 10
	if (idev == 17 &
		isfl==anull) ivol = 10
	if (idev == 3 &
		iufl==anull) ivol = 10
	if (idev == 4 &
		iyfl==anull) ivol = 10
	return
	end

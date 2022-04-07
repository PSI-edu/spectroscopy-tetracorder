	subroutine chkdev
	implicit integer*4 (i-n)

#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc                   This subroutines checks for the device.
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc                    none
#ccc  argument list description:
#ccc     arguments: none
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc

	include "../common/spmaxes"   # max parameters, must be first

	include "../common/lbl4"
	include "../common/lblvol"
	include "../common/filenames"

	character*8     nassnd
	character*10    anull

	data anull /NULL/
	data nassnd /'*unasnd*'/

	if (ivfl==anull) isavt = nassnd
	else if (isavt==nassnd) isavt = ' '

	if (iwfl==anull) iwdgt = nassnd
	else if (iwdgt==nassnd) iwdgt = ' '

	if (idfl==anull) iwrkt = nassnd
	else if (iwrkt==nassnd) iwrkt = ' '

	if (iufl==anull) inmu = nassnd
	else if (inmu==nassnd) inmu = ' '

	if (iyfl==anull) inmy = nassnd
	else if (inmy==nassnd) inmy = ' '

	return
	end

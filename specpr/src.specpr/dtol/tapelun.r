	logical function tapelu(lun)
	implicit integer*4 (i-n)

#ccc  version date: 06/01/83
#ccc  author(s):  Roger Clark & Jeff Hoover
#ccc  language:   Ratfor
#ccc
#ccc  short description:
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc                    none
#ccc  argument list description:
#ccc     arguments: lun
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc
########################################################################
#                                                                      #
#      this routine returns .true. if the logical unit lun is		   #
#      assigned to a magnetic tape drive. Otherwise it returns		   #
#      .false.                                                         #
#                                                                      #
########################################################################

	include "../common/lblvol"
	include "../common/alphabet"
	include "../common/filenames"

	character*9     tape

	data tape/DTAPE/

	if (lun == 8) {
		tapelu=(tape==ivfl(1:9))
		return
	} else if (lun == 9) {
		tapelu=(tape==iwfl(1:9))
		return
	} else if (lun == 7) {
		tapelu=(tape==idfl(1:9))
		return
	} else if (lun == 3) {
		tapelu=(tape==iufl(1:9))
		return
	} else if (lun == 4) {
		tapelu=(tape==iyfl(1:9))
		return
	} else {
		tapelu=(.false.)
		return
	}
	end

	logical function tapevo(id)
	implicit integer*4 (i-n)

#ccc  version date: 06/01/83
#ccc  author(s):  Roger Clark & Jeff Hoover
#ccc  language:   Ratfor
#ccc
#ccc  short description:
#ccc      This subroutine returns .true. if the device whose
#ccc      id is assigned to a magtape drived. otherwise it
#ccc                   returns .false.
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc                    none
#ccc  argument list description:
#ccc     arguments: id
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
#      this routine returns .true. if the device whose id is 'id'      #
#      is assigned to a magnetic tape drive. Otherwise it returns      #
#      .false.                                                         #
#                                                                      #
########################################################################

	include "../common/lblvol"
	include "../common/alphabet"
	include "../common/filenames"

	character*9     tape

	data tape/DTAPE/

	if (id == ihv) {
		tapevo=(tape==ivfl(1:7))
		return
	} else if (id == ihw) {
		tapevo=(tape==iwfl(1:7))
		return
	} else if (id == ihd) {
		tapevo=(tape==idfl(1:7))
		return
	} else if (id == ihu) {
		tapevo=(tape==iufl(1:7))
		return
	} else if (id == ihy) {
		tapevo=(tape==iyfl(1:7))
		return
	} else {
		tapevo=(.false.)
		return
	}
	end

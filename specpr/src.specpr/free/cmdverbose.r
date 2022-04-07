	function cmdverbose (ii)
	implicit integer*4 (i-n)

	integer*4 cmdverbose

#ccc  version date: 3/24/94
#ccc  author(s): Roger Clark
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc         check or set ioutverbose flag
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
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

	include "../common/cmd"
	include "../common/iocontrol"

	integer*4 ii

	if (ii >= 0) {           # set the value
		ioutverbose = ii
		cmdverbose = ii
		return
	} else {

		if (redire) {
			if (ii == -1) {
				cmdverbose = ioutverbose
				return
			} else {
				cmdverbose = 0
				return
			}
		} else {
			cmdverbose = 0
			return
		}
	}
	end

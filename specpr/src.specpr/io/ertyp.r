	subroutine ertyp(str,idev,ier)
	implicit integer*4 (i-n)

#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc         This subroutine prints an error messege for various
#ccc         system errors
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc          crtin
#ccc  argument list description:
#ccc        arguments: str,idev,ier
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc
#########################################################
#                                                       #
#       this routine prints an error message for        #
#       various system errors.                          #
#                                                       #
#       arguments:                                      #
#          str          string to indicate where        #
#                       error occured (routine name)    #
#          idev         lun on which error occured      #
#          ier          error flags from io operation   #
#                       HP-UX dictates this be integer*4#
#                                                       #
#########################################################

	include "../common/lundefs"
	integer*4 ier

	character*(*)   str

         lunerror=ttyout
         if (ier!=0) write (lunerror,100) str,idev


	if (ier == 0) {
		return

	} else if (ier == 1) {
		print *,' ERROR: Not owner'

	} else if (ier == 2) {
		print *,' ERROR: No such file or directory'

	} else if (ier == 5) {
		print *,' I/O ERROR'

	} else if (ier == 6) {
		print *,' ERROR: No such device or device busy'

	} else if (ier == 9) {
		print *,' ERROR: Bad file number'

	} else if (ier == 13) {
		print *,' ERROR: Permission denied'

	} else if (ier == 17) {
		print *,' ERROR: File exists'

	} else if (ier == 20) {
		print *,' ERROR: Not a directory'

	} else if (ier == 21) {
		print *,' ERROR: Is a directory'

	} else if (ier == 23) {
		print *,' ERROR: File table overflow'

	} else if (ier == 24) {
		print *,' ERROR: Too many open files'

	} else if (ier == 25) {
		print *,' ERROR: Not a typewriter device'

	} else if (ier == 28) {
		print *,' ERROR: No space left on device'

	} else if (ier == 30) {
		print *,' ERROR: Read-only file system'

	} else if (ier == -1) {
		print *,' End of file'

	} else {
		print *,' Unexpected ERROR #',ier

	}
	print *,' Press return to continue'
	call crtin
	return
100	format(/,a,': ERROR on device',i3)
	end

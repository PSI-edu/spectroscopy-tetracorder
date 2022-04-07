      subroutine lundev (i,idletr,lun)
	implicit integer*4 (i-n)

#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Barry J. Middlebrook
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc                   This subroutine determines the device id letter code
#ccc                   from the device logical number
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc                     none
#ccc  argument list description:
#ccc          arguments:  i,idletr,lun
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
#    this routine determines the device logigal unit number from       #
#    the device id letter code.                                        #
#                                                                      #
#      arguments:                                                      #
#                                                                      #
#      i:      (input) flag for which units are valid.                 #
#                      0= all                                          #
#                      1= all but  u  and  y                           #
#                      2= all but  u,  y,  and  s                      #
#                      3= only  v and d                                #
#                      4= all but  s                                   #
#                      5= all but  w                                   #
#                      6= all but  w  and  s                           #
#    only 0,4 should be currently used, others are from old systems.   #
#                                                                      #
#      idletr: (output) id letter of requested device                  #
#                                                                      #
#      lun:    (input) logical unit number of device                   #
#                                                                      #
########################################################################
	include "../common/alphabet"

	   if (lun == 8) {
		idletr = ihv

	   } else if (lun == 7) {
		idletr = ihd

	   } else if (lun == 9) {
		idletr = ihw

	   } else if (lun == 17) {
		idletr = ihs

	   } else if (lun == 3) {
		idletr = ihu

	   } else if (lun == 4) {
		idletr = ihy

	   } else {
		WRITE (6,*)'lundev: ERROR - Unit number not set'
	}

	RETURN
	END

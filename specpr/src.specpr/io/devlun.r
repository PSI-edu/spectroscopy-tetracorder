      subroutine devlun (i,idletr,lun)
	implicit integer*4 (i-n)

#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc                   This subroutine determines the device logical
#ccc                   unit number from the device id letter code
#ccc                   added capitol letters 12/22/2009
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
#    only 0,4 should be currently used others are from old systems.    #
#                                                                      #
#      idletr: (input) id letter of requisted device                   #
#                                                                      #
#      lun:    (output)logical unit number of device                   #
#                                                                      #
########################################################################
	include "../common/alphabet"

	lun =0

	if (idletr == ihv)       lun = 8
	if (idletr == ihcv)      lun = 8

	if (idletr == ihd)       lun = 7
	if (idletr == ihcd)      lun = 7

	if (idletr == ihw)       lun = 9
	if (idletr == ihcw)      lun = 9

	if (idletr == ihs)       lun = 17

	if (idletr == ihu)       lun = 3
	if (idletr == ihcu)      lun = 3

	if (idletr == ihy)       lun = 4
	if (idletr == ihcy)      lun = 4

	if (i==4 & lun==17) lun = 0

	return
	end

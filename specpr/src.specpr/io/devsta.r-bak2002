	subroutine devsta (idev,ista,ire,iprt)
	implicit integer*4 (i-n)

#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc                   This subroutine determines the device protection
#ccc                   and status
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc                    none
#ccc  argument list description:
#ccc       arguments: idev,ista,ire,iprt
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
#      this routine determines the device protection and status        #
#                                                                      #
#      arguments                                                       #
#        idev          input   logical unit number of device           #
#        ista          output  device status < 0 if /dev/null          #
#                              or if illegal device                    #
#        ire           input   return flag,if == 1 skip error msgs.    #
#        iprt          output  protection of the device                #
#                                                                      #
########################################################################

	include "../common/lblvol"
	include "../common/lblprt"
	include "../common/labelf"
	include "../common/lundefs"
	include "../common/filenames"

	character*9     anull

	data anull/NULL/

	ista=-4


		if (idev == 3) {
			iprt = iprtu
			if (iufl!=anull) ista=isvcu

		} else if (idev == 4) {
			iprt = iprty
			if (iyfl!=anull) ista=iwjcy

		} else if (idev == 7) {
			iprt = iprtd
			if (idfl!=anull) ista=iwrkf

		} else if (idev == 8) {
			iprt = iprtv
			if (ivfl!=anull) ista=isavf

		} else if (idev == 9) {
			iprt = iprtw
			if (iwfl!=anull) ista=iwjf

		} else if (idev == 17) {
			iprt = iprts
			if (isfl!=anull) ista=istrf

		} else {
			ista = -2
			iprt = -2
		}
	if (ire == 1) return
	if (ista == -4) {
		write(ttyout,1)
		return
	}
	if (ista == -2) {
		ista = -4
		write(ttyout,2)
		return
	}
	if (ista > 0) return
#
# if get to here something wrong
#
	write (ttyout,3) idev,ista,iprt
	call crtin
	ista = -4
	return
1       format(' *** ERROR: FILE ID is NOT ASSIGNED ***')
2       format(' *** ERROR: ILLEGAL DEVICE ***')
3	format(' *** ERROR: on logical unit',i3,5x,'status:',i5,
		5x,'protection=',i8,/,
		' PRESS RETURN to continue')
	end

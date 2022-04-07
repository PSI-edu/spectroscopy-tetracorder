      subroutine devok(ivalid,ifilid,ifile,lun,ier)
	implicit integer*4 (i-n)

#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Fortran
#ccc
#ccc  short description:
#ccc                    This subroutine calls necessary routines to see
#ccc                    if ok to read in a file. retrurns ier=0 if ok
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc                    devlun,devsta
#ccc  argument list description:
#ccc         arguments: ivalid,ifilid,ifile,lun,ier
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc

#c     *********************************************************
#c     *
#c     * routine calls necessary routines to see if ok to read
#c     * in a file. returns ier=0 if ok.
#c     *
#c     * input parameters:
#c     *   ivalid - flag for following units...
#c     *            0 = all
#c     *            1 = all but u and y
#c     *            2 = all but u,y,and s
#c     *            3 = only v and d
#c     *            4 = all but s
#c     *            5 = all but w
#c     *            6 = all but w and s
#c     *   ifilid - file id
#c     *   ifile  - file number
#c     *
#c     * output parameters:
#c     *   lun    - logical unit number
#c     *   ier    - error flag...
#c     *            0 = ok
#c     *            1 = status bad
#c     *            2 = protection violation
#c     *            3 = both 1 and 2
#c     *
#c     *********************************************************

	include "../common/iocontrol"

      ier = 0

#c     *** get logical unit number, status and priority ***
      call devlun(ivalid,ifilid,lun)
      call devsta(lun,ista,0,iprt)

#c     *** set proper return error flag ***
      if ((ista .gt. 0) .and. (ifile .ne. 0)) go to 10
      ier = 1
      if ((iprt .ne. -1) .and. (ifile .gt. iabs(iprt))) ier = 3
      go to 20
10    if ((iprt .ne. -1) .and. (ifile .gt. iabs(iprt))) ier = 2

20	if (ier == 0) {
		idwcon = ifilid
	} else {
		idwcon = 0
	}
	
	return
	end

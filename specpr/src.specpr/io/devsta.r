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
	integer*4	fsize, filsiz, newprot, ier, irecl

	data anull/NULL/

	ier = 0
	irecl = 1536
	ista=-4


		if (idev == 3) {       # file u
			iprt = iprtu
			if (iufl!=anull) {
				ista=isvcu
				if (ufollow == 1) {    # evaluate for growing file
					filsiz = fsize(iufl)
					filsiz = (filsiz-1536)/1536 # number of specpr records in file
					if (filsiz > abs(iprt)) {
					   iprtu = -1 * filsiz
					   write (ttyout, 23) iprtu
23					   format (' DEBUG: file u grew, now ', i6)
					   iprt  = iprtu
					}
				}
			}

		} else if (idev == 4) {       # file y
			iprt = iprty
			if (iyfl!=anull) {
				ista=iwjcy
				if (yfollow == 1) {    # evaluate for growing file
					filsiz = fsize(iyfl)
					filsiz = (filsiz-1536)/1536 # number of specpr records in file
					if (filsiz > abs(iprt)) {
					   iprty = -1 * filsiz
					   write (ttyout, 24) iprty
24					   format (' DEBUG: file y grew, now ', i6)
					   iprt  = iprty
					}
				}
			}

		} else if (idev == 7) {       # file d
			iprt = iprtd
			if (idfl!=anull) {
				ista=iwrkf
				if (dfollow == 1) {    # evaluate for growing file
					filsiz = fsize(idfl)
					filsiz = (filsiz-1536)/1536 # number of specpr records in file
					if (filsiz > abs(iprt)) {
					   iprtd = -1 * filsiz
					   write (ttyout, 27) iprtd
27					   format (' DEBUG: file d grew, now ', i6)
					   iprt  = iprtd
					}
				}
			}

		} else if (idev == 8) {       # file v
			iprt = iprtv
			if (ivfl!=anull) {
				ista=isavf
				if (vfollow == 1) {    # evaluate for growing file
					#write (ttyout,1001)
1001					format (' DEBUG: in devsta, vfollow == 1')
					filsiz = fsize(ivfl)
					filsiz = (filsiz-1536)/1536 # number of specpr records in file
					#write (ttyout,1002) iprtv, filsiz
1002					format (' DEBUG: in devsta, vfollow == 1  iprtv=',i6,' size=',i6)
					if (filsiz > abs(iprt)) {
					   iprtv = -1 * filsiz
					   write (ttyout, 28) iprtv
28					   format (' DEBUG: file v grew, now ', i6)
					   iprt  = iprtv

						# need to close and reopen (linux mint 2020)
						
						close (vlun, iostat=ier)
						if (ier != 0) {
						  write (ttyout,2001)  ier, ivfl
2001						  format ('devsta: close error',i6,' on ',a)
						}
						open(vlun,file=ivfl,iostat=idummy,
						    access='direct',recl=irecl,form='unformatted')
						if (ier != 0) {
						  write (ttyout,2002) ier, ivfl
2002						  format ('devsta: reopen error',i6,' on ',a)
						  ivfl = anull
						}
					}
				}
			}

		} else if (idev == 9) {       # file w
			iprt = iprtw
			if (iwfl!=anull) {
				ista=iwjf
				if (wfollow == 1) {    # evaluate for growing file
					filsiz = fsize(iwfl)
					filsiz = (filsiz-1536)/1536 # number of specpr records in file
					if (filsiz > abs(iprt)) {
					   iprtw = -1 * filsiz
					   write (ttyout, 29) iprtw
29					   format (' DEBUG: file w grew, now ', i6)
					   iprt  = iprtw
					}
				}
			}

		} else if (idev == 17) {
			iprt = iprts
			if (isfl!=anull) {
				ista=istrf
			}

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

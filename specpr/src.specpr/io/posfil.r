	subroutine posfil(idev,n,tape,ier)
	implicit integer*4 (i-n)

#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc          This subroutine positions the magnetic tapes
#ccc          at the proper record positions for input/output
#ccc  algorithm description:  none
#ccc  system requirements:    none
#ccc  subroutines called:
#ccc                    rewinf,bkreck,frdrec
#ccc  argument list description:
#ccc         arguments: idev,n,tape,ier
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc
#################################################################
#                                                               #
#       this routine positions the magnetic tapes at            #
#       the proper record positions for input/output            #
#                                                               #
#       arguments                                               #
#         idev          device logical number                   #
#         n             record number where i/o is to be done   #
#         tape          logical flag to calling routine         #
#                               tape == .true. if magtape       #
#         ier           error flag to calling routine           #
#                       ier = 0   ---  normal return            #
#                       ier = -1  ---  bad parameters (invalid  #
#                                       (lun, n<=0, etc.)       #
#                       other     ---  unix error # from        #
#                                       system call             #
#################################################################

	include "../common/lblvol"
	include "../common/labelf"
	include "../common/lundefs"
	include "../common/filenames"


	character*9 magtap
	logical tape

	data magtap/DTAPE/

	ier = 0

	tape = .false.
	if (idev == 3) {
			tape = magtap==iufl
			ipos = isvcu

		} else if (idev == 4) {
			tape = magtap==iyfl
			ipos = iwjcy

		} else if (idev == 7) {
			tape = magtap==idfl
			ipos = iwrkf

		} else if (idev == 8) {
			tape = magtap==ivfl
			ipos = isavf

		} else if (idev == 9) {
			tape = magtap==iwfl
			ipos = iwjf

		} else if (idev == 17) {
			tape = magtap==isfl
			ipos = istrf

		} else {
			ier = -1
			write(ttyout,5) idev
5			format(' invalid lun =',i6)
			return
	}

	if (!tape) return

	if (n <= 0) {
		write(ttyout,20) n
20		format(' **record number less than 1:',i6,'**'/)
		ier = -1
	}

	else if (n == 1) {
		call rewinf(idev,irer)
		ier = ier
	}

	else if (n < ipos)
		call bkrec(idev,ipos-n,ier)


	else if ((n-ipos)!=0)
		call frdrec(idev,n-ipos,ier)


	return
	end

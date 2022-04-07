	subroutine tcheck (igo,ifilid,ifildt,ifiln,iflnu,iln,lunt,inames)
	implicit integer*4 (i-n)

#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc         This subroutine checks for proper record number
#ccc         in transfer from tape to disk and disk to tape.
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc          crtin,devsta
#ccc  argument list description:
#ccc     arguments: igo,ifilid,ifildt,ifiln,iflnu,iln,lunt,inames
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
#    check for proper record numbers in transfer from tape to disk and #
#    disk to tape                                                      #
#                                                                      #
#      arguments:                                                      #
#                                                                      #
#      igo:    (output)statement label  ******KLUDGE******             #
#                      to do next in calling routine                   #
#      ifilid: (input) file id of "from" device                        #
#      ifildt: (input) file id of "to" device                          #
#      ifiln:  (input) record number for "to" device                   #
#      iflnu:  (input) record number for "from" device                 #
#      iln:    (input) transfer counter. =1 for first file, =2 for     #
#                      second, etc.                                    #
#      lunt:   (input) logical unit number of "to" device              #
#      inames: (input) =1 iff tape names are equal                     #
#                                                                      #
########################################################################

	include "../common/lundefs"
	logical tapevo

	character*1 icrrt

	icrrt=char(13)   # carriage return character

	write(ttyout,10) icrrt,ifilid,iflnu,ifildt,ifiln
#
	if (inames!=0) {

		if (tapevo(ifildt) && (ifiln!=iflnu)) {
#
#     disk to tape transfer
#
			write(ttyout,20)
			call crtin
			igo = 399
			return
		}
                else if (tapevo(ifilid) .eqv. .true. && tapevo(ifildt) .eqv. .false. && ifiln!=iln) {


#
#       tape to disk transfer
#
			write(ttyout, 30)
			call crtin
			igo = 399
			return
		}
	}
	call devsta (lunt,ista,1,iprt)
	if (iprt<=-2) write(ttyout,40)
	if (iprt<=-2 || (iprt!=-1 && ifiln!=iprt+1)) {
		write(ttyout, 50)
		call crtin
		igo = 399
		return
	}
	igo = 801
	return

%10	format (a,'transferring ', a,i4,' to ',a,i4,40(1h ),$)

20      format (' *** transfer to a file with the same name must have',
		' files transferred to their corresponding',            /,
		'          record number on tape ***.',                 //,
		' press return to continue:',                           /)

30      format ('*** transferring files with the same names must start',
		' with file 1, otherwise',                              /,
		'          the file counter will be wrong****',         //,
		' press return  to continue')

40      format (' *** transfer to a read only device ***')

50      format (' *** illegal file transfer ***',                       //,
		' press return to continue')

      end

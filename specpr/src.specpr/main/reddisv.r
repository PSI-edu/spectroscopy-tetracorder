	subroutine reddis(idvx)
	implicit integer*4 (i-n)
#################################################################
#                                                               #
#       This is the main routine for the file transfer          #
#       display and math operations.                            #
#                                                               #
#       AUTHOR: JAH  ??-??-??                                   #
#       Modified: JAH 02-28-83  call rstart in loop             #
#                                                               #
#################################################################

	include "../common/alphabet"

	integer*4 idvx, i


	if (idvx ==  iht | idvx == ihy | idvx == ihd | 
			idvx == ihu | idvx == ihv | idvx == ihw) {
		call disv(idvx)

	} else if (idvx ==  ihm) {
		call redwg2(idvx)

	} else if (idvx ==  ihe | idvx == ihx) {
		call rstart(1)
		return

	} else {
		write (*,10) idvx
10		format (a1,' is not a legal command here')
		i=1
		call what(i)
		call rstart(1)
		return
	}
	call rstart(1)

	end

	subroutine flname (i,iopcon,namfil,err)
	implicit integer*4 (i-n)
#ccc  name: flname.r
#ccc  version date: 08/05/85
#ccc  author(s): Kathy Kierein
#ccc  language:  Ratfor
#ccc
#ccc  short description:  This subroutine finds a filename using
#ccc			  iopcon in listfl.r
#ccc
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called: none
#ccc
#ccc  argument list description:
#ccc			begfil=beginning location of filename
#ccc			endfil=ending location of filename
#ccc			sizfil=size of filename
#ccc			namfil=filename
#ccc			err=error indicator
#ccc
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc

	include "../common/lundefs"

	integer*4 begfil, endfil, sizfil, err
	character*80 namfil, iopcon

	err = 0
#
# This finds the first letter in the filename
#
# RED   Initialized to 0
	begfil = 0
	do j=i+1,80 {
		if (iopcon(j:j) != ' ') {
			begfil = j
		}else{
			next
		}
		break
	}
#
# This gives an error when the filename is not given
#
	if (j == 81) {
		write (ttyout,12)
12		format ('Error, filename not entered.')
		err = 1
		return
	}
#
# This finds the last letter in the filename
#
# RED   Initialize to 0
	endfil = 0
	do k=j+1,80 {
		if (iopcon(k:k) == ' ') {
			endfil = k
		}else{
			next
		}
		break
	}
#
# This finds the filename and the new beginning point for searching
# the line using wjfren
#
	sizfil = endfil - begfil 
	namfil(1:sizfil) = iopcon(begfil:endfil)
	i = endfil + 1

	return
	end

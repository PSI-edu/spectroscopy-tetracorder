	subroutine wcomma (namfil)
	implicit integer*4 (i-n)
#ccc  name: wcomma.r
#ccc  version date: 08/05/85
#ccc  author(s): Kathy Kierein
#ccc  language:  Ratfor
#ccc
#ccc  short description:  This subroutine determines which type of
#ccc			  file to open and opens the file. It is used
#ccc			  in listfl.r to write a comma separated list
#ccc			  to the file
#ccc
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc			wjfren.r, crtin.r
#ccc  argument list description:
#ccc				ex=logical to determine if file exists
#ccc				namfil=filename to write to
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

	include "../common/spmaxes"   # max parameters, must be first

	include "../common/lbl4"
	include "../common/lundefs"
	include "../common/alphabet"

	logical*4 ex
	character*80 namfil

	i = 1

#
# This determines if the file already exists
#
	inquire (file = namfil, exist = ex)
#
# If the file exists, this opens it either old or unknown.  If it is
# opened old, the new data will be added to the end of the file.  If
# it is opened unknown, the new data will overwrite the old data.
#
	if (ex) {
		write (ttyout,10) namfil
10		format ('File ',a,' exists.  Default(return) will append',
			' new information onto end of file.  Enter o',
                        ' to write over existing file.')

		while (i <= 80) {
			call crtin
			call wjfren (i, x, il)
			if (il == iho) {
			    open(wrtlun,file=namfil,iostat=ier,
				access='sequential',form='formatted',
				status='unknown')
				go to 100
			}else{
				next
			}
		}
		open(wrtlun, file=namfil, iostat=ier, status='old',
			access='sequential', form='formatted')

		do j=1,10000 {
			read (wrtlun,20,end=100) iopcon
20			format (a)
		}

	}else{
		write (ttyout,30) namfil
30		format ('File ',a,' is a new file.')
		open (wrtlun, file=namfil, iostat=ier, status='new',
			access='sequential', form='formatted')
 	}
#
# This check for errors in opening the file
#
100	if (ier != 0) {
		write (ttyout,40) namfil
40		format ('Error in opening file ',a)
	}

	return
	end

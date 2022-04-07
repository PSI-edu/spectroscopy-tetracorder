	subroutine flushseqfile (lun, filename, ier)

        implicit integer*4(i-n)

#ccc  name:         tri1setup
#ccc  version date:
#ccc  author(s): Roger N. Clark
#ccc  language: ratfor
#ccc
#ccc  short description: close a sequential file, then
#ccc                     reopen and position to end of file
#ccc
#ccc  algorithm description:
#ccc  system requirements: Unix
#ccc  subroutines called: 
#ccc  argument list description:
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced: none
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines: none
#ccc  update information:
#ccc  NOTES:

	include "../specpr/src.specpr/common/spmaxes"   # max parameters, must be first

	include "../specpr/src.specpr/common/lundefs"

	include "multmap.h"

	integer*4 lun
	character*(*) filename
	integer*4 ier
	character*1024 txt


	if (imatenable(imat) == 0) {   # material not enabled, so do nt open a file

		#if (cmdverbose(-1) <= 1) write (ttyout,*) 'NOTE: material',imat,' is disabled, no buffer to flush'
		return
	}

	close (lun, iostat=ier)

        if (ier != 0) {
		write (ttyout,7) ier, filename
7		format (' CLOSE ERROR',i5,' on file:',/,a)
                return
        }

        open (unit=lun, file=filename,
                access='sequential', form='formatted',
                status='old', iostat=ier)

        if (ier != 0) {
                write (ttyout,187) ier, filename
187             format (' OPEN ERROR',i5,' on file:',/,a)
                return
        }

	do i = 1, 9999999 {

		read (unit=lun, iostat=ier, fmt=200, end=1000) txt
200		format (a)

		if (ier != 0) {
			write (ttyout, 250) ier, i, filename
250			format (' ERROR positoning at end of file:',/,
				' ERROR=', i6, 5x,'line=',i7,5x,
				'filename:', /,a)
			return
		}
	}

1000	ier = 0
	return
	end

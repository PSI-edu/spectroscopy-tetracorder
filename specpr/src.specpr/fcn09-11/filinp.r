	subroutine filinp(ifilid,ifilnm,errs,ic)
	implicit integer*4 (i-n)

#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc                   This subroutine inputs a file into <data> given
#ccc                   file id and number. returns a logical flag errrs
#ccc                   which is false if no errors, and a flag ic, which
#ccc                   is 0 if every thing is ok, an x if not.
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc                    devok,redfil
#ccc  argument list description:
#ccc     arguments: ifilid,ifilnm,errs,ic
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc

#     ***************************************************************
#     *
#     * routine inputs a file into <data> given file id and number.
#     * returns a logical flag, errs which is false if no errors,
#     * and an flag, ic, which is 0 if everything's ok, an x if not.
#     *
#     ***************************************************************

	include "../common/alphabet"
	include "../common/lundefs"

	logical errs


	errs = .true.
	ic = 0

#     *** call routine to get logical unit number for device ***
#     *** then return error flag (ier)                       ***
	call devok(4,ifilid,ifilnm,lun,ier)
	if (ier == 0) {
		call redfil(ifilnm,lun,ier)
		if (ier == 0) {
			errs = .false.
		} else {
			write(ttyout,10)
			ic = ihx
		}
	} else if (ier == 1) {
		write(ttyout,20)
	} else if (ier == 2) {
		write(ttyout,30)
	} else {
		write(ttyout,40)
	}

	return
10      format(' i/o error. hard exit.'/)
20      format(' *** illegal status ***')
30      format(' *** protection violation ***')
40      format(' *** illegal status and protection violation ***')
	end

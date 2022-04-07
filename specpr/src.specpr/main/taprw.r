	subroutine taprw
	implicit integer*4 (i-n)

#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc         This subroutine deassigns the magtapes so that
#ccc         specpr can restart without crashing
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc                    none
#ccc  argument list description:
#ccc       arguments: none
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc
#########################################################
#                                                       #
#       this routine deassigns the mag tapes            #
#       so that specpr can restart without              #
#       crashing.                                       #
#                                                       #
#########################################################

	include "../common/lblvol"
	include "../common/lundefs"
	include "../common/filenames"


	character*9     tape
	character*9     anull

	logical         chnged


	data    tape    /DTAPE/
	data    anull    /NULL/

	chnged = .false.

	if (ivfl==tape) {
		ivfl=anull
		chnged = .true.
	}

	if (iwfl==tape) {
		iwfl=anull
		chnged = .true.
	}

	if (idfl==tape) {
		idfl=anull
		chnged = .true.
	}

	if (iufl==tape) {
		iufl=anull
		chnged = .true.
	}

	if (iyfl==tape) {
		iyfl=anull
		chnged = .true.
	}

	if (chnged) write(ttyout,10)
10	format(' *** warning *** tapes were deassigned'/)

	return
	end

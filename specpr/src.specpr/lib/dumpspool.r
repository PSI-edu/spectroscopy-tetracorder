	subroutine dumpsp
	implicit integer*4 (i-n)

#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc         This subroutine causes the spooler to print the
#ccc         spoolfile
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc          tname,setfil
#ccc  argument list description:
#ccc       argument: none
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
#       this routine causes the spooler to print the spoolfile
########################################################################

	include "../common/lblvol"
	include "../common/lundefs"
	include "../common/filenames"
	include "../common/spool"

	character*26    c,t
	character*8	user
	integer*4		fsize
	integer*4	idummy

	data 	t 	/SPLTMP/
	data 	c 	/SPLCMD/

	close(lstlun,iostat=idummy)

	if (ilfl==SPOOLFL) {   # if it is the spool file for the printer spooling,
                               # then print and zero the spool file
				# else just return.

		if (fsize(SPOOLFL) <= 0) return		#empty so exit
		call system ('specprnt ' // SPOOLFL // ' ; cp ' // NULL // ' ' // SPOOLFL)
		return
	}

	return
	end

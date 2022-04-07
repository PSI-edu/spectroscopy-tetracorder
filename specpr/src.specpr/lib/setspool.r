	subroutine setspo
	implicit integer*4 (i-n)

#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc         This subruotine sets up the spooler output file
#ccc         as lu 12 if the list file name = 'spooler'
#ccc         otherwise it does nothing
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc                    tname,setfil
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
########################################################################
#       this routine sets up the spooler output file as lu 12 if
#       the list file name = 'spooler' otherwise it does nothing
########################################################################
	include "../common/lblvol"
	include "../common/lundefs"
	include "../common/filenames"
	include "../common/spool"

	character*26 l
	integer*4 time, idummy

	data l/SPLDAT/


	close(lstlun,iostat=idummy)
	if (ilfl!=SPLFILE) {
		open(lstlun,file=ilfl,form='formatted')
		return
	}

	spfile = l
	call tname(spfile,23,time())
	open(lstlun,file=spfile,form='formatted')
	return
	end

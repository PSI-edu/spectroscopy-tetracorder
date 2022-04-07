	integer*4 function iopenp(lun,name,ext,disp)
	integer*4		lun
	character*2     disp
	character*(*)   name,ext

#       this routine opens the vector and text plotfiles
#
#       arguments:
#
#       lun:    input   logical unit of the file
#       name:   i/o     name of the plotfiles. 40 characters or less
#                       this name is modified by appending the
#                       contents of 'ext'
#       ext:    input   extension of filename. 4 characters
#       disp:   input   disposition. 'O' if opening an existing file.
#
#
	include "fdefs.h"
	include "../common/plot01"

#RED
	integer*4 lnb      # function lnb

	logical*4 exists
	integer*4 ier

################ find end of name (null character) #####################
	i = lnb(name)

################### append extension ##############################
	name(i+1:i+len(ext)) = ext

################ check if we can access the old file #################
	if (disp(1:1) == 'O') {
		inquire(file=name,exist=exists)
		if (!exists) {
			write(lunmsg,1000) name
			iopenp = 1
			return
		}
		open(lun,file=name,status='old')
	}
######################### create new file ############################
	else {
		if (lun==2) {
			open(lun,file=name,status='new',
				iostat=ier,form='formatted')
		} else if (lun==1) {
			open(lun,file=name,status='new',
				iostat=ier,form='unformatted',
				access='direct',recl=10)
		} else {
			write (lunmsg, '(" iopenp: lun not = 1 or 2")')
			iopenp = 1
			return
		}
		vecrec = 1
		if (ier != 0) {
			write(lunmsg,2000) name
			iopenp = 1
			return
		}
	}
	iopenp = 0
	return

1000	format(' cannot access ',a)
2000	format(' cannot create ',a)

	end

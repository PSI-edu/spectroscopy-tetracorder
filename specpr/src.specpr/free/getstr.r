	subroutine getstr(i,ifile,ier)
	implicit integer*4 (i-n)

#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc                   This subroutine reads a string from iopcon.
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc                    none
#ccc  argument list description:
#ccc     arguments: i,ifile,ier
#ccc  parameter description:
#ccc    i     input  starting point in iopcon
#ccc    ifile output the returned string
#ccc    ier   output error flag. =0 if no error
#ccc                             =1 if string found is longer than ifile
#ccc                             =2 if no string found
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc
#######################################################################



	include "../common/lbl4"

	character*(*)   ifile

	n = len(ifile)

	ier=0
	while (i<=80 && iopcon(i:i) == ' ')
		i=i+1

	if (i>=80 && iopcon(80:80) == ' ') {
		ier = 2
		ifile = ' '
		return
	}

	j = index(iopcon(i:80),' ')
	if (j==0) {
		ifile = iopcon(i:80)
		if (80-i>n-1) ier = 1
		i = 80
	} else {
		ifile = iopcon(i:i+j-1)
		if (j-i>n) ier = 1
		i = i+j
	}

	return
	end

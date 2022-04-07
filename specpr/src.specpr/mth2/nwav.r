	subroutine nwav(i)
	implicit integer*4(i-n)
#cc  version date: 06/01/83
#cc  author(s): roger clark & jeff hoover
#cc  language:  fortran
#cc
#cc  short description:
#cc      this subroutine determines the wavelength file id and number
#cc  algorithm description: none
#cc  system requirements:   none
#cc  subroutines called:
#cc                    wjfren,redwav, ihchar
#cc  argument list description:
#cc         arguments: i
#cc  parameter description:
#cc  common description:
#cc  message files referenced:
#cc  internal variables:
#cc  file description:
#cc  user command lines:
#cc  update information:
#cc  notes:
#cc
	include "../common/spmaxes"   # max parameters, must be first

	include "../common/blank"
	include "../common/lbl7"
	include "../common/lbl4"
	include "../common/lblg"
	include "../common/label1"
	include "../common/lblwav"
	include "../common/alphabet"
	include "../common/lundefs"

#RED
	integer*4 ihchar   # function ihchar

#
#     determine the wavelength file set number and space type.
#
	if ((iopcon(i:i)=='V')|(iopcon(i:i)=='W')|(iopcon(i:i)=='D')|
		(iopcon(i:i)=='U')|(iopcon(i:i)=='Y')) {

		itrol(1) = ihchar(iopcon(i:i))
		j = i
		i = i+1
		call wjfren(i,x,il)
		if (il!=0) i = i-1
		itrol(2) = x
		call redwav(itrol(1), itrol(2), ier)
		nchans = iwtchn
	} else {
		if (iopcon(i:i)=='C') {
			itrol(1)= ihchar(iopcon(i:i))
			j = i
			i = i+1
			call wjfren (i,x,il)
			if (il != 0) i = i-1
			if ((x > 0) & (x <= maxchn)) {
				itrol(2)=x
				nchans = x
			} else {
				write (ttyout,100) x
				itrol(2)=1
			}
		}
	}
100	format (1x, 'Specified number of channels too large:', f14.0, /)
	return
	end

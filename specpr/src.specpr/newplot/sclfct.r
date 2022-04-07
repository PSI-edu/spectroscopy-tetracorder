	logical function sclfct(ii)
	implicit integer*4 (i-n)

#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Fortran
#ccc
#ccc  short description:
#ccc                   This subroutine reads the plot scaling factor
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc                    crtin, wjfren,what
#ccc  argument list description:
#ccc       argument: vscale,hscale,ic
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc
################################################################
#       this subroutine reads the plot scaling factors
################################################################

	include "../common/spmaxes"   # max parameters, must be first

	include "../common/pltcnt"
	include "../common/lundefs"
	include "../common/alphabet"

	real*4	x

	repeat {
		write(ttyout,10)
		call crtin
		i=1
		vscale=1.0
		hscale=1.0
		call wjfren(i,x,il)

		if (i>=80) {
			sclfct=.true.
			return
		}
		if (il==ihx || il==ihe) {
			sclfct=.false.
			return
		}

		if (x<0.09 || x>1.0) {
			call what(i)
			next
		}
		hscale=x

		call wjfren(i,x,il)
		if (i>=80 || x<0.09 || x>1.0) {
			call what(i)
			next
		}
		vscale=x
		sclfct=.true.
		return
	}

10     format(/,' Type in the horizontal and vertical ',
					'axes SCALING FACTORS',/,
		' or press return for no scaling.',/,
		' the horizontal scaling must be ',
				'less than or equal to 1.0',/,
		' the vertical scaling must be ',
				'less than or equal to 1.0',/)
	end

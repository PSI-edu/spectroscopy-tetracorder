	logical function haxis (ii)
	implicit integer*4 (i-n)

#ccc  version date: %W% %G% %U%
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  RATFOR
#ccc
#ccc  short description: 
#ccc	This routine gets the horizontal axis plotting
#ccc	parameters from the user.
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc	crtin,wjfren,ihchar
#ccc  argument list description: none
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


	include "../common/pltcnt"
	include "../common/lbl4"
	include "../common/alphabet"
	include "../common/lundefs"

	real*4 	x


	xlabel = ' '
	repeat {
		wminp = 0.0
		wmaxp = 0.0
		iptype = 0

		write(ttyout,7)
		call crtin
		i = 1
		call wjfren(i,x,il)
		isave = i

		if (il ==  ihe | il == ihx) {
			haxis=.false.
			return
		} else if (il ==  ihy) {
			iptype = -1

		} else if (il ==  ihw) {
			iptype = 1

		} else if (il ==  ihn) {
			iptype = 2

		} else if (il ==  ihr) {
			iptype = 3
		}

		if (iptype==0) next

		call wjfren(i,x,il)
		if (il == ihn) nhscle = .true.
		else {
			i = isave
			nhscle = .false.
		}

		call wjfren(i,x,il)
		if (il!=0) next
		if (i>=80) break
		wminp = x

		call wjfren(i,x,il)
		if (il!=0) next
		if (x<=wminp) next
		wmaxp = x
		break
	}

	if (iptype==-1) {
		write(ttyout,10)
		call crtin
		xlabel = iopcon
	}
	haxis=.true.
	return

7     format(' ',
 'Type  w  to plot in WAVELENGTH mode',/,
'       n  to plot in INVERSE WAVELENGTH ',
				'(WAVENUMBER) mode',/,
'       r  to plot in REVERSE WAVENUMBER mode. (WAVENUMBER ',
				'increasing to the',/,60x,'left)',/,
'       y  for LINEAR mode with YOUR OWN horizontal ',
						'axis LABEL',//,
'       e or x to EXIT routine', //,
' Each letter (w, n, r or y) may be followed by an  n  ',
			'to remove the 2% scaling',/,
'              normally done.',/,
'      You can also enter the horizontal axis plot limits.',/,
'      For example:   w 0.2 3.0       or       wn 0.2 3.0')

10      format(' Type in the HORIZONTAL AXIS LABEL',/,
		1x,23('-'),'|',/)

	end

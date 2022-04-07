	subroutine get3ptbd (i,ibd,ierw)
	implicit none

# read wavelengths/channels for feature band depts

	include "../common/spmaxes"   # max parameters, must be first

        include "../common/lbl4"
        include "../common/alphabet"
        include "../common/sp3pfeat"

#       get 3-point band depth wavelengths/channels for band depth calculations
#
#             label      left1   left2  center1  center2   right1  right2
#       bd1= 1.04um C|W  0.958   0.986   1.02     1.05      1.080   1.110
#       bd1= 1.04um   W  0.958   0.986   1.02     1.05      1.080   1.110
#       bd2= 1.25um   W  1.12    1.16    1.25     1.29      1.34    1.38  box
#
#       bd1= 1.04um   W  0.958   0.986   1.02     1.05      1.080   1.110 box
#       bd2= 1.25um   W  1.12    1.16    1.25     1.29      1.34    1.38  box
#       bd3= 1.5um    W  1.35    1.41    1.50     1.55      1.76    1.81  box
#       bd4= 2.0um    W  1.79    1.845   2.01     2.04      2.22    2.259 box
#       bd5= 2.3um    W  2.22    2.259   2.34     2.37      2.43    2.475 box
#
#       added 11/23/2018 - R. Clark

#       ibd = band number,  current limit is 9 bands

	integer*4 i, i2, ibd, ierw, j, il, nb, fnb
	real*4    x

	character*12 iname

	ierw=0   # error value
	x = 0.0

	# nb = next blank

	#write (*,*) "DEBUG: get3ptbd pos1"

#       i = index in the iopcon command line array
#       ibd = which array to put specturm into (1 to 9)

	# imax3pt is set in spmaxes, but currently = 9

	if ( ibd > imax3pt ) {
		write (*,*) "ERROR, band depth is out of range:", ibd
		call what(i)
		ierw=1
		return
	}

	# sfmode    # mode: 0= not defined, 1= C= channels, 2= W= waves

	if ( ibd > 0 ) {

		sfmode(ibd)=0       # no 3-point band defined yet
		sonoff(ibd)=0       # off
		spcbox(ibd)=0       # no bax

		i = i + fnb(iopcon(i:maxcline)) -1

		for (nb=i; nb<=maxcline; nb=nb+1) {
			if (iopcon(nb:nb) == ' ') {
				nb = nb-1
				break
			}
		}
		iname='            '
		i2 = nb -i +1
		iname(1:i2) = iopcon(i:nb)  # feature name
		sfeatname(ibd) = iname

		i = nb+1

		call wjfren (i, x, il)

		if (il == ihc || il == ihcc) {
			sfmode(ibd) = 1   # channeks
		} else if (il == ihw || il == ihcw) {
			sfmode(ibd) = 2   # wavelengths
		} else {
			call what(i)
			write (*,*) "Error: input for 3-point feature confused.  Expecting C or W"
			sfmode(ibd)=0
			ierw = 1
			return
		}

		# now expect imax3pt numbers for left, center and right continuum pairs

		if (sfmode(ibd) == 1) {   # get channels

			# left continuum channels
			call wjfren (i, x, il)
			if (x > 0.0 ) {
				sleftchan(1,ibd) = x
			} else {
				call what(i)
				write (*,*) 'Error: 3pt channel out of range'
				ierw = 1
				return
			}
			call wjfren (i, x, il)
			if (x > 0.0 ) {
				sleftchan(2,ibd) = x
			} else {
				call what(i)
				write (*,*) 'Error: 3pt channel out of range'
				ierw = 1
				return
			}

			# center channels
			call wjfren (i, x, il)
			if (x > 0.0 ) {
				sctrchan(1,ibd) = x
			} else {
				call what(i)
				write (*,*) 'Error: 3pt channel out of range'
				ierw = 1
				return
			}
			call wjfren (i, x, il)
			if (x > 0.0 ) {
				sctrchan(2,ibd) = x
			} else {
				call what(i)
				write (*,*) 'Error: 3pt channel out of range'
				ierw = 1
				return
			}

			# right continuum channels
			call wjfren (i, x, il)
			if (x > 0.0 ) {
				srightchan(1,ibd) = x
			} else {
				write (*,*) 'Error: 3pt channel out of range'
				call what(i)
				ierw = 1
				return
			}
			call wjfren (i, x, il)
			if (x > 0.0 ) {
				srightchan(2,ibd) = x
			} else {
				call what(i)
				write (*,*) 'Error: 3pt channel out of range'
				ierw = 1
				return
			}
		}

		if (sfmode(ibd) == 2) {   # get wavelengths

			# left continuum wavelengths
			call wjfren (i, x, il)
			if (x > 0.0 ) {
				sleftwave(1,ibd) = x
			} else {
				call what(i)
				write (*,*) 'Error: 3pt channel out of range'
				ierw = 1
				return
			}
			call wjfren (i, x, il)
			if (x > 0.0 ) {
				sleftwave(2,ibd) = x
			} else {
				call what(i)
				write (*,*) 'Error: 3pt channel out of range'
				ierw = 1
				return
			}

			# center wavelengths
			call wjfren (i, x, il)
			if (x > 0.0 ) {
				sctrwave(1,ibd) = x
			} else {
				call what(i)
				write (*,*) 'Error: 3pt channel out of range'
				ierw = 1
				return
			}
			call wjfren (i, x, il)
			if (x > 0.0 ) {
				sctrwave(2,ibd) = x
			} else {
				call what(i)
				write (*,*) 'Error: 3pt channel out of range'
				ierw = 1
				return
			}

			# right continuum wave
			call wjfren (i, x, il)
			if (x > 0.0 ) {
				srightwave(1,ibd) = x
			} else {
				write (*,*) 'Error: 3pt channel out of range'
				call what(i)
				ierw = 1
				return
			}
			call wjfren (i, x, il)
			if (x > 0.0 ) {
				srightwave(2,ibd) = x
			} else {
				call what(i)
				write (*,*) 'Error: 3pt channel out of range'
				ierw = 1
				return
			}

		}
		sonoff(ibd)=1  # band depth calculation on

		call wjfren (i, x, il)
		if (il == ihb && iopcon(i:i+1) == "ox") {   # box keyword found
			spcbox(ibd)=1
		}

	}

	#call crtin   # DEBUG
	return
	end

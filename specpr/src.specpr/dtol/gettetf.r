	subroutine gettetf (i,ibd,ierw)
	implicit none

# read wavelengths/channels for tetracorder feature definition

	include "../common/spmaxes"   # max parameters, must be first

        include "../common/lbl4"
	include "../common/lundefs"
        include "../common/alphabet"
        include "../common/tetfeat"

# example entries:
#
# tf1= mineral1 f1a DLw 2.150   2.210   2.390   2.430  ct 0.04 r*bd> 0.002 0.004
# tf1= mineral1 f1a MLw 2.150   2.210   2.390   2.430  ct 0.04 r*bd> 0.002 0.004
# tf2= mineral1 f2a DLw 2.199   2.230   2.275   2.290  ct 0.04 r*bd> 0.002 0.004
# tf3= thuring  f3a WLw 0.512   0.542   1.696   1.726

# tf1= Snow.H2O f1a DLw 0.958   0.986   1.080   1.110  ct 0.08
# tf2= Snow.H2O f2a DLw 1.150   1.178   1.315   1.345  ct 0.08 lct/rct> 0.9 1.1

#       added 11/25/2018 - R. Clark

#       ibd = band number,  current limit is 6 bands

	integer*4 i, i2, ibd, ierw, j, il, nb, fnb
	real*4    x

	character*12 iname

	ierw=0   # error value
	x = 0.0

	# nb = next blank

	#write (*,*) "DEBUG: tetfeat pos1"

#       i = index in the iopcon command line array
#       ibd = which array to put specturm into (1 to 6)

	# imaxtet is set in spmaxes, but currently = 9

	if ( ibd > imaxtet ) {
		write (*,*) "ERROR, tetracorder feature definition is out of range:", ibd
		call what(i)
		ierw=1
		return
	}

	# tfmode    # mode: 0= not defined, 1= C= channels, 2= W= waves

222	format (1x, "example:",/,
		1x, "tf1= Snow.H2O f1a DLw 0.958   0.986   1.080   1.110  ct 0.08")

	if ( ibd > 0 ) {

		tfmode(ibd)=0       # no tetracorder feature defined yet
		tetonoff(ibd)=0       # off

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
		tfeatname(ibd) = iname

		i = nb+1

		call wjfren (i, x, il)  # now expect f2a, f2a, etc

		if (il == ihf) {
			tetfna(ibd) = '    '
			iname='            '
			iname(1:3)  = iopcon(i-1:i+1)
	 		tetfna(ibd) = iname(1:4) 
			i=i+2
		} else {
			call what(i)
			write (*,*) "Error: input for tetracorder feature confused.  Expecting f"
			write (ttyout, 222)   # example
			tfmode(ibd)=0
			ierw = 1
			return
		}

		call wjfren (i, x, il)  # now expect DLw, OLw. WLw

		if (il == ihcd || il == ihcm ||  il == ihco || il == ihcw) {
			tetfnc(ibd) = '    '
			iname='            '
			iname(1:3)  = iopcon(i-1:i+1)
	 		tetfnc(ibd) = iname(1:4) 
		} else {
			call what(i)
			write (*,*) "Error: input for tetracorder feature confused.  Expecting D, W or O"
			write (ttyout, 222)   # example
			tfmode(ibd)=0
			ierw = 1
			return
		}

		call wjfren (i, x, il)  # now expect L or C  in DLw, WLw, or OLw

		tfcont(ibd) = 0   # no continuum type yet
		if (il == ihcl) {
			tfcont(ibd) = 1   # linear continuum
		} else if (il == ihcc) {
			tfcont(ibd) = 1   # curved continuum
		} else {
			call what(i)
			write (*,*) "Error: input for tetracorder feature continuum confused.  Expecting L or C"
			write (ttyout, 222)   # example
			tfcont(ibd)=0
			ierw = 1
			return
		}

		call wjfren (i, x, il)  # now expect w or c  in DLw or DLc

		tfmode(ibd) = 0   # no wave/chan mode type yet
		if (il == ihcc || il == ihc) {
			tfmode(ibd) = 1   # channels
		} else if (il == ihcw || il == ihw) {
			tfmode(ibd) = 2   # wavelength
		} else {
			call what(i)
			write (*,*) "Error: input for tetracorder feature confused.  Expecting w or c"
			write (ttyout, 222)   # example
			tfmode(ibd)=0
			ierw = 1
			return
		}

		
		# now expect 4 numbers for left, center and right continuum pairs

		if (tfmode(ibd) == 1) {   # get channels

			# left continuum channels
			call wjfren (i, x, il)
			if (x > 0.0 ) {
				tleftchan(1,ibd) = x
			} else {
				call what(i)
				write (*,*) 'Error: left channel out of range'
				write (ttyout, 222)   # example
				ierw = 1
				return
			}
			call wjfren (i, x, il)
			if (x > 0.0 ) {
				tleftchan(2,ibd) = x
			} else {
				call what(i)
				write (*,*) 'Error: left channel out of range'
				write (ttyout, 222)   # example
				ierw = 1
				return
			}

			# right continuum channels
			call wjfren (i, x, il)
			if (x > 0.0 ) {
				trightchan(1,ibd) = x
			} else {
				write (*,*) 'Error: right channel out of range'
				write (ttyout, 222)   # example
				call what(i)
				ierw = 1
				return
			}
			call wjfren (i, x, il)
			if (x > 0.0 ) {
				trightchan(2,ibd) = x
			} else {
				call what(i)
				write (*,*) 'Error: right channel out of range'
				write (ttyout, 222)   # example
				ierw = 1
				return
			}
		}

		if (tfmode(ibd) == 2) {   # get wavelengths

			# left continuum wavelengths
			call wjfren (i, x, il)
			if (x > 0.0 ) {
				tleftwave(1,ibd) = x
			} else {
				call what(i)
				write (*,*) 'Error: left continuum wavelength out of range'
				write (ttyout, 222)   # example
				ierw = 1
				return
			}
			call wjfren (i, x, il)
			if (x > 0.0 ) {
				tleftwave(2,ibd) = x
			} else {
				call what(i)
				write (*,*) 'Error: left continuum wavelength out of range'
				write (ttyout, 222)   # example
				ierw = 1
				return
			}

			# right continuum wave
			call wjfren (i, x, il)
			if (x > 0.0 ) {
				trightwave(1,ibd) = x
			} else {
				write (*,*) 'Error: right continuum wavelength out of range'
				write (ttyout, 222)   # example
				call what(i)
				ierw = 1
				return
			}
			call wjfren (i, x, il)
			if (x > 0.0 ) {
				trightwave(2,ibd) = x
			} else {
				call what(i)
				write (*,*) 'Error: right continuum wavelength out of range'
				write (ttyout, 222)   # example
				ierw = 1
				return
			}

		}
		tetonoff(ibd)=1  # band depth calculation on

		#call wjfren (i, x, il)
		#if (il == ihb && iopcon(i:i+1) == "ox") {   # box keyword found
		#	spcbox(ibd)=1
		#}

		tetfline(ibd) = iopcon(1:80)   # the full user input line

	}

	#call crtin   # DEBUG
	return
	end

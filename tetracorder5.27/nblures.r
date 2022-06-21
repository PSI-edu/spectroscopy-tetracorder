	subroutine nvres (wav,rflibc,rfobs,vegspec,
			cl1,cl2,cr1,cr2,ictrol,minch,
			maxch,ifeattype,iflag, ixpxl,iypxl,
                        rfobsc,xk,bd,rfit,slope,yintcp,rftemp,
			conref,a,b,
			xndvivegspec,xndviobs,xfactor,bdnorm)

######	implicit integer*4 (i-n)
	implicit none

#ccc  name:  nvres
#ccc  version date:  December 19, 1994
#ccc  author(s):  Roger N. Clark
#ccc  language:  Ratfor
#ccc
#ccc  short description: 
#ccc		This program does a least squares fit of a library
#ccc            reference spectrum to another spectrum over a given wavelength
#ccc            range.
#ccc
#ccc            WARNING: Input setup must be correct (see input
#ccc              parameters below).  This is to maximize speed 
#ccc              in imaging spectroscopy applications.
#ccc
#ccc            WARNING: subroutine bdmset MUST be called first!
#ccc
#ccc  algorithm description:
#ccc                    given continuum removed library spectrum,
#ccc                    add a constant to the spectrum and renormalize
#ccc                    to modify band depth such that there is a least
#ccc                    squares fit to the observed spectrum.
#ccc  system requirements:
#ccc  subroutines called:
#ccc
#ccc
#ccc  parameter description:
#ccc     INPUT:
#ccc        wav    = wavelengths  R*4  (cr2 elements)
#ccc        rflibc = reference library spectrum, continuum
#ccc                                 removed  R*4 (cr2 elements)
#ccc        rfobs  = observed spectrum  R*4 (cr2 elements)
#ccc        vegspec= vegetation red-edge reference spectrum
#ccc        cl1    = continuum point begin on left side of band  I*4
#ccc        cl2    = continuum point end on left side of band  I*4
#ccc                   note: cl2 >= cl1  (not checked)
#ccc        cr1    = continuum point begin on right side of band  I*4
#ccc                   note: cr1 > cl2 + 1 (not checked)
#ccc        cr2    = continuum point end on right side of band  I*4
#ccc                   note: cr2 >= cr1 (not checked)
#ccc                   note: cr2 also determines the max array sizes
#ccc        ictrol = error message control flag:
#ccc                   = 0 don't print error messages, just set
#ccc                       output to deleted values.
#ccc                   = 1 print error messages and set output to
#ccc                       deleted values.
#ccc        minch  = minimum channel in the library spectrum.
#ccc        maxch  = maximum channel in the reference library spectrum.
#ccc                 This is defined to be the band maximum.
#ccc        ifeattype = -1 is an emission feature
#ccc                  =  1 is an absorption band
#ccc        iflag  = flag to indicate if processing image-type data:
#ccc                   = 0 not image data (single spectrum analysis)
#ccc                   = 1 imaging spectrometer data.
#ccc        ixpxl  = x-pixel coordinate of image data (iflag = 1)
#ccc        iypxl  = y-pixel coordinate of image data (iflag = 1)
#ccc
#ccc     OUTPUT:
#ccc        rfobsc = continuum removed observed spectrum  R*4 (cr2 elements)
#ccc                   NOTE: continuum is removed ONLY between cl2 and cr1
#ccc                         To get complete continuum, you must remove
#ccc                         it yourself using the slope and yintcp
#ccc                         variables below.
#ccc        xk     = k factor needed to make reference match library  R*4
#ccc        bd     = band depth  R*4
#ccc        rfit   = fit parameter normalized to 1 channel R*4
#ccc        slope  = slope to continuum of observed spectrum
#ccc        yintcp = intercept to continuum of observed spectrum
#ccc        rftemp = temporary working array R*4 (cr2 elements)
#ccc                 at a normal return, rftemp is the fitted,
#ccc                 continuum removed spectrum.
#ccc        conref = continuum reflectance value of observed spectrum
#ccc        vegspec= normalized vegetation red-edge reference
#ccc                 NOTE: the input vegspec array IS MODIFIED
#ccc        rfobs  = original rfobs divided by normalized vegspec
#ccc                 NOTE: the input rfobs array IS MODIFIED
#ccc        a      = factor added to vegspec to match rfobs
#ccc        b      = vegspec = (vegspec + a) / b
#ccc        xndvivegspec = NDVI for vegspec
#ccc        xndviobs = NDVI for rfobs
#ccc        xfactor= factor the depth was increased by to normalize
#ccc                 the feature strength to that in the reference spectrum
#ccc                 This allows calibration to nanometers of shift.
#ccc        bdnorm = normalized band depth (= bd * xfactor)
#ccc
#ccc  common description: none
#ccc  message files referenced: none
#ccc  internal variables:
#ccc  file description: none
#ccc  user command lines: none
#ccc  update information:
#ccc  NOTES:
#ccc

        integer*4  cl1,cl2,cr1,cr2,ictrol,minch,maxch,ifeattype
	real*4 wav(cr2),rflibc(cr2),rfobs(cr2)
        real*4 rfobsc(cr2),xk,bd,rfit, rfit2
	real*4 rftemp(cr2), slope, yintcp, xn
	real*4 vegspec(cr2),conref,conrefl,conrefr
	integer*4 iflag,ixpxl,iypxl
	real*4 xndvivegspec,xndviobs,xfactor,bdnorm

	real*4 a, b
	real*4 avlcv, avrcv, avlcu, avrcu
	real*4 atop, abot
	integer*4 n, i, il, ir

	real*4 delpt

	integer*4 ttyout

	ttyout = 6
#
# deleted point value:
#
	delpt = -1.23e34

#
# first we must normalize the red-edge in the vegspec spectrum:
#
#       avlcv = average left  side of continuum in reference vegspec
#       avrcv = average right side of continuum in reference vegspec
#       avlcu = average left  side of continuum in unknown rfobs
#       avrcu = average right side of continuum in unknown rfobs

# first left continuum average:
	avlcv = 0.0
	avlcu = 0.0
	n = 0
	do i = cl1, cl2 {
		if (wav(i) == delpt | rflibc(i) == delpt |
			vegspec(i) == delpt | rfobs(i) == delpt) {

			rfobs(i) = delpt  # be sure rfobs marked deleted
			next
		}
		avlcu = avlcu + rfobs(i)
		avlcv = avlcv + vegspec(i)
		n = n +1
	}
	if (n < 1) {
		if (ictrol == 1 & iflag == 0) write (ttyout,120)
120		format (' ERROR: all points in observed spectrum,',
                        ' left continuum deleted.')
		if (ictrol == 1 & iflag == 1) write (ttyout,121) ixpxl, iypxl
121		format (' ERROR: all points in observed spectrum,',
                        ' left continuum deleted: pixel',i4,',',i4)
		xk = delpt
		bd = delpt
		rfit = delpt
		return
	}
	xn = float(n)
	avlcu = avlcu / xn
	avlcv = avlcv / xn
	if (avlcu < 0.1e-10 | avlcv < 0.1e-10) {   # if the continuum is 
						# less than zero, delete output
		if (ictrol == 1 & iflag == 0) write (ttyout,124)
124		format (' ERROR: left continuum <0.0.')
		if (ictrol == 1 & iflag == 1) write (ttyout,125) ixpxl, iypxl
125		format (' ERROR: left continuum <0.0r: pixel',i4,',',i4)
		xk = delpt
		bd = delpt
		rfit = delpt
		return
	}

# now right continuum average:
	avrcu = 0.0
	avrcv = 0.0
	n = 0
	do i = cr1, cr2 {
		if (wav(i) == delpt | rflibc(i) == delpt |
			vegspec(i) == delpt | rfobs(i) == delpt) {

			rfobs(i) = delpt  # be sure rfobs marked deleted
			next
		}
		avrcu = avrcu + rfobs(i)
		avrcv = avrcv + vegspec(i)
		n = n +1
	}
	if (n < 1) {
		if (ictrol == 1 & iflag == 0) write (ttyout,122)
122		format (' ERROR: all points in observed spectrum,',
                        ' right continuum deleted.')
		if (ictrol == 1 & iflag == 1) write (ttyout,123) ixpxl, iypxl
123		format (' ERROR: all points in observed spectrum,',
                        ' right continuum deleted: pixel',i4,',',i4)
		xk = delpt
		bd = delpt
		rfit = delpt
		return
	}
	xn = float(n)
	avrcu = avrcu / xn
	avrcv = avrcv / xn
	if (avrcu < 0.1e-10 | avrcv < 0.1e-10) {   # if the continuum is
						# less than zero, delete output
		if (ictrol == 1 & iflag == 0) write (ttyout,126)
126		format (' ERROR: right continuum <0.0.')
		if (ictrol == 1 & iflag == 1) write (ttyout,127) ixpxl, iypxl
127		format (' ERROR: right continuum <0.0r: pixel',i4,',',i4)
		xk = delpt
		bd = delpt
		rfit = delpt
		return
	}

# now we have two pairs of x,y points to derive the normalization of vegspec
#       thus: new vegspec = (vegspec + a) / b
#
#       derive a and b:

	atop = avlcu * avrcv - avrcu * avlcv
	abot = avrcu - avlcu

	if (abs(abot) < 0.1e-10 ) {
		if (ictrol == 1 & iflag == 0) write (ttyout,136)
136		format (' ERROR: observed spectrum is a straight line',/,
			'        can not determine an edge')
		if (ictrol == 1 & iflag == 1) write (ttyout,137) ixpxl, iypxl
137		format (' ERROR: observed spectrum is flat:pixel',i4,',',i4)
		xk = delpt
		bd = delpt
		rfit = delpt
		return
	}

	a = atop /abot

	b = (avlcv + a) / avlcu

	if (abs(b) < 0.1e-10) {
		if (ictrol == 1 & iflag == 0) write (ttyout,146)
146		format (' ERROR: infinite normalizarion factor ',
				'for edge spectrum',/,
			'        can not determine an edge')
		if (ictrol == 1 & iflag == 1) write (ttyout,147) ixpxl, iypxl
147		format (' ERROR: infinite edge normalization: pixel',i4,',',i4)
		xk = delpt
		bd = delpt
		rfit = delpt
		return
	}

# now normalize vegspec so edge at continuum points match the unknown

	#if (iflag == 1) {        # imaging mode, do minimum
	#	il = cl2 + 1
	#	ir = cr1 - 1
	#} else {                 # single spectrum mode, do more.
	#	il = cl1
	#	ir = cr2
	#}
	il = cl1
	ir = cr2

	do i = il, ir {

		if (vegspec(i) == delpt | 
				abs(vegspec(i)) < 0.1e-10) {

			rfobs(i) = delpt
			next
		}
		if (rfobs(i) == delpt) {
			vegspec(i) = delpt
			next
		}

		vegspec(i) = (vegspec(i) + a) / b
		rfobs(i) = rfobs(i) / vegspec(i)
	}


	call bandmp (wav,rflibc,rfobs, cl1,cl2,cr1,cr2,ictrol,minch,
			maxch,ifeattype,iflag, ixpxl,iypxl,
                        rfobsc,xk,bd,rfit,slope,yintcp,rftemp,conref,
			conrefl,conrefr)

######	compute xndvivegspec xndviobs xfactor bdnorm

	if (bd != delpt) {  # normalize band depth to what it would be
				# if the veg red-edge in the observed
				# was as strong as that in the
				# reference edge spectrum.

		xndvivegspec = (avrcv - avlcv) / (avrcv + avlcv)
		xndviobs     = (avrcu - avlcu) / (avrcu + avlcu)

		if (abs(xndviobs) > 0.1e-10) {
			xfactor = xndvivegspec / xndviobs
			bdnorm = xfactor * bd
		} else {
			xfactor = delpt
			bdnorm  = delpt
		}
	
	} else {
		xfactor = delpt
		bdnorm  = delpt

	}

	return
	end

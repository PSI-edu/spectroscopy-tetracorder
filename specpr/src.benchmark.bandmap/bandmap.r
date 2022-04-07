	subroutine bandmp (wav,rflibc,rfobs,cl1,cl2,cr1,cr2,ictrol,minch,
			maxch,ifeattype,iflag, ixpxl,iypxl,
                        rfobsc,xk,bd,rfit,slope,yintcp,rftemp,conref)
	implicit integer*4 (i-n)
#ccc  name:  bandmp
#ccc  version date:  January 29, 1990
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
#ccc
#ccc  common description: none
#ccc  message files referenced: none
#ccc  internal variables:
#ccc        suml   = sum (rflibc)           Real*8
#ccc        sumll  = sum (rflibc * rflibc)  Real*8
#ccc        sumol  = sum (rfobsc * rflibc)  Real*8
#ccc        sumo   = sum (rfobsc)           Real*8
#ccc        sumoo  = sum (rfobsc * rfobsc)  Real*8
#ccc  file description: none
#ccc  user command lines: none
#ccc  update information:
#ccc  NOTES:
#ccc

        integer*4  cl1,cl2,cr1,cr2,ictrol,minch,maxch,ifeattype
	real*4 wav(cr2),rflibc(cr2),rfobs(cr2)
        real*4 rfobsc(cr2),xk,bd,rfit, rfit2
	real*4 rftemp(cr2), slope, yintcp, xn
	real*4 top, bottom, botm2, bprime, drfit2, drfit

	real*8 suml,sumll,sumol,sumo,sumoo,dxk,dbb,dxn
	real*8 dro, drl

	integer*4 ttyout

	ttyout = 6

#
# deleted point value:
#
	delpt = -1.23e34

#
# first we must remove the continuum to the observed spectrum:
#      compute rfobsc
#
#       avlr = average left  side of continuum
#       avrc = average right side of continuum
#       avwlc = average wavelength left side continuum
#       avwrc = average wavelength right side continuum

# first left continuum average:
	avlc = 0.0
	avwlc = 0.0
	n = 0
	do i = cl1, cl2 {
		if (wav(i) == delpt || rflibc(i) == delpt ||
						rfobs(i) == delpt) next
		avlc = avlc + rfobs(i)
		avwlc = avwlc + wav(i)
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
	avlc = avlc / xn
	avwlc = avwlc / xn

# now right continuum average:
	avrc = 0.0
	avwrc = 0.0
	n = 0
	do i = cr1, cr2 {
		if (wav(i) == delpt || rflibc(i) == delpt ||
						rfobs(i) == delpt) next
		avrc = avrc + rfobs(i)
		avwrc = avwrc + wav(i)
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
	avrc = avrc / xn
	avwrc = avwrc / xn

# now we have two pairs os x,y points to derive the continuum line
#       thus: wav = a * rfobs  + b

	bottom = avwrc - avwlc
	if (abs(bottom) < 0.1e-20) {
		if (ictrol == 1) write (ttyout, 130)
130		format (' ERROR: wavelength range of continuum is',
                        ' too small!')
		xk = delpt
		bd = delpt
		rfit = delpt
		return
	}

	a = (avrc - avlc)/bottom
	b = avrc - a * avwrc

	slope = a
	yintcp = b

#
# now we can continuum correct the observed spectrum.
#     compute rfobsc
#
# don't do because speed is reduced
#############
#	if (cl1 > 1) {
#		do i = 1, cl1-1 {
#			rfobsc(i) = delpt
#		}
#	}
#############

#  compute continuum only between cl2 and cr1 (but not including them)
#          in imaging mode, but in single spectrum mode, include all
#          of continuum.

	if (iflag == 1) {        # imaging mode, do minimum
		il = cl2 + 1
		ir = cr1 - 1
	} else {                 # single spectrum mode, do more.
		il = cl1
		ir = cr2
	}

	do i = il, ir {
		if (wav(i) == delpt || rflibc(i) == delpt ||
						rfobs(i) == delpt) {
			rfobsc(i) = delpt
		} else {
			contin = a * wav(i) + b
			if (abs(contin) > 0.1e-20) {
				rfobsc(i) = rfobs(i) / contin
			} else {
				rfobsc(i) = delpt
			}
		}
	}
	if (ifeattype == 1)  {
		if (wav(minch) == delpt) {
			conref = delpt
		} else {
			conref = a * wav(minch) + b
		}
	} else {
		if (wav(maxch) == delpt) {
			conref = delpt
		} else {
			conref = a * wav(maxch) + b
		}
	}

#
#  now compute sums between cl2 and cr1 (but not including them)
#
	n = 0
	suml = 0.0
	sumll = 0.0
	sumol = 0.0
	sumo = 0.0
	sumoo = 0.0
	do i = il, ir {
		if (rfobsc(i) == delpt) next
		n = n +1
		drl = dble(rflibc(i))
		dro = dble(rfobsc(i))
		suml  = suml  + drl
		sumll = sumll + drl*drl
		sumol = sumol + dro*drl
		sumo  = sumo  + dro
#                      the following is needed only for correlation coefficient
		sumoo = sumoo + dro*dro
	}
	if (n == 0) {         # no sums, so delete output
		if (ictrol == 1) write (ttyout, 140)
140		format (' ERROR: routine bandmp: sums in least ',
			'squares have no data summed!  Output deleted.')
		xk = delpt
		bd = delpt
		rfit = delpt
		return
	}
#
#  now compute a and b in the least squares solution
#
	xn = float(n)
	dxn = dble(xn)
	top = sngl(sumol - (sumo * suml / dxn))
	bottom = sngl(sumll - (suml * suml / dxn))
	if (abs(bottom) < 0.1e-30) {
		b = 0.0
	} else {
		b = top / bottom
	}
#
# save the b value
#
	bb = b
#
# now compute the xk value
#
	if (abs(bb) < 0.1e-30) {   # xk too large (b too small)
		xk = delpt
		bd = delpt
		rfit = delpt
		return
	} else {
		xk = (1.0 - b) / b
	}
#
# now compute the reference spectrum that best matches the observed spectrum
#
	xk1 = xk + 1.0
	do i = il, ir {
		if (rflibc(i) == delpt) {
			rftemp(i) = delpt
		} else {
			rftemp(i) = (rflibc(i) + xk) / xk1
		}
	}
#
# compute band depth
#
	if (ifeattype == 1)  {
		bd = 1.0 - rftemp(minch)
	} else {
		bd = 1.0 - rftemp(maxch)
	}
#
# compute goodness of fit
#
#  now compute aprime and bprime in the least squares solution
#
#	top is still the same
	botm2 = sngl(sumoo - (sumo * sumo / dxn))
	if (abs(botm2) < 0.1e-30) {
		bprime = 0.0
	} else {
		bprime = top / botm2
	}
	rfit2 = abs(bb * bprime)
	rfit = sqrt(rfit2)       # goodness of fit
#
# DEBUG:
#
#	need to add: ictrol,ixpxl,iypxl,
#
#	write (ttyout,*) 'DEBUG: bandmp: cl1=',cl1,' cl2=',cl2,
#					' cr1=',cr1,' cr2=',cr2
#	do i = cl1, cr2 {
#		write (ttyout,*) 'DEBUG: bandmp: wav(',i,')=',wav(i),
#				'  rflibc(',i,')=',rflibc(i),
#					'  rfobs(',i,')=',rfobs(i)
#	}
# 	write (ttyout,*) 'DEBUG: bandmap: xn=',xn
# 	write (ttyout,*) 'DEBUG: bandmap: suml=',suml,
#				' sumll=',sumll
# 	write (ttyout,*) 'DEBUG: bandmap: sumol=',sumol,
#				' sumo=',sumo,
#				' sumoo=',sumoo
#	write (ttyout,*) 'DEBUG: bandmap: xk=',xk,
#				' bd=',bd,' rfit2=',rfit2,' rfit=',rfit
#	write (ttyout,*) 'DEBUG: bandmap: slope=',slope,
#				' yintcp=',yintcp
#	write (ttyout,*) 'DEBUG: bandmap: top=',top,
#				' bottom=',bottom,
#				' botm2=',botm2
#	write (ttyout,*) 'DEBUG: bandmap: bb=',bb,
#				' bprime=',bprime
#	do i = cl1, cr2 {
#		write (ttyout,*) 'DEBUG: bandmap: rfobsc(',i,')=',
#				rfobsc(i), ' rftemp(',i,')=',rftemp(i)
#	}
#	write (ttyout,*) 'DEBUG: bandmp: minch=',minch,' iflag=',iflag
# END.DEBUG
#
	return
	end

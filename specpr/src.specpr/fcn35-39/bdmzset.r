	subroutine bdmzset (wav,rflib,cl1,cl2,cr1,cr2,
                           rflibc,minch,maxch,ifeattype,ier)
	implicit integer*4 (i-n)
#ccc  name:  bdmzset
#ccc  version date:  January 29, 1990
#ccc  author(s):  Roger N. Clark
#ccc  language:  Ratfor
#ccc
#ccc  short description: 
#ccc		This program computes non-changing library reference data
#ccc            for the bandmp subroutine.
#ccc
#ccc  algorithm description:
#ccc                    given continuum removed -1.0 library spectrum,
#ccc                    scale the feaqtures
#ccc                    to modify band depth such that there is a least
#ccc                    squares fit to the observed spectrum.
#ccc
#ccc                    THIS subroutine computes the non-changing
#ccc                         components of the least squares solution.
#ccc  system requirements: none
#ccc  subroutines called: none
#ccc
#ccc
#ccc  parameter description:
#ccc     INPUT:
#ccc        rflib  = reference library spectrum  R*4 (cr2 elements)
#ccc        cl1    = continuum point begin on left side of band  I*4
#ccc        cl2    = continuum point end on left side of band  I*4
#ccc                   note: cl2 >= cl1  (checked)
#ccc        cr1    = continuum point begin on right side of band  I*4
#ccc                   note: cr1 > cl2 + 1 (checked)
#ccc        cr2    = continuum point end on right side of band  I*4
#ccc                   note: cr2 >= cr1 (checked)
#ccc                   note: cr2 also determines the max array sizes
#ccc     OUTPUT:
#ccc        rflibc = reference library spectrum, continuum
#ccc                                 removed  R*4 (cr2 elements)
#ccc        minch  = minimum channel in the reference library spectrum.
#ccc                 This is defined to be the band minimum.
#ccc        maxch  = maximum channel in the reference library spectrum.
#ccc                 This is defined to be the band maximum.
#ccc        ifeattype = -1 is an emission feature
#ccc                  =  1 is an absorption band
#ccc        ier    = error detected in input:
#ccc                    = 0 no error
#ccc                    = 1 error
#ccc
#ccc  common description: none
#ccc  message files referenced: none
#ccc  internal variables:
#ccc  file description: none
#ccc  user command lines: none
#ccc  update information:
#ccc  NOTES:
#ccc

	integer*4 ttyout
	integer*4 cl1,cl2,cr1,cr2,ier,minch,maxch,ifeattype
	real*4 wav(cr2), rflib(cr2)
	real*4 rflibc(cr2)

	ttyout = 6
	minch=0
	maxch=0

#
# check input parameters are correct
#
	ier = 0
	if (cl1 > cl2) {
		write (ttyout, 100)
100		format (' ERROR: left continuum segment point 1 is',
			' greater than point 2')
		ier = 1
	}
	if (cr1 > cr2) {
		write (ttyout, 101)
101		format (' ERROR: right continuum segment point 1 is',
			' greater than point 2')
		ier = 1
	}
	if (cl2 + 1 > cr1 - 1) {
		write (ttyout, 102)
102		format (' ERROR: left continuum segment point 2 + 1 ',
			'channel is greater than ',/,
                        '        right continuum segment point 1 - 1 channel',/,
			'        This means there are no channels for',
			' the actual absorption band' )
		ier = 1
	}
	if (ier == 1) return
#
# deleted point value:
#
	delpt = -1.23e34

#
# compute continuum correction to ref spectrum
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
		if (wav(i) == delpt || rflib(i) == delpt) next
		avlc = avlc + rflib(i)
		avwlc = avwlc + wav(i)
		n = n +1
	}
	if (n < 1) {
		write (ttyout,120)
120		format (' ERROR: all points in reference spectrum,',
                        ' left continuum deleted.')
		ier = 1
		return
	}
	avlc = avlc / float(n)
	avwlc = avwlc / float(n)

# now right continuum average:
	avrc = 0.0
	avwrc = 0.0
	n = 0
	do i = cr1, cr2 {
		if (wav(i) == delpt || rflib(i) == delpt) next
		avrc = avrc + rflib(i)
		avwrc = avwrc + wav(i)
		n = n +1
	}
	if (n < 1) {
		write (ttyout,121)
121		format (' ERROR: all points in reference spectrum,',
                        ' right continuum deleted.')
		ier = 1
		return
	}
	avrc = avrc / float(n)
	avwrc = avwrc / float(n)

# now we have two pairs of x,y points to derive the continuum line
#       thus: wav = a * rflib  + b

	bottom = avwrc - avwlc
	if (abs(bottom) < 0.1e-20) {
		write (ttyout, 130)
130		format (' ERROR: wavelength range of continuum is too small!')
		ier = 1
		return
	}

	a = (avrc - avlc)/bottom
	b = avrc - a * avwrc

#
# now we can continuum correct the reference spectrum.
#     compute rflibc
#
	if (cl1 > 1) {
		do i = 1, cl1-1 {
			rflibc(i) = delpt
		}
	}
	do i = cl1, cr2 {
		if (wav(i) == delpt || rflib(i) == delpt) {
			rflibc(i) = delpt
		} else {
			contin = a * wav(i) + b
			if (abs(contin) > 0.1e-20) {
				rflibc(i) = rflib(i) / contin
			} else {
				rflibc(i) = delpt
			}
		}
	}
#
# now find the band minimum and maximum in case of emission feature
#
	il = cl2 + 1
	ir = cr1 - 1
195	if (rflibc(il) == -1.23e+34) {  # find first non-deleted point
		il = il + 1
		if (il > ir) {
			write (ttyout, 196)
196			format (' ERROR in band map setup:',/,
				'       can not find a min or max: ',
				'all channels deleted')
			ier = 1
			return
		}
		go to 195
	}
	rmin = rflibc(il)
	rmax = rmin
	minch = il
	maxch = il
	do i = il, ir {
		if (rflibc(i) == delpt) next
		if (rflibc(i) < rmin) {
			minch = i
			rmin = rflibc(i)
		}
		if (rflibc(i) > rmax) {
			maxch = i
			rmax = rflibc(i)
		}
	}
	depth = 1.0 - rmin  # band depth for an emission feature
	emiss = rmax - 1.0  # emission feature
	if (rmin < 0.0) {
		write (ttyout, 200)
200		format (' ERROR: band depth in the library spectrum ',/,
			'        is LESS THAN ZERO: that is invalid')
		ier = 1
		return
	}
#
# reduce continuum level to zero
#
	do i = cl1, cr2 {
		rflibc(i) = rflibc(i) - 1.0
	}
#
# determine if the feature is emission or absorption
#
	ifeattype = 0
	if (emiss > depth) {
		write (ttyout, 201)
201		format (' NOTE: this feature is an emission feature.')
		ifeattype = -1 # emission feature
	} else {
		ifeattype = 1  # absorption band
	}
	if (ifeattype == -1 & maxch == 0) {
		write (ttyout,303) maxch
303		format ('ERROR: emission feature maximum channel=',
			i7)
		ier = 1
		return
	}
	if (ifeattype == 1 & minch == 0) {
		write (ttyout,304) minch
304		format ('ERROR: absorption feature minimum channel=',
			i7)
		ier = 1
		return
	}
#
# DEBUG:
#
#	write (ttyout,*) 'DEBUG: bdmset: cl1=',cl1,' cl2=',cl2,
#					' cr1=',cr1,' cr2=',cr2
#	do i = 1, cr2 {
#		write (ttyout,*) 'DEBUG: bdmset: wav(',i,')=,wav(i),
#					'  rflib(',i,')=,rflib(i),
#					'  rflibc(',i,')=,rflibc(i)
#	}
#	write (ttyout,*) 'DEBUG: bdmset: minch=',minch,' ier=',ier
#	write (ttyout,*) 'DEBUG: bdmset: maxch=',maxch
# END.DEBUG

#
#  done!
#
	return
	end

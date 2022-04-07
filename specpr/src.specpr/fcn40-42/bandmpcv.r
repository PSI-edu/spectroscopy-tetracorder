	subroutine bandmpcv (wav,rflibc,rfobs,
			cvl1,cvl2,cl1,cl2,cr1,cr2,cvr1,cvr2,
			cwav,ictrol,minch,
			maxch,ifeattype,iflag, ixpxl,iypxl,
                        rfobsc,rfcon,xk,bd,rfit,slope,yintcp,rftemp,conref,
			avlc,avrc)
	implicit integer*4 (i-n)
#ccc  name:  bandmp
#ccc  version date:  January 29, 1990, modified March 6, 2015 
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
#ccc
#ccc        cvl1   = curved continuum channel number begin on left side of band  I*4
#ccc        cvl2   = curved continuum channel number end on left side of band  I*4
#ccc
#ccc        cl1    = continuum channel number begin on left side of band  I*4
#ccc        cl2    = continuum channel number end on left side of band  I*4
#ccc                   note: cl2 >= cl1  (not checked)
#ccc        cr1    = continuum channel number begin on right side of band  I*4
#ccc                   note: cr1 > cl2 + 1 (not checked)
#ccc        cr2    = continuum channel number end on right side of band  I*4
#ccc                   note: cr2 >= cr1 (not checked)
#ccc                   note: cr2 also determines the max array sizes
#ccc
#ccc        cvr1   = curved continuum channel number begin on left side of band  I*4
#ccc        cvr2   = curved continuum channel number end on left side of band  I*4
#ccc
#ccc        cwav   = 4 wavelengths for the above 4 intervals
#ccc
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
#ccc        avlc   = left continuum reflectance value of observed spectrum
#ccc        avrc   = right continuum reflectance value of observed spectrum
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

#RED Test
#	include "../common/spdebug"

	integer*4  cvl1,cvl2,cl1,cl2,cr1,cr2,cvr1,cvr2
        integer*4  ictrol,minch,maxch,ifeattype
	integer*4  iflag,ixpxl,iypxl
	real*4 wav(cr2),rflibc(cr2),rfobs(cr2)
        real*4 rfobsc(cr2),rfcon(cr2),xk,xk1,bd,rfit, rfit2
	real*4 rftemp(cr2), slope, yintcp, xn
	real*4 top, bottom, botm2, bprime, drfit2, drfit
	real*4 conref,cvlcm avlc,avrc, cvrc, delpt

	real*4 cwav(4), cvconobs(4)  # 4 wavelength observed spectrum to define the curved continuum
	real*4 bpar(4)  # for cubic spline

	real*8 suml,sumll,sumol,sumo,sumoo,dxk,dbb,dxn
	real*8 dro, drl

	integer*4 nx  # number of channels in the curved continuum (4 here)
	integer*4 nchs # number of rows matrix scoef, here = nx=1 = 3
	integer*4 ideriv  # spline: no derivative
	real*4 scoeff(3,3)   # nx-1 by 3 matrix for the cubic spline coefficients

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
#      observed spectrum at the 4 continuum points: cvconobs(4)
#
# the 4 curved contin points are cvlc, avlc, avrc, cvrc

# first curved left continuum average:
	cvlc = 0.0
	#cvwlc = 0.0
	n = 0
	do i = cvl1, cvl2 {
		if (wav(i) == delpt || rflibc(i) == delpt ||
						rfobs(i) == delpt) next
		cvlc = cvlc + rfobs(i)
		#cvwlc = cvwlc + wav(i)
		n = n +1
	}

	if (n < 1) {
		if (ictrol == 1 & iflag == 0) write (ttyout,1201)
1201		format (' ERROR: all points in observed spectrum,',
                        ' curved end left continuum deleted.')
		if (ictrol == 1 & iflag == 1) write (ttyout,1211) ixpxl, iypxl
1211		format (' ERROR: all points in observed spectrum,',
                        ' curved end left continuum deleted: pixel',i4,',',i4)
		xk = delpt
		bd = delpt
		rfit = delpt
		return
	}
	xn = float(n)
	cvlc = cvlc / xn
	#cvwlc = cvwlc / xn
	if (cvlc < 0.1e-20) {   # if the continuum is less than zero, delete output
		if (ictrol == 1 & iflag == 0) write (ttyout,1241)
1241		format (' ERROR: curved end left continuum <0.0.')
		if (ictrol == 1 & iflag == 1) write (ttyout,1251) ixpxl, iypxl
1251		format (' ERROR: curved end left continuum <0.0r: pixel',i4,',',i4)
		xk = delpt
		bd = delpt
		rfit = delpt
		return
	}
	cvconobs(1) = cvlc  # left most curved continuum point


# the 4 curved contin points are cvlc, avlc, avrc, cvrc
# first band left continuum average:
	avlc = 0.0
	avwlc = 0.0
	n = 0
# debug
#	bugcount = bugcount + 1
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
	if (avlc < 0.1e-20) {   # if the continuum id less than zero, delete output
		if (ictrol == 1 & iflag == 0) write (ttyout,124)
124		format (' ERROR: left continuum <0.0.')
		if (ictrol == 1 & iflag == 1) write (ttyout,125) ixpxl, iypxl
125		format (' ERROR: left continuum <0.0r: pixel',i4,',',i4)
		xk = delpt
		bd = delpt
		rfit = delpt
		return
	}
	cvconobs(2) = avlc  # left side of feature continuum point

# the 4 curved contin points are cvlc, avlc, avrc, cvrc
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
	if (avrc < 0.1e-20) {   # if the continuum is less than zero, delete output
		if (ictrol == 1 & iflag == 0) write (ttyout,126)
126		format (' ERROR: right continuum <0.0.')
		if (ictrol == 1 & iflag == 1) write (ttyout,127) ixpxl, iypxl
127		format (' ERROR: right continuum <0.0r: pixel',i4,',',i4)
		xk = delpt
		bd = delpt
		rfit = delpt
		return
	}
	cvconobs(3) = avrc  # left side of feature continuum point

# the 4 curved contin points are cvlc, avlc, avrc, cvrc
# now right most curved continuum average:
	cvrc = 0.0
	#cvwrc = 0.0
	n = 0

	do i = cvr1, cvr2 {
		if (wav(i) == delpt || rflibc(i) == delpt ||
						rfobs(i) == delpt) next
		cvrc = cvrc + rfobs(i)
		#cvwrc = cvwrc + wav(i)
		n = n +1
	}

	if (n < 1) {
		if (ictrol == 1 & iflag == 0) write (ttyout,1229)
1229		format (' ERROR: all points in observed spectrum,',
                        ' curved end right continuum deleted.')
		if (ictrol == 1 & iflag == 1) write (ttyout,1239) ixpxl, iypxl
1239		format (' ERROR: all points in observed spectrum,',
                        ' curved end right continuum deleted: pixel',i4,',',i4)
		xk = delpt
		bd = delpt
		rfit = delpt
		return
	}

	xn = float(n)
	cvrc = cvrc / xn
	#cvwrc = cvwrc / xn
	if (cvrc < 0.1e-20) {   # if the continuum id less than zero, delete output
		if (ictrol == 1 & iflag == 0) write (ttyout,1269)
1269		format (' ERROR: curved end right continuum <0.0.')
		if (ictrol == 1 & iflag == 1) write (ttyout,1279) ixpxl, iypxl
1279		format (' ERROR: curved end right continuum <0.0r: pixel',i4,',',i4)
		xk = delpt
		bd = delpt
		rfit = delpt
		return
	}
	cvconobs(4) = cvrc  # left side of feature continuum point

	#write (ttyout,*)  'DEBUG: cwav    =', cwav(1),cwav(2),cwav(3),cwav(4)
	#write (ttyout,*)  'DEBUG: cvconobs=', cvconobs(1),cvconobs(2),cvconobs(3),cvconobs(4)

# now we have 4 pairs os x,y points to derive the curved continuum line
#       thus: wav = a * rfobs  + b

#     *** get spline coefficients ***
        ier=0
	nx = 4  # input = 4 channel spectrum
	nchs =3  # scoef matrix row size
	bpar(1) =0.0  # note, this should be done earlier for speed
	bpar(2) =0.0
	bpar(3) =0.0
	bpar(4) =0.0

        # icsicu finds the coefficients for the spline
	call icsicu(cwav, cvconobs,nx,bpar,scoeff,nchs,ier)

	#write (ttyout,*)  'DEBUG: scoeff=', scoeff

#     *** check return error flag ***
        if (ier==131 || ier==129 || ier==130) {
                write(ttyout,10) ier
10		format(' Error with cubic spline coefficients, in routine bandmpcv after call to:',/,
			' icsicu: Most probably the wavelength file used is not correct.',/,
			' Possibly due to non-sequential wavelengths with channel number.',/,
			' If you don''t think this is the',/,
                           'case, send mail to specpr and note error',
                           'number ',i3,//)

		write (ttyout,11) imat, ifeat
11		format (' ERROR on material', i6, ' feature', i6)

		write (ttyout,12) cwav(1), cwav(2), cwav(1), cwav(4)
12		format(' cwav: ', 1pe13.5,  1pe13.5,  1pe13.5,  1pe13.5, '= the 4 wavelengths in the curved continuum',/)

		#write (ttyout,*) 'DEBUG in bandmpcv after icsicu call:'
		#write (ttyout,*) '        4 cwav wavelengths=', cwav(1), cwav(2),cwav(3),cwav(4)

                call crtin
		xk = delpt
		bd = delpt
		rfit = delpt
                return
        }

        indx2 = cvl1   # 1st channel to compute
        nch0  = cvr2 - indx2 +1  # last channel to compute

#     *** use coefficients got above to compute the curved continuum ***
		### cwav = 4 wavelengths of the curved copntinuum
		### cvconobs observed spectrum averaged of the continuum intervals, at 4 wavelengths
		###  nx=4, the 4 continuum wavelengths
        ier=0
	ideriv=0  # no derivatives

	# icsevu computes the continuum (splined) spectrum from thxje coefficients scoeff
		### cwav = input 4 wavelengths
		### cvconobs = the 4 continuum values
		### nx = 4
		### scoeff derived spline coefficients from above
		### nchs = 3
		### wav      wavelengths of output spectrum (input)
		### rfcon    the computed continuum spectrum (output)
		### 
	call icsevu(cwav, cvconobs,nx,scoeff,nchs,
                wav(indx2),rfcon(indx2),nch0,ier,ideriv)

	#write (ttyout,*) 'DEBUG: wav(cvl1:cvr2)=', wav(cvl1:cvr2)
	#write (ttyout,*) ' '
        #write (ttyout,*) 'DEBUG: rfcon(cvl1:cvr2)=', rfcon(cvl1:cvr2)

#     *** check return error flag ***
# this check is probably not needed.  Even if a wavelength is deleted or
# outside the bounds, the icsevu routine computes the rest of the spectrum.
# may need to add deletion of points in the icsevu subroutine.  RNC 3/6/2015
#        if (ier==33 || ier==34) {
#                write(ttyout,11) ier
#11		format(' Error ',i5,' with cubic spline result from icsevu: Most probably the wavelength file used',/,
#			'is not correct.  If you don''t think this is the',/,
#                           'case, send mail to specpr and note error',
#                           'number ',i3,//)
#                call crtin
		#xk = delpt
		#bd = delpt
		#rfit = delpt
                #return
#        }



# OLD linear continuum:
	bottom = avwrc - avwlc
	if (abs(bottom) < 0.1e-20) {
		if (ictrol == 1) write (ttyout, 130)
130	format (' ERROR: wavelength range of continuum is',' too small!')
		xk = delpt
		bd = delpt
		rfit = delpt
		return
	}


	a = (avrc - avlc)/bottom
	b = avrc - a * avwrc

	slope = a
	yintcp = b

### rfobsc   the computed continuum-removed spectrum (output)
#
# now we can continuum correct the observed spectrum.
#     compute rfobsc
#
# don't do this part because speed is reduced
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

	if (iflag == 1) {        # imaging mode, do minimum, just the core feature and bounding continuum, popints
		il = cl2 + 1
		ir = cr1 - 1
	} else {                 # single spectrum mode, do more, the full range of the 4-ppint continuum
		il = cl1
		ir = cr2
	}


	do i = il, ir {
		if (wav(i) == delpt || rflibc(i) == delpt ||
						rfobs(i) == delpt) {
			rfobsc(i) = delpt
		} else {
			if (abs(rfcon(i)) > 0.1e-20) {
				rfobsc(i) = rfobs(i) / rfcon(i)    # continuum removed spectrum
			} else {
				rfobsc(i) = delpt
			}
		}
	}
        #write (ttyout,*) 'DEBUG: rfobsc(il:ir)=', rfobsc(il:ir)
	# continuum reflectance value of observed spectrum at band min/max
	if (ifeattype == 1)  {
		if (wav(minch) == delpt) {
			conref = delpt
		} else {
			conref = rfcon(minch)
		}
	} else {
		if (wav(maxch) == delpt) {
			conref = delpt
		} else {
			conref = rfcon(maxch)
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
140		format (' ERROR: routine bandmpcv: sums in least ',
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
	if (abs(bottom) < 0.1e-20) {
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

	if (abs(bb) < 0.1e-20) {   # xk too large (b too small)
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
	if (abs(xk1) < 0.1e-20) {
		
		if (ictrol == 1 & iflag == 0) write (ttyout,222)
222		format (' ERROR: bandmp xk1 value too small,',
                        ' output fit, depth deleted.')
		if (ictrol == 1 & iflag == 1) write (ttyout,223) ixpxl, iypxl
223		format (' ERROR: bandmp xk1 value too small,',
                        ' output fit, depth deleted: pixel',i4,',',i4)
		bd = delpt
		rfit = delpt
		return
	}

# debug
#        if (bugcount == bugstop) {
#                write (ttyout,*) 'DEBUG12'
#		write (ttyout,*) 'il=',il,' ir=',ir 
#        }
# debug
	do i = il, ir {
		if (rflibc(i) == delpt) {
			rftemp(i) = delpt
		} else {
# debug
#			if (bugcount == bugstop & i == 0) {
# RED The following code within the if that will never be true,
#     is needed to get around a REAL OVERFLOW condition resulting
#     in an arithmetic trap.  Reason is not yet known and the problem
#     has been reported to HP (case # 3200930885).
			if (i == 0) {
				write (ttyout,*) 'i=',i
				write (ttyout,*) 'rftemp(i)=',rftemp(i)
				write (ttyout,*) 'rflibc(i)=',rflibc(i)
				write (ttyout,*) 'xk       =',xk
				write (ttyout,*) 'xk1      =',xk1
			}
# debug
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
	if (abs(botm2) < 0.1e-20) {
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
# 	write (ttyout,*) 'DEBUG: bandmapcv: xn=',xn
# 	write (ttyout,*) 'DEBUG: bandmapcv: suml=',suml,
#				' sumll=',sumll
# 	write (ttyout,*) 'DEBUG: bandmapcv: sumol=',sumol,
#				' sumo=',sumo,
#				' sumoo=',sumoo
#	write (ttyout,*) 'DEBUG: bandmapcv: xk=',xk,
#				' bd=',bd,' rfit2=',rfit2,' rfit=',rfit
#	write (ttyout,*) 'DEBUG: bandmapcv: slope=',slope,
#				' yintcp=',yintcp
#	write (ttyout,*) 'DEBUG: bandmapcv: top=',top,
#				' bottom=',bottom,
#				' botm2=',botm2
#	write (ttyout,*) 'DEBUG: bandmapcv: bb=',bb,
#				' bprime=',bprime
#	do i = cl1, cr2 {
#		write (ttyout,*) 'DEBUG: bandmapcv: rfobsc(',i,')=',
#				rfobsc(i), ' rftemp(',i,')=',rftemp(i)
#	}
#	write (ttyout,*) 'DEBUG: bandmp: minch=',minch,' iflag=',iflag
# END.DEBUG
#
	return
	end

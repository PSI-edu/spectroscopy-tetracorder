	subroutine comptetf (waves, datac, nchans, xmax, xmin, lbnd, diff)

	implicit none

#ccc  name: comptetf
#ccc  version date: 11/23/2018
#ccc  author(s): Roger N. Clark
#ccc  language: Ratfor
#ccc
#ccc  short description: display tetracorder feature definition on plot 
#ccc                     
#ccc
#ccc  algorithm description:
#ccc  system requirements: none
#ccc  subroutines called: none
#ccc  argument list description: see short description
#ccc  parameter description: none
#ccc  common description: none
#ccc  message files referenced: none
#ccc  internal variables:
#ccc  file description: none
#ccc  user command lines: none
#ccc  update information:
#ccc  NOTES:
#ccc

	include "../common/spmaxes"   # max parameters, must be first
	include "../common/lundefs"
	include "../common/tetfeat"
	include "../common/alphabet"
	include "../common/deletep"
	include "../common/dscrch"
	include "../common/hptrm"

	integer*4 nchans, i, j, ifeat, ier, k, m
	integer*4 ich1, ich2, icurvchans, nx, ic
	integer*4 isort(SPMAXCHAN), ideriv

	real*4 waves(nchans), datac(nchans), xmax, xmin, lbnd, diff
	real*4 lc, rc, bb, lcmin, lcmax, bbmin, bbmax, rcmin, rcmax
	real*4 w1, w2, y1, y2, ydel
	real*4 lcc, lccmin, lccmax,  rcc, rccmin, rccmax
	real*4 curvwav(4), curvcontin(4)   # 4-points for curved continuum
	real*4 bpar(4), scoeff(SPMAXCHAN,3)
	
	equivalence (isort(1),datsc4(1))
	equivalence (scoeff,datsc1)
	

	do ifeat = 1, imax3pt {

		#    if (tfmode(ifeat) > 0) {
		#	write (ttyout,*) 'DEBUG comptetf: ifeat=', ifeat, ' mode=', tfmode(ifeat)
		#    }

	 if (tfmode(ifeat) > 0 && tetonoff(ifeat) > 0) {      # feature defined and enabled for plot

                # bd1= 1.04um   W  0.958   0.986   1.02     1.05      1.080   1.110
		#write (ttyout,*) 'DEBUG comptetf: ifeat=', ifeat, ' mode=', tfmode(ifeat)
		#write (ttyout,*) 'DEBUG comptetf:', tleftwave(1,ifeat), tleftwave(2,ifeat)
		#write (ttyout,*) 'DEBUG comptetf:', tctrwave(1,ifeat),  tctrwave(2,ifeat)
		#write (ttyout,*) 'DEBUG comptetf:', trightwave(1,ifeat),  trightwave(2,ifeat)

	   if (tfmode(ifeat) == 2 && tfcont(ifeat) == 1) {    # features defined in wavelength, linear continuum

		#              waves manchan     wav1                  wav2
		call wtochbin (waves, nchans, tleftwave(1,ifeat), tleftwave(2,ifeat),  tleftchan(1,ifeat), tleftchan(2,ifeat), ier)

		if (ier != 0) write (ttyout,*) 'ERROR: getting wavelengths in comptetf, left continuum, error=', ier 

		call wtochbin (waves, nchans, trightwave(1,ifeat), trightwave(2,ifeat),  trightchan(1,ifeat), trightchan(2,ifeat), ier)

		if (ier != 0) write (ttyout,*) 'ERROR: getting wavelengths in comptetf, right continuum, error=', ier 

	   }
	   if (tfmode(ifeat) == 2 && tfcont(ifeat) == 2) {    # features defined in wavelength, curved continuum

		#              waves manchan     wav1                  wav2
		call wtochbin (waves, nchans, tleftcwave(1,ifeat), tleftcwave(2,ifeat),  tleftcchan(1,ifeat), tleftcchan(2,ifeat), ier)

		if (ier != 0) write (ttyout,*) 'ERROR: getting wavelengths in comptetf, left curved continuum, error=', ier 

		call wtochbin (waves, nchans, tleftwave(1,ifeat), tleftwave(2,ifeat),  tleftchan(1,ifeat), tleftchan(2,ifeat), ier)

		if (ier != 0) write (ttyout,*) 'ERROR: getting wavelengths in comptetf, left continuum, error=', ier 

		call wtochbin (waves, nchans, trightwave(1,ifeat), trightwave(2,ifeat),  trightchan(1,ifeat), trightchan(2,ifeat), ier)

		if (ier != 0) write (ttyout,*) 'ERROR: getting wavelengths in comptetf, right continuum, error=', ier 

		call wtochbin (waves, nchans, trightcwave(1,ifeat), trightcwave(2,ifeat),  trightcchan(1,ifeat), trightcchan(2,ifeat), ier)

		if (ier != 0) write (ttyout,*) 'ERROR: getting wavelengths in comptetf, right curved continuum, error=', ier 

	   }

		#write (ttyout,*) 'DEBUG comptetf:', tfeatname(ifeat)
		#write (ttyout,*) 'DEBUG', tleftchan(1,ifeat), tleftchan(2,ifeat) 
		#write (ttyout,*) 'DEBUG',trightchan(1,ifeat),trightchan(2,ifeat)

		#write (ttyout,*) 'DEBUG comptetf: computing averages'

		# set color

		# Colors defined in xinit.c, xset_color:
		#
		#   black     0
		#   gray      1 
		#   red       2
		#   blue      3
		#   green     4
		#   orange    5
		#   cyan      6
		#   magenta   7
		#   purple    8
		#   brown     9
		#   white    10

		if (igrmod >= 50 && igrmod <= 53) {

		   if (ifeat < 8) {
#XWIN			call xset_color(ifeat+1)   # 2=red, 3=blue, 4=green, 5=orange, 6=magenta
			write (ttyout,*) "DEBUG: comptetf xset_color= ", ifeat+1
		   } else {

#XWIN			call xset_color(1)   # 1=gray, 2=red, 3=blue, 4=green, 5=orange, 6=magenta
			write (ttyout,*) "DEBUG: comptetf xset_color= 1"
		   }
		}

	# now find the min and max data range for each continuum interval (and band bottom)


	   if (tfmode(ifeat) == 2 && tfcont(ifeat) == 2) {   # curved contiuum
		lcc =0.0
		lccmin =  1.e+30
		lccmax = -1.e-30
		j=0
		do i = tleftcchan(1,ifeat), tleftcchan(2,ifeat) {

		   if (datac(i) > delptup && waves(i) > delptup && datac(i) > -0.1) {    # not a deleted point
			lcc = lcc + datac(i)
			if (datac(i) > lccmax) lccmax = datac(i)
			if (datac(i) < lccmin) lccmin = datac(i)
			j = j +1
		   }
		}
		if ( j > 0 ) {
	   	   lcc = lcc /float(j)
		} else {
		   lcc = delpt
		}
		tlccavg(ifeat) = lcc
	   }

	   lc =0.0
	   lcmin =  1.e+30
	   lcmax = -1.e-30
	   j=0
	   do i = tleftchan(1,ifeat), tleftchan(2,ifeat) {

		if (datac(i) > delptup && waves(i) > delptup && datac(i) > -0.1) {    # not a deleted point
			lc = lc + datac(i)
			if (datac(i) > lcmax) lcmax = datac(i)
			if (datac(i) < lcmin) lcmin = datac(i)
			j = j +1
		}
	   }
	   if ( j > 0 ) {
	   	lc = lc /float(j)
	   } else {
		lc = delpt
	   }
	   tlcavg(ifeat) = lc

	   bb =0.0
	   bbmin =  1.e+30
	   bbmax = -1.e-30
	   #j=0
	   #do i = tctrchan(1,ifeat), tctrchan(2,ifeat) {
#
#		if (datac(i) > delptup) {    # not a deleted point
#			bb = bb + datac(i)
#
#			if (datac(i) > bbmax) bbmax = datac(i)
#			if (datac(i) < bbmin) bbmin = datac(i)
#			j = j +1
#		}
#	   }
#	   if ( j > 0 ) {
#	   	bb = bb /float(j)
#	   } else {
#		bb = delpt
#	   }
#	   tbbavg(ifeat) = bb

	   rc =0.0
	   rcmin =  1.e+30
	   rcmax = -1.e-30
	   j=0
	   do i = trightchan(1,ifeat), trightchan(2,ifeat) {

		if (datac(i) > delptup && waves(i) > delptup && datac(i) > -0.1) {    # not a deleted point
			rc = rc + datac(i)
			if (datac(i) > rcmax) rcmax = datac(i)
			if (datac(i) < rcmin) rcmin = datac(i)
			j = j +1
		}
	   }
	   if ( j > 0 ) {
	   	rc = rc /float(j)
	   } else {
		rc = delpt
	   }
	   trcavg(ifeat) = rc


	   if (tfmode(ifeat) == 2 && tfcont(ifeat) == 2) {   # curved contiuum
		rcc =0.0
		rccmin =  1.e+30
		rccmax = -1.e-30
		j=0
		do i = trightcchan(1,ifeat), trightcchan(2,ifeat) {

		   if (datac(i) > delptup && waves(i) > delptup && datac(i) > -0.1) {    # not a deleted point
			rcc = rcc + datac(i)
			if (datac(i) > rccmax) rccmax = datac(i)
			if (datac(i) < rccmin) rccmin = datac(i)
			j = j +1
		   }
		}
		if ( j > 0 ) {
	   	   rcc = rcc /float(j)
		} else {
		   rcc = delpt
		}
		trccavg(ifeat) = rcc
	   }

		   #tbdepth(ifeat) = (1.0 - bb) / ((rc+lc)/2.0)

		   #write (ttyout,*) 'DEBUG comptetf: lc, bb, rc:', lc, bb, rc

#		   write (ttyout, 100) sfeatname(ifeat), sbdepth(ifeat) 
100		   format (1x, a, "bdepth=", f8.4)

		   # now plot box if enabled

		ydel = diff*0.01   # amount to increase box size by

		# leftmoset in curved continuum
		if (tfcont(ifeat) == 2) {

			w1=tleftcwave(1,ifeat)
			w2=tleftcwave(2,ifeat)
			y1 = lccmax + ydel
			y2 = lccmin - ydel
			call pltbox1 (w1, y1, w2, y2, xmax, xmin, lbnd, diff)
		}

		# left continuum (both liner and curved):
		w1=tleftwave(1,ifeat)
		w2=tleftwave(2,ifeat)
		y1 = lcmax + ydel
		y2 = lcmin - ydel
		call pltbox1 (w1, y1, w2, y2, xmax, xmin, lbnd, diff)

		# right continuum (both liner and curved):
		w1=trightwave(1,ifeat)
		w2=trightwave(2,ifeat)
		y1 = rcmax + ydel
		y2 = rcmin - ydel
		call pltbox1 (w1, y1, w2, y2, xmax, xmin, lbnd, diff)

		# leftmoset in curved continuum
		if (tfcont(ifeat) == 2) {

			w1=trightcwave(1,ifeat)
			w2=trightcwave(2,ifeat)
			y1 = rccmax + ydel
			y2 = rccmin - ydel
			call pltbox1 (w1, y1, w2, y2, xmax, xmin, lbnd, diff)

		}

		# now plot the linear continuum

		if (tfcont(ifeat) == 1) {   # linear continuum
			w1 = (tleftwave(1,ifeat)  + tleftwave(2,ifeat)) /2.0
			w2 = (trightwave(1,ifeat) + trightwave(2,ifeat))/2.0
			call pltline (w1, lc, w2, rc, xmax, xmin, lbnd, diff)
		}

		# band bottom:
		#w1=sctrwave(1,ifeat)
		#w2=sctrwave(2,ifeat)
		#y1 = bbmax + ydel
		#y2 = bbmin - ydel
		#call pltbox1 (w1, y1, w2, y2, xmax, xmin, lbnd, diff)

		# now plot the curved continuum

		if (tfcont(ifeat) == 2) {   # curved continuum

			curvwav(1) = (tleftcwave(1,ifeat)  + tleftcwave(2,ifeat)) /2.0
			curvwav(2) = (tleftwave(1,ifeat)  + tleftwave(2,ifeat)) /2.0
			curvwav(3) = (trightwave(1,ifeat) + trightwave(2,ifeat))/2.0
			curvwav(4) = (trightcwave(1,ifeat) + trightcwave(2,ifeat))/2.0

			curvcontin(1) = tlccavg(ifeat)
			curvcontin(2) = tlcavg(ifeat)
			curvcontin(3) = trcavg(ifeat)
			curvcontin(4) = trccavg(ifeat)
			
			# use datsc5 for wavlenghs
			# use datsc6 for curved continuum value

			call wtochbin (waves, nchans, curvwav(1), curvwav(4), ich1, ich2, ier)

			icurvchans = ich2 - ich1 + 1

			m = ich1
			do  k = 1, icurvchans {

				datsc5(k) = waves(m)   # waves
				m = m +1
			}

			# sort the output wavelengths 

			call bubble(datsc5,isort,icurvchans)

			do  k = 1, icurvchans {

				datsc5(k) = datsc5(isort(k))
			}

			# find cubic spline coefficients

			nx=4
			ic = nx -1
			do  i = 1,4
				bpar(i) = 0.0
			ier=0
			call icsicu (curvwav, curvcontin, nx, bpar, scoeff,ic,ier)
			if (ier > 0) {
				write(ttyout,*) "curved continuum icsicu calculation error ", ier
			}
			ideriv = 0  # no derivative
			call icsevu (curvwav, curvcontin, nx, scoeff, ic, datsc5, datsc6, icurvchans, ier, ideriv)
			if (ier > 0) {
				write(ttyout,*) "curved continuum icsevu calculation error ", ier
			}

			# now plot the curve
			do  k = 1, icurvchans-1 {

				w1 = datsc5(k)
				lc = datsc6(k)
				w2 = datsc5(k+1)
				rc = datsc6(k+1)
				call pltline (w1, lc, w2, rc, xmax, xmin, lbnd, diff)
			}

		}

	  }
	}
	return
	end

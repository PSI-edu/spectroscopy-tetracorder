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

	integer*4 nchans, i, j, ifeat, ier

	real*4 waves(nchans), datac(nchans), xmax, xmin, lbnd, diff
	real*4 lc, rc, bb, lcmin, lcmax, bbmin, bbmax, rcmin, rcmax
	real*4 w1, w2, y1, y2, ydel

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

	   if (tfmode(ifeat) == 2) {    # features defined in wavelength

		#              waves manchan     wav1                  wav2
		call wtochbin (waves, nchans, tleftwave(1,ifeat), tleftwave(2,ifeat),  tleftchan(1,ifeat), tleftchan(2,ifeat), ier)

		if (ier != 0) write (ttyout,*) 'ERROR: getting wavelengths in comptetf, error=', ier 

		call wtochbin (waves, nchans, trightwave(1,ifeat), trightwave(2,ifeat),  trightchan(1,ifeat), trightchan(2,ifeat), ier)

		if (ier != 0) write (ttyout,*) 'ERROR: getting wavelengths in comptetf, error=', ier 

	   }

		#write (ttyout,*) 'DEBUG comptetf:', tfeatname(ifeat)
		#write (ttyout,*) 'DEBUG', tleftchan(1,ifeat), tleftchan(2,ifeat) 
		#write (ttyout,*) 'DEBUG',trightchan(1,ifeat),trightchan(2,ifeat)

		#write (ttyout,*) 'DEBUG comptetf: computing averages'
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

		   #tbdepth(ifeat) = (1.0 - bb) / ((rc+lc)/2.0)

		   #write (ttyout,*) 'DEBUG comptetf: lc, bb, rc:', lc, bb, rc

#		   write (ttyout, 100) sfeatname(ifeat), sbdepth(ifeat) 
100		   format (1x, a, "bdepth=", f8.4)

		   # now plot box if enabled

		ydel = diff*0.01   # amount to increase box size by

		# left continuum:
		w1=tleftwave(1,ifeat)
		w2=tleftwave(2,ifeat)
		y1 = lcmax + ydel
		y2 = lcmin - ydel
		call pltbox1 (w1, y1, w2, y2, xmax, xmin, lbnd, diff)

		# right continuum:
		w1=trightwave(1,ifeat)
		w2=trightwave(2,ifeat)
		y1 = rcmax + ydel
		y2 = rcmin - ydel
		call pltbox1 (w1, y1, w2, y2, xmax, xmin, lbnd, diff)

		w1 = (tleftwave(1,ifeat)  + tleftwave(2,ifeat)) /2.0
		w2 = (trightwave(1,ifeat) + trightwave(2,ifeat))/2.0
		call pltline (w1, lc, w2, rc, xmax, xmin, lbnd, diff)

		# band bottom:
		#w1=sctrwave(1,ifeat)
		#w2=sctrwave(2,ifeat)
		#y1 = bbmax + ydel
		#y2 = bbmin - ydel
		#call pltbox1 (w1, y1, w2, y2, xmax, xmin, lbnd, diff)


	  }
	}
	return
	end

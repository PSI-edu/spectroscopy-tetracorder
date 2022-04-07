	subroutine comp3ptbd (waves, datac, nchans, xmax, xmin, lbnd, diff)

	implicit none

#ccc  name: comp3ptbd
#ccc  version date: 11/23/2018
#ccc  author(s): Roger N. Clark
#ccc  language: Ratfor
#ccc
#ccc  short description: compute band depths on the spectrum
#ccc                     for the use defined bands in
#ccc                     common spcfeat
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
	include "../common/sp3pfeat"
	include "../common/alphabet"
	include "../common/deletep"

	integer*4 nchans, i, j, ifeat, ier

	real*4 waves(nchans), datac(nchans), xmax, xmin, lbnd, diff
	real*4 lc, rc, bb, lcmin, lcmax, bbmin, bbmax, rcmin, rcmax
	real*4 w1, w2, y1, y2, ydel

	do ifeat = 1, imax3pt {

		#    if (sfmode(ifeat) > 0) {
		#	write (ttyout,*) 'DEBUG comp3ptbd: ifeat=', ifeat, ' mode=', sfmode(ifeat)
		#    }

	  if (sfmode(ifeat) > 0) {      # feature defined

                # bd1= 1.04um   W  0.958   0.986   1.02     1.05      1.080   1.110
		#write (ttyout,*) 'DEBUG comp3ptbd: ifeat=', ifeat, ' mode=', sfmode(ifeat)
		#write (ttyout,*) 'DEBUG comp3ptbd:', sleftwave(1,ifeat), sleftwave(2,ifeat)
		#write (ttyout,*) 'DEBUG comp3ptbd:', sctrwave(1,ifeat),  sctrwave(2,ifeat)
		#write (ttyout,*) 'DEBUG comp3ptbd:', srightwave(1,ifeat),  srightwave(2,ifeat)

	   if (sfmode(ifeat) == 2) {    # features defined in wavelength

		#              waves manchan     wav1                  wav2
		call wtochbin (waves, nchans, sleftwave(1,ifeat), sleftwave(2,ifeat),  sleftchan(1,ifeat), sleftchan(2,ifeat), ier)

		if (ier != 0) write (ttyout,*) 'ERROR: getting wavelengths in comp3ptbd, error=', ier 

		call wtochbin (waves, nchans, sctrwave(1,ifeat), sctrwave(2,ifeat),  sctrchan(1,ifeat), sctrchan(2,ifeat), ier)

		if (ier != 0) write (ttyout,*) 'ERROR: getting wavelengths in comp3ptbd, error=', ier 

		call wtochbin (waves, nchans, srightwave(1,ifeat), srightwave(2,ifeat),  srightchan(1,ifeat), srightchan(2,ifeat), ier)

		if (ier != 0) write (ttyout,*) 'ERROR: getting wavelengths in comp3ptbd, error=', ier 

	   }

		#write (ttyout,*) 'DEBUG comp3ptbd:', sfeatname(ifeat)
		#write (ttyout,*) 'DEBUG', sleftchan(1,ifeat), sleftchan(2,ifeat) 
		#write (ttyout,*) 'DEBUG', sctrchan(1,ifeat), sctrchan(2,ifeat) 
		#write (ttyout,*) 'DEBUG',srightchan(1,ifeat),srightchan(2,ifeat)

		#write (ttyout,*) 'DEBUG comp3ptbd: computing averages'
	   lc =0.0
	   lcmin =  1.e+30
	   lcmax = -1.e-30
	   j=0
	   do i = sleftchan(1,ifeat), sleftchan(2,ifeat) {

		if (datac(i) > delptup) {    # not a deleted point
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
	   slcavg(ifeat) = lc

	   bb =0.0
	   bbmin =  1.e+30
	   bbmax = -1.e-30
	   j=0
	   do i = sctrchan(1,ifeat), sctrchan(2,ifeat) {

		if (datac(i) > delptup) {    # not a deleted point
			bb = bb + datac(i)

			if (datac(i) > bbmax) bbmax = datac(i)
			if (datac(i) < bbmin) bbmin = datac(i)
			j = j +1
		}
	   }
	   if ( j > 0 ) {
	   	bb = bb /float(j)
	   } else {
		bb = delpt
	   }
	   sbbavg(ifeat) = bb

	   rc =0.0
	   rcmin =  1.e+30
	   rcmax = -1.e-30
	   j=0
	   do i = srightchan(1,ifeat), srightchan(2,ifeat) {

		if (datac(i) > delptup) {    # not a deleted point
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
	   srcavg(ifeat) = rc

	   if ( lc > 0.0 && rc > 0.0 && bb > 0.0 ) {
		   sbdepth(ifeat) = 1.0 - (bb / ((rc+lc)/2.0))

		#   write (ttyout,*) 'DEBUG comp3ptbd ',
		#		 'ifeat, lc, bb, rc:', 
		#		  ifeat, lc, bb, rc

		   #write (ttyout, 100) sfeatname(ifeat), sbdepth(ifeat) 
100		   format (1x, a, "bdepth=", f8.4)

		   # now plot box if enabled

		   if (spcbox(ifeat) == 1 && sonoff(ifeat) > 0) {
			ydel = diff*0.01   # amount to increase box size by

			# left continuum:
			w1=sleftwave(1,ifeat)
			w2=sleftwave(2,ifeat)
			y1 = lcmax + ydel
			y2 = lcmin - ydel
			call pltbox1 (w1, y1, w2, y2, xmax, xmin, lbnd, diff)

			# right continuum:
			w1=srightwave(1,ifeat)
			w2=srightwave(2,ifeat)
			y1 = rcmax + ydel
			y2 = rcmin - ydel
			call pltbox1 (w1, y1, w2, y2, xmax, xmin, lbnd, diff)

			w1 = (sleftwave(1,ifeat)  + sleftwave(2,ifeat)) /2.0
			w2 = (srightwave(1,ifeat) + srightwave(2,ifeat))/2.0
			call pltline (w1, lc, w2, rc, xmax, xmin, lbnd, diff)

			# band bottom:
			w1=sctrwave(1,ifeat)
			w2=sctrwave(2,ifeat)
			y1 = bbmax + ydel
			y2 = bbmin - ydel
			call pltbox1 (w1, y1, w2, y2, xmax, xmin, lbnd, diff)
		   }

	   } else {
		   sbdepth(ifeat) = delpt
		   write (ttyout, 101)  sfeatname(ifeat)
101		   format (1x, a, "bdepth= deleted")
	   }

	  }
	}
	return
	end

	subroutine f37(ic)
	implicit integer*4 (i-n)
#ccc  name:  f37
#ccc  version date:  December 19, 1994
#ccc  author(s):  Roger N. Clark
#ccc  language:  Ratfor
#ccc
#ccc  short description: 
#ccc            This program does a Normalized Veg Red-Edge Shift calculation
#ccc		
#ccc            It first normalized the depth of an absorption edge,
#ccc            then ratios a reference spectrum to the unknown, then fits
#ccc            a ratio reference feature (e.g. derived from a shift/unshifted
#ccc            spectrum) using the bandmp (f37) fitting routine.
#ccc
#ccc  algorithm description see bandmp subroutine
#ccc  system requirements:
#ccc  subroutines called:
#ccc
#ccc
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

	include "../common/blank"
	include "../common/lbl6"
	include "../common/lbl3"
	include "../common/label1"
	include "../common/lbl7"
	include "../common/lblg"
	include "../common/lundefs"
	include "../common/alphabet"
	include "../common/dscrch"

#RED
	integer*4 iwidok   # function

	real*4 rflibc(SPMAXCHAN), vegspec(SPMAXCHAN)
	equivalence (datsc1,rflibc), (datsc3,vegspec)

	character*8 inam, inamr, inamwi, inamveg
	character*26 cmode

	integer*4 nch1, nch2, nch3, nch4
	integer*4 iflveg, idveg
	real*4 a1, b1,xndvivegspec,xndviobs,xfactor,bdnorm
	

#	load data and wavelengths
	if (ictrl==-1) {
		ic=ihx
		write(ttyout,55)
		return
	}

	do i=1,maxchn {
		data(i)=0.0
		datab(i)=dataa(i)   # this is the observed spectrum
		datac(i)=0.0
		error(i)=0.0
	}

	delpt = -1.23e34

	call eralph
50	call whedr2
	write(ttyout,115) idv1,ifl1,ititl

140	call crtin
	i=1
150	call wjfren (i,a,il)
	if (iwidok(il) == 1) {
		iwtmpf = il
		call wjfren (i,b,ik)
		if (b<=0 || b>maxrec) {
			call what (i)
			write(ttyout,165)
			go to 140
		} else {
			irecw=b
			call wavlng (iwtmpf,irecw,ier)
			if (ier != 0) {
				write (ttyout,165)
				go to 140
			}
			itrol(1) = iwtmpf
			itrol(2) = irecw
			call whedr2
		}
		if (ik!=0) i=i-1
		go to 150
	} else  if (il==ihx || il==ihe)  {
		ic=il
		return
	} else {
			irecw=itrol(2)
			iwtmpf = itrol(1)
			call wavlng (iwtmpf,irecw,ier)
			if (ier != 0) {
				write (ttyout,165)
				go to 140
			}
	}


	call namdwv(itrol(1),inamwi)  #name of input wavelength set

# read error bars if requested

	idsav = idad
	if (ictrl == ihe) {
		write(ttyout,60)
60		format ('Including error bars to observed spectrum',//)
		idad=2
		call devok (4,idv1,ifl1,lun,ier)
		if (ier!=0) {
			ic = ihe
			return
		}
		itmp = ifl1
		call redfil(itmp,lun,ier) # position before errors
		if (ier != 0) {
			ic = ihe
			return
		}
		itmp = itmp + 1
		call redfil(itmp,lun,ier) # read error bars
		if (ier != 0) {
			ic = ihe
			return
		}
		do i= 1,nchans {
			error(i)=data(i)
		}
	}

#     ********************************************
#     * get "red-edge" reference data set
#     ********************************************


59      write(ttyout,57)
57      format (' Enter file id and record number for the',
			' RED-EDGE REFERENCE SPECTRUM,',
			/, ' or e or x to exit.'/)

	call crtin
	i = 1
	call wjfren(i,x1,idveg)
	if (i >= 79 && idveg == 0) {
		call what(i)
		go to 59
	}
	call wjfren(i,xfilb,ic2)

#        *** check for hard or soft exit ***
	if (idveg==ihe || ic2==ihe) {
		ic = ihe
		return
	} else if (idveg==ihx || ic2==ihx) {
		ic = ihx
		return

#       *** check for invalid input ***
	} else if (x1!=0.0 || ic2!=0 || xfilb<=0.0) {
		write(ttyout,66)
66		format ('invalid input, reenter')
		go to 59

#       *** looks ok so get file b ***
	} else {
		iflveg = xfilb
		call devok (4,idveg,iflveg,lun,ier)
		if (ier!=0) {
			go to 59
		}
		itmp = iflveg
		call redfil(itmp,lun,ier)
		if (ier != 0) {
			go to 59
		}
		iform = 1
		write (ttyout,58) idveg, iflveg, ititl
58		format ('RED-EDGE Reference spectrum: ',/,a,i7,5x,a,//)
		call namdev (idveg, inamveg)

		do i= 1,nchans {
			vegspec(i) = data(i)
		}
	}

#     ********************************************
#     * get library data file
#     ********************************************

69      write(ttyout,70)
70      format (' Enter file id and record number for the',
			' SHIFT-FEATURE REFERENCE SPECTRUM,',
			/, ' or e or x to exit.'/)

	call crtin
	i = 1
	call wjfren(i,x1,idevb)
	if (i >= 79 && idevb == 0) {
		call what(i)
		go to 69
	}
	call wjfren(i,xfilb,ic2)

#        *** check for hard or soft exit ***
	if (idevb==ihe || ic2==ihe) {
		ic = ihe
		return
	} else if (idevb==ihx || ic2==ihx) {
		ic = ihx
		return

#       *** check for invalid input ***
	} else if (x1!=0.0 || ic2!=0 || xfilb<=0.0) {
		write(ttyout,66)
		go to 69

#       *** looks ok so get file b ***
	} else {
		ifilb = xfilb
		call devok (4,idevb,ifilb,lun,ier)
		if (ier!=0) {
			go to 69
		}
		itmp = ifilb
		call redfil(itmp,lun,ier)
		if (ier != 0) {
			go to 69
		}
		iform = 1
		write (ttyout,68) idevb, ifilb, ititl
68		format (' Reference spectrum: ',a,i7,5x,a,//)
		call namdev (idevb, inamr)
	}

#	continuum points

298	write (ttyout,300)
300	format (1x, 'LEFT and RIGHT CONTINUUM INTERVALS:',//,
		'Enter two channel numbers to describe the continuum',
			' interval on the ',/,5x,
			'LEFT side of the absorption, and',//,
		'Enter two channel numbers to describe the continuum',
			' interval on the ',/,5x,
			'RIGHT side of the absorption ',//,
		' (enter 4 channel numbers total)',/,
		' (Optional: AFTER 4 continuum numbers,',/,
		'            enter  i  for duplication of imaging mode.',/,
		' (Optional: BEFORE 4 continuum numbers,',/,
		'            enter  w  for continuum as wavelengths.)')
	call crtin
	i=1
	call wjfren (i,x,il)        # get channel 1
	if (il==ihx || il==ihe) {
		ic=il
		return
	}
	if (il == ihw) {
		icmode = 1
	} else {
		icmode = 0
	}

	if (icmode == 1) { # values in wavelengths
		call wjfren (i,x,il)      # get wav1
		if (il != 0 || x < 0.1e-30) {
			call what(i)
			write (ttyout, 311)
311			format ('ERROR: WAVELENGTH OUT OF RANGE', //)
			go to 298
		}
		wav1 = x

		call wjfren (i,x,il)      # get wav2
		if (il != 0 || x < 0.1e-30) {
			call what(i)
			write (ttyout, 311)
			go to 298
		}
		wav2 = x

		call wjfren (i,x,il)      # get wav3
		if (il != 0 || x < 0.1e-30) {
			call what(i)
			write (ttyout, 311)
			go to 298
		}
		wav3 = x

		call wjfren (i,x,il)      # get wav4
		if (il != 0 || x < 0.1e-30) {
			call what(i)
			write (ttyout, 311)
			go to 298
		}
		wav4 = x
		if (il == ihi) i = i -1  # i set for imaging mode (gotten later)

		call wtochbin (dataa, nchans, wav1, wav2, nch1, nch2, ier)
		if (ier != 0) go to 298

		call wtochbin (dataa, nchans, wav3, wav4, nch3, nch4, ier)
		if (ier != 0) go to 298

		write (ttyout,309) wav1, wav2, wav3, wav4,
                                   nch1, nch2, nch3, nch4
309		format (' NOTE: Wavelengths      ',3(f11.5,4x),f11.5,/,
                        '  translate to channels:',3(i6,9x),i6,/)

	} else {
	
		nch1=x
		if (nch1 < 1 || nch1 > nchans) {
			call what(i)
			write (ttyout, 310)
310			format ('ERROR: CHANNEL OUT OF RANGE', //)
			go to 298
		}
		call wjfren (i,x,il)      # get channel 2
		nch2=x
		if (nch2 < 1 || nch2 > nchans) {
			call what(i)
			write (ttyout, 310)
			go to 298
		}
		call wjfren (i,x,il)      # get channel 3
		nch3=x
		if (nch3 < 1 || nch3 > nchans) {
			call what(i)
			write (ttyout, 310)
			go to 298
		}
		call wjfren (i,x,il)      # get channel 4
		nch4=x
		if (nch4 < 1 || nch4 > nchans) {
			call what(i)
			write (ttyout, 310)
			go to 298
		}
		if (il == ihi) i = i -1  # i set for imaging mode (gotten later)
	}

	if (nch1 > nch2 || nch2+1 >= nch3 || nch3 > nch4) {
		write (ttyout,312)
312		format ('ERROR: channels are not in a valid sequence:',/,
			'       If channels = n1 n2 n3 n4, then the following',
					' must hold:',/,
			'       n1 <= n2, n2+1 < n3, and n3 <= n4',//,
			' REENTER ALL POINTS',//)
		go to 298
	}

	nch12 = nch2 - nch1 + 1
	nch34 = nch4 - nch3 + 1
	write (ttyout,*) 'NOTE: Right continuum =',nch12,' channels'
	write (ttyout,*) 'NOTE: Left  continuum =',nch34,' channels'

	iflag = 0
	call wjfren (i,x,il)  # check for imaging mode flag
	if (il == ihi && i < 80) {
		iflag = 1
		write (ttyout,*) 'IMAGING MODE SET'
	}


#     parameter description for bdm set call:
#        INPUT:
#           dataa = wavelengths
#           data  = reference library spectrum  R*4 (nch4 elements)
#           nch1   = continuum point begin on left side of band  I*4
#           nch2    = continuum point end on left side of band  I*4
#                      note: nch2 >= nch1  (checked)
#           nch3    = continuum point begin on right side of band  I*4
#                      note: nch3 > nch2 + 1 (checked)
#           nch4    = continuum point end on right side of band  I*4
#                      note: nch4 >= nch3 (checked)
#                      note: nch4 also determines the max array sizes
#        OUTPUT:
#           rflibc = reference library spectrum, continuum
#                                    removed  R*4 (nch4 elements)
#           minch  = minimum channel in the reference library spectrum.
#                    This is defined to be the band minimum.
#           maxch  = maximum channel in the reference library spectrum.
#                    This is defined to be the band maximum.
#           ifeattype = -1 is an emission feature
#                     =  1 is an absorption band
#           ier    = error detected in input:
#                       = 0 no error
#                       = 1 error
#   

	call bdmset (dataa,data,nch1,nch2,nch3,nch4,
                           rflibc,minch,maxch,ifeattype,ier)

	if (ier != 0) {
		write (ttyout, 320)
320		format ('ERROR in band mapping library setup',/,
			'Press return to start over',///)
		go to 50
	}

# now do actual fit

#     parameter description to band map call:
#        INPUT:
#           dataa    = wavelengths  R*4  (nch4 elements)
#           rflibc = reference library spectrum, continuum
#                                    removed  R*4 (nch4 elements)
#           datab  = observed spectrum  R*4 (nch4 elements)
#           nch1    = continuum point begin on left side of band  I*4
#           nch2    = continuum point end on left side of band  I*4
#                      note: nch2 >= nch1  (not checked)
#           nch3    = continuum point begin on right side of band  I*4
#                      note: nch3 > nch2 + 1 (not checked)
#           nch4    = continuum point end on right side of band  I*4
#                      note: nch4 >= nch3 (not checked)
#                      note: nch4 also determines the max array sizes
#           ictrol = error message control flag:
#                      = 0 don't print error messages, just set
#                          output to deleted values.
#                      = 1 print error messages and set output to
#                          deleted values.
#           minch  = minimum channel in the library spectrum.
#           maxch  = maximum channel in the reference library spectrum.
#                    This is defined to be the band maximum.
#           ifeattype = -1 is an emission feature
#                     =  1 is an absorption band
#           iflag  = flag to indicate if processing image-type data:
#                      = 0 not image data (single spectrum analysis)
#                      = 1 imaging spectrometer data.
#           ixpxl  = x-pixel coordinate of image data (iflag = 1)
#           iypxl  = y-pixel coordinate of image data (iflag = 1)
#   
#        OUTPUT:
#           datsc2 = continuum removed observed spectrum  R*4 (nch4 elements)
#                      NOTE: continuum is removed ONLY between nch2 and nch3
#                            To get complete continuum, you must remove
#                            it yourself using the slope and yintcp
#                            variables below.
#           xk     = k factor needed to make reference match library  R*4
#           bd     = band depth  R*4
#           rfit   = fit parameter normalized to 1 channel R*4
#           slope  = slope to continuum of observed spectrum
#           yintcp = intercept to continuum of observed spectrum
#           datac = temporary working array R*4 (nch4 elements)
#                    at a normal return, = fitted library reference
#                    spectrum
#           conref = continuum reflectance of observed spec at band minimum

	ixpxl = 0
	iypxl = 0
	ictrol = 1

	call nvres (dataa,rflibc,datab,vegspec,
			nch1,nch2,nch3,nch4,ictrol,minch,
			maxch,ifeattype,iflag, ixpxl,iypxl,
			datsc2,xk,bd,rfit,slope,yintcp,datac,
			conref,a1,b1,
			xndvivegspec,xndviobs,xfactor,bdnorm)

### nvres does a normalization, a ratio, and then calls bandmp with:
#	call bandmp (dataa,rflibc,datab,nch1,nch2,nch3,nch4,ictrol,minch,
#			maxch,ifeattype,iflag, ixpxl,iypxl,
#                        datsc2,xk,bd,rfit,slope,yintcp,datac,conref)

# delete all channels in data set except those in the band and continuum
	if (nch1 > 1) {
		do i = 1, nch1-1 {
			datac(i) = -1.23e34
		}
	}
	if (nch4 < nchans) {
		do i = nch4+1, nchans {
			datac(i) = -1.23e34
		}
	}

400	write (ttyout,410)
410	format (/,'OUTPUT Selection:',//,
		' Press return to save FITTED REFERENCE spectrum',/,
		' or type  o  to save CONTINUUM REMOVED OBSERVED ',
					'FEATURE spectrum',/,
		' ot type  n  to save the RED-EDGE NORMALIZED ',
					'spectrum')
	call crtin
	i=1
	call wjfren (i,x,il)
	if (il==ihx || il==ihe) {
		ic=il
		return
	}
	imode = 0
	if (il == iho) imode = 1
	if (il == ihn) imode = 2

	if (imode == 1 && ictrl == ihe) {    # continuum correct error bars
		do i = nch1, nch4 {
			if (datsc2(i) != delpt && datab(i) != delpt &&
					abs(datab(i)) > 0.1e-20) {
				error(i) = error(i) * datsc2(i) / datab(i)
			} else {
				error(i) = 0.0
			}
		}
	} else {
		ictrl = 0
		idad = idsav
		do i = nch1, nch4 {
			error(i) = 0.0
		}
	}



#	-------------------------------------------------
#	determine history
	
	call namdev (idv1,inam)

	if (imode == 0) {
		cmode=' fitted libry spec saved  '
		write(ihist,506) inamr,ifilb,inam,ifl1,bd
	} else if (imode == 2) {

		cmode='nrmlzd edge spectrum saved'
		write(ihist,506) inamr,ifilb,inam,ifl1,bd
		do i = nch1, nch4 {
			datac(i) = vegspec(i)
		}
	} else {
		do i = nch1, nch4 {
			datac(i) = datsc2(i)
		}
		cmode=' con remvd obs spec saved '
		write(ihist,507) inam,ifl1,inamr,ifilb,bd
	}
	write(mhist(1:74),487) inamwi,itrol(2),rfit,cmode
	write(mhist(75:148),488) xk, conref
	write(mhist(149:222),489) nch1, nch2, nch3, nch4, bdnorm
	write(mhist(223:296),508) inamveg, iflveg, a1, b1

	return
#     ---------------------------------------------------
#	program end


55      format ( ' *** error -- invalid file status ***',/,
	'     program will exit.',/)

115     format ( ' Function f37: Vegetation Red-Edge Shift (NVRES):',//,
'     This function adjusts a vegetation red-edge reflectance spectrum',/,
'          (or other "stair-step" spectrum) to match the unknown, then',/,
'          ratuis the two spectra (unknown / normalized reference) and',/,
'          fits a reference spectrum over a given wavelength', /,
'          (channel) interval.' //,
'     OPERATING ON : ',a,i4,':',a,//,
'     Enter new wavelength file id and record number, (e.g. V23),',//,
'     or press return, or "e", or "x" to exit:',/)

165     format ( ' *** error -- invalid input re-enter ***:',/)

487   format('**f37: wave set: ',a,' r',i7,
		'  fit =',f7.4,a)
488   format('**f37: lib stretch param (xk)=', f10.4,
		'    continuum (conrem)=',f8.5,'  ')
489   format('**f37: continuum channels:',i5,' ',i5,' ',i5,' ',i5,
		' Nrmlzd depth:',f8.4)

506     format ('f37:lib=',a8,' r',i7,' obs=',a8,' r',i7,' dpth=',f6.4)

507     format ('f37:',a8,' r',i7,' fit lib=',a8,' r',i7,' dpth=',f6.4)

508     format ('**f37: Edge Spectrum: ',a8,' r',i7,' a=',f10.5,' b=',f10.5,' ')

	END

	subroutine f43(ic,idv1,ifil)
	implicit integer*4 (i-n)

#ccc  name: f43
#ccc  version date: 2/12/87
#ccc  author(s): Roger N. Clark
#ccc  language: Ratfor
#ccc
#ccc  short description: FFT and inverse
#ccc
#ccc  algorithm description: see specpr manual
#ccc  system requirements: none
#ccc  subroutines called:
#ccc  argument list description: 
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced: none
#ccc  internal variables:
#ccc  file description: none
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc

	include "../common/spmaxes"   # max parameters, must be first

	include "../common/blank"
	include "../common/label1"
	include "../common/lblg"
	include "../common/lbl7"
	include "../common/lbl3"
	include "../common/lbl4"
	include "../common/lundefs"
	include "../common/alphabet"
	include "../common/dscrch"
#RED
	integer*4 iwidok     # function
	integer*4 maxfftchans   # currently set to SPMAXCHAN*2

	integer*4 fileno,outno,bfilno,efilno,devltr,outid,outchn

# the fft subroutine must pack the real array into complex format
# (real number, 0.0, real number, 0.0, etc), so the data array
# must be 2*the number of channels needed.  f43 is using datsc1,
# datsc2 and datsc3 for the working array for a total array size of
# 3*4864 = 14592.  But the number of channels then available is 
# 14592/4 = 3648.  thus the sdata array is set to 2,3648
# was:
#	dimension sdata(2,3648), zdata(14592)

	dimension sdata(2,3648), zdata(SPMAXCHAN*3)
	integer*4 devctw,fnctw
	integer*4 devbdw,fnbdw
	integer*4 tmpctw,tmpbdw
	integer*4 numctw,numbdw
	integer*4 nbndps
	integer*4 erflag
	integer*4 gmode,nmode  # I can not see that gmode is used -RNC 4/4/2002


	logical cnvlot,rtflag
	character*8 inam,inam2
	character*8 inamwi   #file name of input wavelength set
	character*8 inmirs   #file name of input resolution set (gmode 1 only)
	character*8 inmows   #file name of output wavelength set (gmode 1 only)
	character*8 inmors   #file name of output resolution set (gmode 1 only)
	character*2 cmode

	character*11 nmstr

	equivalence (sdata,datab(1)), (zdata(1),datsc1(1))


#*********************************************************************

	maxfftchans=3648

	if (ictrl==-1) {
		ic=ihe
		write(ttyout,45)
		go to 1000
	}
	gnode =0 # I can not see that gmode is used -RNC 4/4/2002

	if (ictrl==ihe) {
		write(ttyout,55)
	}

100     call eralph
	call whedr2
101	write(ttyout,115) idv1,ifil,ititl
	call crtin
	i=1
	call wjfren (i,a,il)
	if (il == ihe || il == ihx) {
		ic = ihe
		goto 1000
	}
	if (iwidok(il) == 1) {
		iwtmpf = il
		call wjfren (i,b,ik)
		if ((b>0)&(i<80)&(((iwtmpf==ihcc)&(b<=maxchn))|
				((iwtmpf!=ihcc)&(b<=maxrec)))) {
			ifilno=b
		} else {
			call what(i)
			write(ttyout,125)
			go to 101
		}
	} 
#
#
# copy dataa to zdata because when read in wavelengths, they are
# put in dataa.

	do i = 1, maxchn {
		zdata(i) = dataa(i)
	}

	if (i>=80) {
		iwtmpf=itrol(1)
		ifilno=itrol(2)
	}

	call wavlng(iwtmpf,ifilno,ier)
	if (ier!=0) go to 101
	itrol(1) = iwtmpf
	itrol(2) = ifilno

	call whedr2
	if (nchans < 3) {
		write (ttyout,450)
450		format ('Number of channels too small',/,
			'Press return to exit')
		call crtin
		ic = ihx
		go to 1000
	}
#
#     ********************************************
#     * get second file if requested.  second is imaginary part
#     ********************************************

65      write(ttyout,70)
70      format (' Enter file id and record number for IMAGINARY COMPONENT,',
	/,'   or press return to accept zeros as the imaginary component',/,
		' or e or x to exit.'/)
	iform = 0   #set if real component only
	call crtin
	i = 1
	call wjfren(i,x1,idevb)
	if (i >= 79 && idevb == 0) go to 600   # zeros for imaginary part
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
66		format ('invalid input, reenter')
		go to 65

#       *** looks ok so get file b ***
	} else {
		ifilb = xfilb
		call devok (4,idevb,ifilb,lun,ier)
		if (ier!=0) {
			go to 65
		}
		itmp = ifilb
		call redfil(itmp,lun,ier)
		if (ier != 0) {
			go to 65
		}
		iform = 1
		write (ttyout,68) idevb, ifilb, ititl
68		format (' Imaginary component: ',a,i7,5x,a,//)
	}


600	write(ttyout,601)
601	format('Enter  f       for FFT computation',/,
	       '       i       for Inverse FFT computation')

	nmode=-1
610 	call crtin
	i=1
615	call wjfren(i,a,il)
	if (il==ihe | il==ihx) {
		ic=ihe
		goto 1000
	}
		
	if (il==ihi) {
		nmode=1 
	} else if (il == ihf) {
		nmode=-1
	} else {
		call what(i)
		go to 610
	}

# determine which data to save:

616	write(ttyout,617)
617	format('Enter  r  to save real component',/,
	       '       i  to save imaginary component')

	call crtin
	i=1
	call wjfren(i,a,il)
	if (il==ihe | il==ihx) {
		ic=ihe
		goto 1000
	}
		
	if (il==ihi) {
		icmplx = 1
		cmode = 'im'
		write (ttyout,"('SAVING IMAGINARY COMPONENT',/)")
	} else if ( il == ihr) {
		icmplx = 0
		cmode = 'rl'
		write (ttyout,"('SAVING REAL COMPONENT',/)")
	} else {
		call what(i)
		go to 616
	}


	if (nchans > maxfftchans) {
		write (ttyout, 619) maxfftchans
619		format ('FFT and Inverse is limited to less ',
                        'than or equal to ',f7.0,' channels',/,
                        'at this time.',//,
                        '  Press return to go to beginning.',/)
			call crtin
		go to 100
	}

# check for equal data spacing and no deleted points

	do i = 1, nchans {
		if ((zdata(i) == -1.23e+34) || (data(i) == -1.23e+34)) {
			write (ttyout,625) i
625			format ('ERROR: deleted channel detected in',
				'data array at channel',i7,/,
				'Press return to exit')
			call crtin
			ic=ihx
			goto 1000
		}
		if (dataa(i) == -1.23e+34) {
			write (ttyout,626) i
626			format ('ERROR: deleted channel detected in',
				'wavelength array at channel',i7,/,
				'Press return to exit')
			call crtin
			ic=ihx
			goto 1000
		}
	}
	deltat = (dataa(nchans)-dataa(1))/float(nchans-1)

	if (abs(deltat) < 0.1e-20) {
		write (ttyout,627) deltat
627		format ('ERROR: channel spacing too small:',
			1pe13.6,/,'Press return to exit')
		call crtin
		ic= ihx
		goto 1000
	}

	do i = 1, nchans-1 {
		xspace = dataa(i+1) - dataa(i)
		if (abs(1.0-(xspace/deltat)) > 0.01 ) {
			write (ttyout, 628) deltat, i, xspace
628			format ('ERROR: wavelengths not evenly spaced',
                        /,'average channel spacing =', 1pe13.6,/,
			/,'spacing at channel',i7,' =',1pe13.6,
			'  Percent difference > 0.01',
			//,'Press return to exit')

			call crtin
			ic = ihx
			go to 1000
		}
	}


# format data into complex format:

	ix = 1
	do i = 1, nchans {
		sdata(1,i) = zdata(i)
		if (idevb == 0) {
			sdata(2,i) = 0.0
		} else {
			sdata(2,i) = data(i)
		}
		ix = ix+2
	}

	if (nmode==-1)   {      # forward FFT transform

#
#  	      ********* nmode =-1: FFT **********
#
		call fft  (sdata, nchans, nmode, iform, zdata)
		ix = 1
		do i = 1, nchans {
			if (icmplx == 1 ) { # save imaginary component
				datac(i) = sdata(2,i)
				ix = ix +2
			} else {             # save real component
				datac(i) = sdata(1,i)
				ix = ix+2
			}
		}

#
#	      *********  nmode =1: Inverse FFT *********

	}else {				# Inverse FFT

		call fft (sdata, nchans, nmode, iform, zdata)
		xchans = float(nchans)
		ix = 1
		do i = 1, nchans {
			if (icmplx == 1 ) { # save complex component
				datac(i) = sdata(2,i)/xchans
				ix = ix +2
			} else {             # save real component
				datac(i) = sdata(1,i)/xchans
				ix = ix+2
			}
		}

	}			# end else nmode != 1		

#
#       *** end of computation loop ***
#

#
#     *** check for center value output ***
#
390 write(ttyout,395)
	cnvlot = .false.
	call crtin
	i=1
	call wjfren (i,a,id)
	if (i>=80) go to 490
	cnvlot = .true.
	if (id==0) {
		call what (i)
		write(ttyout,125)
		go to 390
	} else {
		outid=id
		call wjfren (i,b,il)
		if (b==0) {
			write(ttyout,125)
			go to 390
		} else outno=b
	}
401 write(ttyout,405)
#
	call crtin
	ititl = iopcon(1:40)
#
#     *** write center values if applicable ***
#
	if (cnvlot) {
#
# center values are divided by number of dimensions, but for this
# case, we have only one dimension, so:
#
		xtmp = 2.0*3.14159265/deltat
		do i=1, nchans
			data(i)=xtmp*float(i-1)
	}
	if (nchans < maxchn) {
		do i=nchans+1, maxchn
			data(i)=-1.23e+34
	}
#
#     *** write history to center values***
#
	ihist = ' f43: center values to following data set'
	call namdev(idv1,inam)
	call namdev(idevb,inam2)
	call namdwv(itrol(1),inamwi)  #name of input wavelength set
#
#
	call devlun (4,outid,lun)
	call devsta (lun,ista,ier,iprt)
	if (ier!=0 || iprt==-1) {
		write(ttyout,155)
		go to 390
	}
	mhist(1:296) = ' '
#
#       wavelength set stuff added 4/4/2002 - RNC
#
	devctw = outid
	fnctw  = outno

#
	call wrifil (outno,lun,ier)
#
490	if (nmode == 1) {
		nmstr='Inverse FFT'
	} else {
		nmstr='Forward FFT'
	}

	call namdev(idv1,inam)
	call namdev(idevb,inam2)
	call namdwv(itrol(1),inamwi)  #name of input wavelength set

	if (idevb == 0) {    #real transform only
		write (ihist,495) nmstr, cmode,inam,ifil,inamwi, itrol(2)
		mhist(1:296) = ' '
	} else {
		write (ihist,496) nmstr, inam,ifil,inam2,ifilb
		write(mhist(1:74),487) inamwi, itrol(2), nmstr
#
		mhist (75:296) = ' '
	}

	if (nmode == 1) {        # set new wavelength set
		itrol(1)=devctw
		itrol(2)=fnctw
	}

#
#     *** program end ***
#
1000 return

45      format(' *** error -- no file input to program ***.',/,
	'      program will exit.',/)

55      format(/,
	' *** note: f43 cannot include errors. They will be ignored.',/)

105	format ('ERROR: press return to exit')

115     format(' Function f43: FFT and Inverse FFT',//,
	5x,'This function computes a Fast Fourier Transform or its Inverse',/,
	5x,'assuming equally spaced points but the number of data channels does',/,
	5x,'not have to be a power of two.  If the number of channels is a',/,
	5x,'power of two, the computation goes faster.',/,
	5x,'There can be NO DELETED points in the wavelength set.',/,
	/,5x,'Operating on: ',a,i4,':',a,//,
	5x,'Press RETURN for default wavelength assignment or ',/,
	5x,'the new wavelength file id and number:',/)

125     format (' ***--error--invalid input to program.  re-enter: ',/)

155     format (' ***--error-- invalid file protection ***',/)

205     format (//,
	' Enter file id,  first record number and number of bandpasses',/,
	'     for spectral bandpass data sets :',/)

395     format (' ',
	'The center wavelength values for the transform have been computed for',/,
	'      the output spectrum.  If you want these values written:',/,
	' Enter file id and file no for center value output :',/,
	' RETURN To Ignore',/)

405      format (' enter title for center values :',/,
	' --------------------------------------i',/)

485   format (1x,a)

487   format('**f43: input wavelength set: ',a,' r',i6,2x,a)

495   format ('f43:',a,': ',a,a,' r',i6,'; Wave: ', a,' r',i6)

496   format ('f43:',a,': real:',a,' r',i6,'; im: ', a,' r',i6)

505   format (a)
10000 format(a)

end

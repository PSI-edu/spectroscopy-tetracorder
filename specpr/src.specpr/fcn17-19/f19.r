	subroutine f19(ic)
	implicit integer*4 (i-n)

#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc         This is special function f19 polynomial fit.
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc       seterr,rstart,devok,redfil,hreset,whedr2,crtin
#ccc       wjfren,wavlng,redhed,polfit,namdev,iwidok.
#ccc  argument list description:
#ccc     arguments: none
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc
################################################################

	include "../common/spmaxes"   # max parameters, must be first

	include "../common/blank"
	include "../common/label1"
	include "../common/lblg"
	include "../common/lbl6"
	include "../common/lbl7"
	include "../common/lbl3"
	include "../common/lundefs"
	include "../common/alphabet"

#RED
	integer*4 iwidok    # function
	character*8 inam, inmwav
	real*4 coeff (20)
	real*8 array(10,10)

	logical cof
	equivalence (array,datsc2)


#********************************************************************
# purpose:                                                          *
#     make a least-squares fit to data with a polynomial curve      *
#     y=a(1) + a(2)*x + a(3)*x**2 + a(4)*x**3 + . . .               *
#                                                                   *
#                                                                   *
# usage:                                                            *
#     call polfit (x, y, sigmay, npts, nterms, mode, a, chisqr,array)*
#                                                                   *
# description of parameters:                                        *
#  dataa-array of data points for independent variable              *
#  datab-array of data points for dependent varaible                *
# error -array of standard deviation for y points                   *
# nchans-number of pairs of data points                             *
# nterms-number of coefficients (degree of polynomial +1 )          *
#  mode -determines method of weighting least squares fit :         *
#        mode=1 (    errors )                                       *
#        mode=0 ( no errors )                                       *
#  coeff-array of coefficients of polynomial                        *
# chisqr-reduced chi square for fit                                 *
#                                                                   *
# subroutines and function subprograms required:                    *
#     determ (array,norder)                                         *
#      evaluates the determinant of a symmetric two-dimensional     *
#      matrix of order norder                                       *
#                                                                   *
# comments:                                                         *
#     dimension statement valid for nterms up to 20                 *
#                                                                   *
#********************************************************************
# fix notes:
# 2/18/87: determ was modified to handle any order array, so the
#          array must be declared here and passed to polfit which
#          passes it to determ.   Roger N. Clark
#


	mode=0
	if (ictrl==-1) {
		ic = ihx
		write(ttyout,10)
		call crtin
		goto 700
	} else if (ictrl==ihe) {
		mode=1
		ifile=ifl1
		call devok (4,idv1,ifile,lun,ier)
		call rederr (ifile,lun,ier)
		if (ier!=0) {
			ic=ihx
			write(ttyout,55)
			call crtin
			go to 700
		}
	}

	do i=1,maxchn {
		datab (i) = dataa (i)
		datac (i) = 0.
	}

	call eralph
	call whedr2

#     *** read in x (wavelength) values ***

	write(ttyout,105) idv1,ifl1,ititl
110	call crtin
	i=1
	call wjfren (i,c,il)
	if (il==ihx || il==ihe) {
		ic=ihe
		go to 700
	}

	if (i>=80) {
		iwtmpf=itrol(1)
		ifilno=itrol(2)
	} else {
		if (iwidok(il) == 1) {
			iwtmpf = il
			call wjfren (i,c,il)
			if ((i >= 80) | (c > maxrec) | (il != 0)) {
				write (ttyout,115)
				go to 110
			}
			ifilno = c
		} else {
			write(ttyout,115)
			go to 110
		}
	}

	call wavlng (iwtmpf,ifilno,ier)
	if (ier != 0) {
		write (ttyout, 115)
		go to 110
	}

	ndel = 0
	do i=1,nchans
		if (dataa(i)==-1.23e34 || datab(i)==-1.23e34)
			ndel = ndel + 1

	write(ttyout,125)
130	call crtin
	i=1
	call wjfren (i,b,il)
	if (il==ihx || il==ihe) {
		ic=ihe
		go to 700
	}

	if (i>=80) go to 130
	if (b>=0 && b<=10)
		nterms=b
	else {
		write(ttyout,135)
		go to 130
	}

	write(ttyout,145) nterms

	if (nchans-ndel<nterms) {
		write(ttyout,155) nchans
		go to 130
	}

	write(ttyout,165)
140	call crtin
	i = 1
	call wjfren(i,b,il)
	if (il==ihx || il==ihe) {
		ic = ihe
		goto 700
	}
	if (i>=80) cof = .false.
	else if (il!=ihc) {
			write(ttyout,175)
			goto 140
		} else {
			cof = .true.
		}

	call polfit (dataa,datab,error,nchans,nterms,mode,coeff,chisqr,array)

	if ( nterms < 10) {            # set other terms (unused) to zero
		do j = nterms+1, 10 {
			coeff(j) = 0.0
		}
	}

#
#     *** recalculate data based on polynomial fit ***
#     *** using y=a(1)+a(2)*x+a(3)*x**2+a(4)*x**3+... ***
#
#
#	if (!cof) {
# RED
#	if (cof != .true.) {
	if (cof .neqv. .true.) {
		do j=1,nchans {
			sum=0.
			if (dataa(j)!=-1.23e34) {
				do i=1,nterms {
					k=i-1
					if (i>1) sum=sum+coeff(i)*dataa(j)**k
					else     sum = coeff (i)
				}
				datac (j) = sum
			} else datac(j) = -1.23e34
		}
		call namdev (idv1,inam)
		write(ihist,600) inam,ifl1,nterms
	} else {
		do i=1,nterms
			datac(i) = coeff(i)
		call namdev(idv1,inam)
		write(ihist,690) inam, ifl1, nterms
	}

# make sure coeffs beyond nterms are zero.

	if (nterms < 10) {
		do i = nterms+1,10 {
			coeff(i) = 0.0
		}
	}
#
#     *** write history ***
#
	call namdwv(itrol(1),inmwav)

	write (mhist(1:74),650) (coeff(j), j=1,3)
	write (mhist(75:148),660) (coeff(j), j=4,8)
	write (mhist(149:222),670) (coeff(j), j=9,10), chisqr
	write (mhist(223:296),680) inmwav, itrol(2), nchans

	write(ttyout,685) mhist(1:74),mhist(75:148),mhist(149:222)

700	return

10	format (' *** ERROR *** : NO FILE input. f19 will hard exit.',/)
55	format (' *** ERROR *** : INVALID FILE STATUS for ', /,
		'     reading of errors.  program will exit.',/)
105	format (' This is Special Function f19 (polyfit).'/,
' fitting ',a,i5,5x,a,/,
'     This routine makes a least squares fit ',
				'to data with a poly-',/,
'     nomial curve.',/,
'Enter new WAVELENGTH FILE ID (upper case) ',
			'and RECORD NUMBER or',/,
'RETURN for DEFAULT wavelength assignment:',/)
115	format (' *** ERROR *** - INVALID RECORD NUMBER - re-enter:',/)
125	format (' Now enter NUMBER OF TERMS for polynomial fit -',/,
		'     (nterms from one to ten):',/)
135	format (' *** ERROR *** - INVALID NUMBER OF TERMS -re-enter:',/)
145	format (' Number of terms is ',i4,/)
155	format (' *** ERROR *** : Number of CHANNELS (',i4,' ) is ',/,
		'LESS THAN the number of TERMS for fit.  re-enter the number ',
		' of terms:',/)
165	format (' Type  c  to ONLY calculate COEFFICIENTS,'/,
		' <return> to Calculate the POLYNOMIAL.',/)
175	format (' *** ERROR *** - INVALID INPUT -re-enter:',/)
600	format ('f19: polynomial fit to ',a8,',f',i5,';',i4,' terms.')
650	format ('** polynomial coefficients:   1=',1pe11.4,'  2=',
		e11.4,'  3=',e11.4)
660	format ('4=',1pe11.4,'  5=',e11.4,'  6=',e11.4,'  7=',e11.4,
		'  8=',e11.4)
670	format ('9=',1pe11.4,'  10=',e11.4,12x,'chisqr=',e11.4)
680	format (' wave set= ',a,' rec ',i5, ' channels=',i5, '     ')
685	format (3(a,/))
690	format ('f19: coeff. of polynomial fit to ',a,
		',f',i5,';',i3,' terms.')
	end

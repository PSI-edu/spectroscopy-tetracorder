	subroutine f49wav(wmin1,wmax1,wmin2,wmax2,ipts,ic,
		idw1,ifw1,idw2,ifw2,idlcon)
	implicit integer*4 (i-n)

#ccc  version date: 02/21/88 (modified from f12wav.r)
#ccc  author(s): Roger Clark,Jeff Hoover and Barry J. Middlebrook
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc       This subroutine gets wavelength file associated
#ccc       with the data file user entered and wavelength
#ccc       file for interpolation and put first into
#ccc       < datab > and second into < dataa >.(for subr f49).
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc                    crtin,wjfren,wavlng,iwidok
#ccc  argument list description:
#ccc     arguments: wmin1,wmax1,wmin2,wmax2,ipts,ic
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc

#     **************************************************************
#     *
#     * get wavelength file associated with data file user entered
#     * and wavelength file for interpolation and put first
#     * into <datab> and second into <data>. (for subr f49)
#     *
#     **************************************************************

	include "../common/spmaxes"   # max parameters, must be first

	include "../common/blank"
	include "../common/lbl7"
	include "../common/lbl4"
	include "../common/label1"
	include "../common/lbl3"
	include "../common/lblg"
	include "../common/interp"
	include "../common/lundefs"
	include "../common/alphabet"
	include "../common/dscrch"
#RED
	integer*4 iwidok    # function

	integer*4 isort(SPMAXCHAN)
	logical errs
	character*74 idlcon
	integer*4 idlt(SPMAXCHAN)
	equivalence (isort(1),datsc4(1))
	equivalence (idlt,datsc3)
	ipts=0

#timj
    for(i=1;i<=maxchn;i=i+1) 
		idlt(i) = 0 

2	errs = .true.
	while  (errs)  {
		write(ttyout,10)
		call crtin
		i = 1
		call wjfren(i,x,iwfl1)
		call wjfren(i,wavfl1,ic1)
		if (ic1 != 0) i = i-1
		call wjfren(i,x,iwfl2)
		call wjfren(i,wavfl2,ic2)

#        *** check for soft or hard exit ***
		if (ic1==ihe || ic2==ihe || iwfl1==ihe ||
						iwfl2==ihe) {
			ic = ihe
			return
		} else if (ic1==ihx || ic2==ihx || iwfl1==ihx ||
						iwfl2==ihx) {
			ic = ihx
			return

#        *** check if have a valid wavelength file id and number ***
		} else if ( (iwidok(iwfl1) == 0) |
				(iwidok(iwfl2) == 0) |
				(wavfl1 < 1) | (wavfl2 < 1) |
				(wavfl1 > maxrec) | (wavfl2 > maxrec) |
				((iwfl1 == ihcc) & (wavfl1 > maxchn)) |
				((iwfl2 == ihcc) & (wavfl2 > maxchn))) {
			write(ttyout,40)
		} else {
			errs = .false.
		}
	}

	iwav1 = wavfl1
	iwav2 = wavfl2
	idw1=iwfl1
	idw2=iwfl2
	ifw1=wavfl1
	ifw2=wavfl2

#     *** read original wavelengths into <datab>, ***
#     *** and copy associated data values to <error>       ***
	il = 1
	call wavlng(iwfl1,iwav1,ier)
	if (ier != 0) go to 2
	nch1 = nchans
	nch = nch1

#     *** ask if want to delete any channels ***
	write(ttyout,50)
	call crtin
	i = 1
	call wjfren(i,x,ic)
	if (ic==ihe || ic==ihx) return
#
# store first line of deleted points in idlcon
#
	idlcon = iopcon(1:74)
#
	if (i < 80) {
		i = 1
		call dltpts(i,ipts,idlt,nchans,ic)
		if (ic==ihe || ic==ihx) return
	}
	write(ttyout,60)
	if (ipts != 0) {
		do  k = 1,ipts {
			dataa(idlt(k)) = -1.23e34
		}
		nch = nch1 - ipts
		il = ipts + 1
	}
	do i=1,nch1 {
		if (datac(i)==-1.23e34 & dataa(i)!=-1.23e34) {
			dataa(i)=-1.23e34
			nch=nch-1
			il=il+1
		}
	}
	do  i = 1,nch1 {
		datab(i) = dataa(i)
		error(i) = datac(i)
	}
	wmin1 = datab(il)
	wmax1 = datab(nch1)

	write (6,*)wmin1,wmax1
#     *** copy errors into <datac> ***
	if (ictrl == ihe) {
		do  i = 1,nch1 {
			datac(i) = data(i)
		}
	}

#     *** read interpolated wavelengths into <data> ***
	call wavlng (iwfl2,iwav2,ier)
	if (ier != 0) go to 2
	nch2 = nchans
	do  i = 1,nch2 {
		data(i) = dataa(i)
	}

	wmin2 = data(il)
	wmax2 = data(nch2)
	write(6,*)wmin2,wmax2

#     *** note which channels in interpolated wavelength file has ***
#     *** values outside bounds of other wavelength file          ***
	ilow = 0
	while (data(ilow+1) < datab(il))  {
		ilow = ilow + 1
		data(ilow) = datab(il)
	}
	ihigh = nch2 + 1
	while  (data(ihigh-1) > datab(nch1) && ((ihigh-1) > 0))  {
		ihigh = ihigh - 1
		data(ihigh) = datab(nch1)
	}
	return
10      format (' Input:',
' Wavelength File ID and record # associated ',
			'with the INPUT DATA SET, and the',/,
' Wavelength File ID and record # for the ',
			'OUTPUT DATA SET.',/,
'     File ID''s must be UPPER CASE LETTERS',/,
'     Separate two inputs with one or more spaces. ',
			' (e.g. V23 V46)',/,
' Type  e  or  x  to EXIT.',/)

40      format (' Found ILLEGAL RECORD NUMBER OR CHARACTER. Reenter line.'/)
50      format (//,' Enter channels to be DELETED.  To delete',
		' consecutive channels',/,' type lower bound t',
		' upper bound (e.g. 1 t 10).',/,'  If none, press',
		' return.  Enter  e  or  x  to EXIT.'/)
60      format (' *** WORKING ***'/)
      end

	subroutine f12wav(wmin1,wmax1,wmin2,wmax2,ipts,ic,
		idw1,ifw1,idw2,ifw2,idlcon)
	implicit integer*4 (i-n)

#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc       This subroutine gets wavelength file associated
#ccc       with the data file user entered and wavelength
#ccc       file for interpolation. sort and put first into
#ccc       < datab > and second into < dataa >.(for subr f12).
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc                    crtin,wjfren,wavlng,bubble,iwidok
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
#     * and wavelength file for interpolation. sort and put first
#     * into <datab> and second into <data>. (for subr f12)
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
	include "../common/lblwav"

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

#     *** read in original wavelengths, sort into <datab>, ***
#     *** and copy associated data values to <error>       ***
	il = 1
	call wavlng(iwfl1,iwav1,ier)
	if (ier != 0) go to 2
	nch1 = nchans
	write (ttyout, 20) iwtitl, nch1
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
	call bubble(dataa,isort,nch1)
	do  i = 1,nch1 {
		datab(i) = dataa(isort(i))
		error(i) = datac(isort(i))
	}
	wmin1 = datab(il)
	wmax1 = datab(nch1)

#     *** copy errors into <datac> using sorted pointers ***
	if (ictrl == ihe) {
		do  i = 1,nch1 {
			datac(i) = data(isort(i))
		}
	}

#     *** read in interpolated wavelengths and sort into <data> ***
	call wavlng (iwfl2,iwav2,ier)
	if (ier != 0) go to 2
	nch2 = nchans
	write (ttyout, 21) iwtitl, nch2
	call bubble(dataa,isort,nch2)
	do  i = 1,nch2 {
		data(i) = dataa(isort(i))
	}

	wmin2 = data(il)
	wmax2 = data(nch2)

#     *** note which channels in interpolated wavelength file has ***
#     *** values outside bounds of other wavelength file          ***
	ilow = 0
	while (data(ilow+1) < datab(il) && ilow < nch2)  {
		ilow = ilow + 1
		data(ilow) = datab(il)
	}
	ihigh = nch2 + 1
	while  (data(ihigh-1) > datab(nch1) && ((ihigh-1) > 0))  {
		ihigh = ihigh - 1
		data(ihigh) = datab(nch1)
	}
	return
10      format (' Input:',/,
' Wavelength File ID and record # of ',
			'the INPUT DATA SET, and the',/,
' Wavelength File ID and record # for the ',
			'OUTPUT DATA SET.',/,
'     File ID''s must be UPPER CASE LETTERS',/,
'     Separate two inputs with one or more spaces. ',
			' (e.g. V23 V46)',/,
' Type  e  or  x  to EXIT.',/)

20	format (' INPUT  wavelength set: ',a, '  Channels=',i5, /)

21	format (' OUTPUT wavelength set: ',a, '  Channels=',i5, /)

40      format (' Found ILLEGAL RECORD NUMBER OR CHARACTER. Reenter line.',/)
50      format (/,' Enter channels to be DELETED.  To delete',
		' consecutive channels',/,' type lower bound t',
		' upper bound (e.g. 1 t 10).',/,
		' To keep all by listed channels, type  k  and the channels',/,
		'  If none, press',
		' return.  Enter  e  or  x  to EXIT.',/)
60      format (' *** WORKING ***',/)
      end

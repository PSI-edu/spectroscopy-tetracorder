	subroutine f12(ic)
	implicit integer*4 (i-n)

#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc                   This subroutine interpolates values to a new
#ccc                   wavelength set.
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc                    rstart,er,crtin,whedr2,wjfren,filinp,f12wav,
#ccc                    f12itp,namdev
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
#     *******************************************************
#     *
#     * routine interpolates values to a new wavelength set
#     *
#     *******************************************************

	include "../common/spmaxes"   # max parameters, must be first

	include "../common/blank"
	include "../common/lbl7"
	include "../common/lbl6"
	include "../common/lbl4"
	include "../common/label1"
	include "../common/lbl3"
	include "../common/lblg"
	include "../common/interp"
	include "../common/lundefs"
	include "../common/alphabet"
	include "../common/dscrch"

	logical errs
	character*8 iname,inamw1,inamw2
	character*74 idlcon
	integer*4 isort(SPMAXCHAN)

	equivalence (isort(1),datsc4(1))
	call eralph
	write(ttyout,10)
	if (ictrl == -1) {
		write(ttyout,20)
		call crtin
		ic = ihx
		return
	}

#     *** print out id, number and title ***
	write(ttyout,50) idv1,ifl1,ititl
	idev= idv1
	ifile= ifl1
	call whedr2

#     *** see if user wants to continue ***
	ideriv = 0
	write(ttyout,60)
	call crtin
	i = 1
	call wjfren(i,x,ic)
	if ((ic == ihe) | (ic == ihx)) return
	if (ic == ihd)
		ideriv = ihd
	do  i = 1,maxchn
		datac(i) = dataa(i)

#     *** read in errors into <dataa> ***
	if (ictrl == ihe) {
		call devlun (4,idv1,lun)
		itmp = ifl1
		call redfil(itmp,lun,ier) # position to error record
		ierfil = itmp + 1
		call filinp(idv1,ierfil,errs,ic)

#        *** check if had i/o error ***
		if ((errs) | (ic == ihx)) {
			write(ttyout,70)
			call crtin
			return
		}
	}


#     ************************************
#     * get wavelengths and interpolate
#     ************************************
	call f12wav(wmin1,wmax1,wmin2,wmax2,ipts,ic,
			idw1,ifw1,idw2,ifw2,idlcon)
	if ((ic == ihe) | (ic == ihx)) return

#     ***************
#     * interpolate
#     ***************
	call f12itp(ic)
	if (ic == ihx) return
	do  i = 1,nch1
		error(i) = datac(i)
	do  i = 1,nch2
		datac(isort(i)) = dataa(i)

#     *** interpolate errors if any ***
	if (ictrl == ihe) {
		call f12itp(ic)
		if (ic == ihx) return
		do  i = 1,nch2 {
#           *** errors should be positive except if equal -1.23e34 ***
			if ((dataa(i) != -1.23e34) & (dataa(i) < 0.0)) {
				error(isort(i)) = abs(dataa(i))
			} else {
				error(isort(i)) = dataa(i)
			}
		}
	}

#     *** write history ***
	mhist = ' '
	call namdev(idev,iname)
	call namdwv(idw1,inamw1)
	call namdwv(idw2,inamw2)
	if (ideriv == ihd) {
		write(ihist,95) iname,ifile
		write(mhist(1:74),115) wmin1,wmax1,wmin2,wmax2
		write(mhist(75:148),135) inamw1,ifw1,inamw2,ifw2
	} else {
#timj
		write(ttyout,90)iname,ifile
		write(ihist,90) iname,ifile
		write(mhist(1:74),110) wmin1,wmax1,wmin2,wmax2
		write(mhist(75:148),135) inamw1,ifw1,inamw2,ifw2
	}
	if (ipts != 0) {
		mhist(149:222)=' deleted channel(s). first 74 '
		mhist(178:222)='characters of deleted channel input:'
		mhist(223:296)=idlcon
	} else {
		mhist(149:222) = ' no channels deleted'
	}
	return

10      format(' Special Function f12:',/,
' This routine does a CUBIC SPLINE',
		' interpolation to a new wavelength set',/,
' or the DERIVITIVE of a data set.',//,
' Errors, if included, are also interpolated.',/,
' NOTE: remove glitches in the data',
	       ' as they could lead to erroneous results.'/)
20      format('USAGE: file ID record no. f12 (e.g. v3f12).',/,
               ' Press <return> to hard EXIT.'/)
50      format(' interpolation applied to ',a,i6,':',/,1x,a/)
60      format(' Press return for CUBIC SPLINE,',/,
		'Type:    d   for DERIVITIVE,',/,
		'      e or x to EXIT'/)
70      format(' ERROR in reading errors file. press',/,
	       ' return to hard EXIT.',/)
90      format('f12:interpolate using ',a,' rec',i6,
	       ' + waves, see manhst')
95      format('f12: derivitive using ',a,' rec',i6,
	       ' + waves, see manhst')
110     format(' wav min,max: data=',1pe10.3,',',1pe10.3,
	       ' intepolate=',1pe10.3,',',1pe10.3)
115     format(' wav min,max: data=',1pe10.3,',',1pe10.3,
	       ' derivitive=',1pe10.3,',',1pe10.3)
135	format(' input waves: ',a, ' rec', i6, 
               ',  output waves: ',a,' rec',i6)
      end

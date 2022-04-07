	subroutine f12itp(ic)
	implicit integer*4 (i-n)

#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc                   This subroutine calls necessary routines to
#ccc                   interpolate for f12.
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc                    icsicu,crtin,icsevu
#ccc  argument list description:
#ccc     arguments: ic
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc
#     *************************************************************
#     *
#     * routine to call necessary routine to interpolate for f12
#     *
#     *************************************************************

	include "../common/spmaxes"   # max parameters, must be first

	include "../common/blank"
	include "../common/label1"
	include "../common/lbl3"
	include "../common/interp"
	include "../common/lundefs"
	include "../common/alphabet"
	include "../common/dscrch"
	
	integer*4 isort(SPMAXCHAN)

	real*4 bpar(4),scoeff(SPMAXCHAN,3)
	equivalence (isort(1),datsc4(1))
	equivalence (scoeff,datsc1)


#     ***************************************************
#     * call subroutines to interpolate
#     *
#     * array status:
#     *   dataa - (interpolated result)
#     *   datab - sorted wavelengths
#     *   datac - not used
#     *   data  - sorted interpolated wavelength
#     *   error - sorted data values
#     ***************************************************

	do  i = 1,4
		bpar(i) = 0.0


#     *** get spline coefficients ***
	ier=0
	call icsicu(datab(il),error(il),nch,bpar,scoeff,nch,ier)
#     *** check return error flag ***
	if (ier==131 || ier==129 || ier==130) {
		write(ttyout,10) ier
		call crtin
		ic = ihx
		return
	}

	indx2 = 1
	nch0 = nch2
	if (ilow != 0) {
		indx2 = ilow + 1
		nch0 = ihigh - ilow - 1
	}
#     *** use coefficients got to interpolate ***
	ier=0
35      call icsevu(datab(il),error(il),nch,scoeff,nch,
		data(indx2),dataa(indx2),nch0,ier,ideriv)
#     *** check return error flag ***
	if (ier==33 || ier==34) {
		write(ttyout,10)
		call crtin
		ic = ihx
		return
	}

#     *** delete unnecessary points in interpolated data ***
	ilow1 = ilow
	ihigh1 = ihigh
	while  (ilow1 > 0)  {
		if (ideriv!=ihd) dataa(ilow1) = error(il)
		else dataa(ilow1) = 0.0
		ilow1 = ilow1 - 1
	}

	while  (ihigh1 < (nch2 + 1))  {
		if (ideriv!=ihd) dataa(ihigh1) = error(nch1)
		else dataa(ihigh1) = 0.0
		ihigh1 = ihigh1 + 1
	}
	return
10      format(' Error: Most probably the wavelength file used',/,
               'is not correct.  If you don''t think this is the',/,
			   'case, send mail to specpr and note error',
			   'number ',i3,//)
#
#20      format(' f12 error #',i3,'. send mail to specpr',
#	       '/'note this number  ',
#	       ' press return to hard exit.'/)
	end

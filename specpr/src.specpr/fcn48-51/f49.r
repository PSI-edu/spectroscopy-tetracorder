	subroutine f49(ic)
	implicit integer*4 (i-n)

#ccc  version date: 02/19/88
#ccc  author(s): Roger Clark, Jeff Hoover and Barry Middlebrook
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc                   This subroutine interpolates (linearly) values to a new
#ccc                   wavelength set.
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc                    rstart,er,crtin,whedr2,wjfren,filinp,f49wav,
#ccc                    namdev
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
#     3/15/88
#            This routine is a modified f12 routine.  Most of 
#            program was completed when I started and has some
#            characteristics which I find somewhat unusual.  As
#            an example, the f49wav routine puts the errors into
#            a temporary array called dataa while the spectra 
#            goes into an array called error.  I removed the
#            sorting call for my own purposes and so all data
#            entered must be wave sequential in order to use the
#            program correctly.  Testing has revealed that the 
#            program works properly at the end cases but that the 
#            effect on the input spectra varies with the distance
#            from the points used to calculate the linear equation.
#                                            - Barry J. Middlebrook

#     *******************************************************
#     *
#     * routine interpolates values to a new wavelength set
#     *
#     *******************************************************

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

	logical errs,limflg
	character*8 iname,inamw1,inamw2
	character*74 idlcon
	integer*4 isort(4864)
	real*4 me,mr,be,br
 	equivalence (isort(1),datsc4(1))

#RED Initialize to 0
	me=0
	mr=0
	be=0
 
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
	i=1

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
	call f49wav(wmin1,wmax1,wmin2,wmax2,ipts,ic,
			idw1,ifw1,idw2,ifw2,idlcon)
	if ((ic == ihe) | (ic == ihx)) return

#     ***************
#     * interpolate
#     ***************
	if (ic == ihx) return
	
#  The following modification to the original f12 subroutine is based on
#  the assumption that:
#                      reflectance data is in <error> array (strange but true)
#	               error data is really in <dataa> array
#                      input wave set is in <datab> array
#                      output wave set is in <datac> array

#  Set initialize loop subscripts and set wavelength range to operate on
	i=1
	j=1
	tmin=MAX(wmin1,wmin2)
	tmax=MIN(wmax1,wmax2)
	limflg=.true.

#  Loop while the wavelength are w/in limits and calc the intrp
	while (limflg)  {

	   if (data(j) < tmin)  {
	      datsc2(j)=-1.23e+34
	      if (ictrl == ihe) datsc3(j)=-1.23e+34 
	      j=j+1
	   }
	   else {
	   #  Calculate the slope and intercept of reflectance and error data
	      xtmp = (datab(i+1)-datab(i))
              if (abs(xtmp) < 0.1e-30) {
                 datsc2(j) = error(i)
 	         j=j+1
              }
              else  {
	   	 mr=(error(i+1)-error(i))/xtmp
              }
	      br=mr*datab(i+1)-error(i+1)

	      if (ictrl == ihe) {
	         if (abs(xtmp) < 0.1e-30) {
		   datsc3(j) = dataa(i)
		   j=j+1
	         }
                 else {
		   me=(dataa(i+1)-dataa(i))/xtmp
	         }
	         be=me*datab(i+1)-dataa(i)
	      }

	  #  Interpolate all wavelength points within point-slope equation range
	      while (data(j)<datab(i+1)&data(j)>=datab(i)&data(j)<=tmax) {   
		if (data(j)==tmax | datab(i+1)==tmax) limflg=.false.

	      #  Store in temporary buffers
	         datsc2(j)=mr*data(j)-br
	         if (ictrl == ihe) datsc3(j)=me*data(j)-be
	         j=j+1
	      }
	   }
	   i=i+1
 	}

#  Now load the data buffers into data arrays
#  Errors go into dataa, spectra into datac
	do i=1,j-1  {
	   datac(i)=datsc2(i)
	   if (ictrl == ihe) dataa(i)=datsc3(i)
	}

#  Set the points outside the interpolation limits to zero
	do i=j,4864  {
	   datac(i)=-1.23e+34
	   if (ictrl == ihe) dataa(i)=-1.23e+34
	}

#     *** write history ***
	mhist = ' '
	call namdev(idev,iname)
	call namdwv(idw1,inamw1)
	call namdwv(idw2,inamw2)
#timj
		write(ttyout,90)iname,ifile
		write(ihist,90) iname,ifile
		write(mhist(1:74),110) wmin1,wmax1,wmin2,wmax2
		write(mhist(75:148),135) inamw1,ifw1,inamw2,ifw2
	 
	if (ipts != 0) {
		mhist(149:222)=' deleted channel(s). first 74 '
		mhist(178:222)='characters of deleted channel input:'
		mhist(223:296)=idlcon
	} else {
		mhist(149:222) = ' no channels deleted'
	}
	return

10      format(' special function f49:'/' does a linear',
	       ' interpolation on sorted arrays to find data values',
               ' of a'/' wavelength file given a different set of known',
               ' data values and wavelength file.  errors, if included, are',
	       ' also interpolated.'/' note: remove gliches in data',
	       ' as they could lead to erroneous results.'/)
20      format('USAGE: file ID record no. f49 (e.g. v3f49).',/,
               ' Press <return> to hard exit.'/)
50      format(' interpolation applied to ',a,i6,':',/,1x,a/)
70      format(' error in reading errors file. press',/,
	       ' return to hard exit.',/)
90      format('f49:interpolate using ',a,' rec',i6,
	       ' + waves, see manhst')
110     format(' wav min,max: data=',1pe10.3,',',1pe10.3,
	       ' interpolate=',1pe10.3,',',1pe10.3)
135	format(' input waves: ',a, ' rec', i6, 
               ',  output waves: ',a,' rec',i6)
#  End program
	stop
	end

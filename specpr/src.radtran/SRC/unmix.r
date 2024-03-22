	subroutine unmix (idummy)
#     RATFOR 77
#     this program computes the absorption coefficient (inverse cm)
#     of the unknown element in a mixture
#     given the reflectance spectra of the mixture, the wavelengths, 
#     the index of refraction, absorption coefficient, grain size,
#     and fractional abundance
#     for each known element of the mixture, as well as a geuss at
#     the index of refraction, grain size and fraction of the unkown
#     element.
#
#     this program reads the grain size and fractions from the terminal, 
#     reads the reflectances and indices of refraction from a specpr file,
#     and writes the absorption coefficient to another specpr file
#
#     input from a specpr file:
#                       w  = wavelength in microns
#                       r = reflectance (actual observed)
#                       xn = index of refraction (real part)
#
#     input from terminal: d = grain size in cm.
#                        mu0 = cosine of angle of incidence
#                        mu  = cosine of angle of emission
#
#     output:
#               xk= absorption coefficient
#
#**************************************************************************
	implicit integer*4 (i-n)

	include "defs.h"
	include "../../src.specpr/common/spmaxes"
	include "../../src.specpr/common/label1"
	include "../../src.specpr/common/lbl4"
	include "../../src.specpr/common/lundefs"
	include "../../src.specpr/common/alphabet"
	include "../../src.specpr/common/cmd"
	include "../../src.specpr/common/lblg"
	include "../../src.specpr/common/lblwav"
#
#
	character*1536 dummy
 	equivalence (dummy,ititl)

	integer*4 fsize
	integer*4 recnum
	integer*4 idummy, filsiz, icrst
	integer*2 chkbit,ibit,bitnum

	real*4 param(18),pturb,fitcri,stndev,min,num
	real*4 xrec(SPMAXCHAN),gmalb
	character*8 file1,namwav
	character*1 iform
	integer*4 iargc, nn, ilen
#
	include "lmrefl.h"
	include "convh.h"
#
#	ttyout = screen output
#	ttyin = keyboard input

	write (ttyout,5)
5	format (//,20x,'PROGRAM unmix:',/,
		' Compute the absorption coefficient of an ',
		'unknown element in a mixture',/, 
		' from a given reflectance spectrum and the ',
		'indices of refraction, ',/,
		' absorption coefficient, grain sizes and fractional ',
		'abundances of ',/,
		' the known elements',/)

	call getnset(il)
	if (il == ihe || il == ihx) go to 10000

	call getwav(iwavd,iwavr,il)
	if (il == ihe || il == ihx) go to 10000

	call getspec(il)
	if (il == ihe || il == ihx) go to 10000

#
# now read in data set for each element and ask for record numbers, grain
# sizes, weight fractions, and density
#
	write (ttyout, 95)
95	format(' Next are prompts to input the OPTICAL CONSTANTS, ',
		' grain size,',/,'    weight fraction, etc',
		' information for each optical index set', /,
		' the LAST SET is for the UNKNOWN ELEMENT')

	do iminr = 1, nminer - 1 {

		call getn(iminr,il)
		if (il == ihe || il == ihx) go to 10000

		call getk(iminr,il)
		if (il == ihe || il == ihx) go to 10000

		call getdfp(iminr,il)
		if (il == ihe || il == ihx) go to 10000

	}
	write(ttyout,20)
20	format (' Now for the UNKNOWN')
	iminr = nminer
	call getn(iminr,il)
	if (il == ihe || il == ihx) go to 10000

	call getdfp(iminr,il)
	if (il == ihe || il == ihx) go to 10000
	
	call getmu(diangl,deangl,gmalb,astep,il)
	if (il == ihe || il == ihx) go to 10000

	call getdefs(sd,il)
	if (il == ihe || il == ihx) go to 10000

	call kiter(gmalb,astep,sd,iminr)

#
# write results
#
	irec = recnum + 1
500	write (ttyout, 501)
501	format ('type in a title',/,39('-'),'I')
	call crtin
	i = 1
	call wjfren (i,x,il)
	if (i >= 80) {
		write (ttyout,502)
502		format ('NO TITLE entered, try again')
		go to 500
	}
	ititl = iopcon(1:40)
#
# determine history
#
	mhist = ' '   # to be written
	call namdwv(iwavd,namwav)
	write (ihist, 601) namwav, iwavr, nchsav
601	format('abscoef: wave:',a,' rec',
		i6,' ch=',i5,'   ')
	if (gmalb == 1.) {
		write(mhist(1:52),602) sd,df,w0
602		format('gm alb',' sd=',f6.2,' df=',f7.3,' w0=',f7.3)
	} else {
	write (mhist(1:52),603) diangl,deangl,sd,df,w0
603	format('i=',f7.3,' e=',f7.3,' sd=',f6.2,' df=',f7.3,' w0=',f7.3)
	}

	itemp1 = 52
	write (mhist(itemp1:itemp1+135), 604) idxn(iminr), irecxn(iminr),
						idxk(iminr), irecxk(iminr)
604	format('index of refraction set: ',a,' r',i7,
		'    reflectance spectrum: ',a,' r',i7)
#
# write results to file
	do i = 1 , nchans {
		xrec(i) = xk(iminr,i)
	}
	call spcwri(xrec,il)

10000	return
	end

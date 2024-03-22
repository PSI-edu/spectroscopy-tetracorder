	subroutine abscf (idummy)
#     RATFOR 77
#     this program computes the absorption coefficient (anverse cm)
#     given the wavelength, the material index of refraction,
#     reflectance, and a grain size.
#
#     this program reads the grain size from the terminal, and the reflectance
#     and index of refraction from a specpr file and writes the
#     absorption coefficient to another specpr file
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
	character*1536 dummy
 	equivalence (dummy,ititl)

	integer*4 fsize
	integer*4 recnum
	integer*4 idummy, filsiz, icrst
	integer*2 chkbit,ibit,bitnum

	real*4 param(18),pturb,fitcri,stndev,min,num
	real*4 xrec(SPMAXCHAN), gmalb
	character*8 file1,namwav
	character*1 iform
	integer*4 iargc, nn, ilen, idlt(SPMAXCHAN)
#
	include "lmrefl.h"
	include "convh.h"
#
#	ttyout = screen output
#	ttyin = keyboard input

	write (ttyout,1)
1	format (//,20x,'PROGRAM abscoef:',/,
' Compute the ABSORPTION COEFFICIENT from a ',
			'reflectance spectrum given',/,
' the index of refraction.',/)

	nminer = 1
	iminr  = 1

	call getwav(iwavd,iwavr,il)
	if (il == ihe || il == ihx) go to 10000

	call getspec(il)
	if (il == ihe || il == ihx) go to 10000

	call getn(iminr, il)
	if (il == ihe || il == ihx) go to 10000

	call getdfp(iminr, il)
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
		write (mhist(1:52),602) sd,df,w0
		write (mhist(75:148),605) d(1),dens(1)
602		format('gm alb',' sd=',f6.2,' df=',f7.3,' w0=',f7.3)
605		format('grain size=',f7.4,'  density=',f6.2)
	} else {
		write (mhist(1:52),603) diangl,deangl,sd,df,w0
		write (mhist(75:148),605) d(1),dens(1)
603	format('i=',f7.3,' e=',f7.3,' sd=',f6.2,' df=',f7.3,' w0=',f7.3)
	}

	write (mhist(149:222), 604) idxn(iminr), irecxn(iminr)
	write (mhist(223:296), 607) idxk(iminr), irecxk(iminr)
604	format('  index of refraction set: ',a,' rec',i7)
607 	format('    reflectance spectrum: ',a,' rec',i7)
#
# write results to file
#
	do i = 1, nchans {
		xrec(i) = xk(iminr,i)
	}
	call spcwri(xrec,il)
	
10000	return
	end

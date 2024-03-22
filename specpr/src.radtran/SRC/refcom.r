	subroutine refcom(il)
#     RATFOR
#     this program computes the reflectance and mean optical path length
#     given the wavelength, the material index of refraction,
#     absorption coefficient in inverse cm, and a grain size.
#     and works for a multimineralic surface with up to 9 minerals
#
#     VERSION 2:
#     specpr file name is given on the command line, then all
#     reading and writing is done to/from the specpr file.
#
#     The user must INPUT the following:
#       specpr record # of wavelength set
#       record # of index of refraction, and absorption coeff,
#	   followed by the grain size,  weight fraction, and material density.
#       then the angle of incidence, and emission.
#
#     OUTPUT:
#             reflectance
#
#**************************************************************************

#	implicit integer*4 (i-n)
	implicit none

	include "../../src.specpr/common/spmaxes"
	include "defs.h"
	include "lmrefl.h"
	include "convh.h"
#
	include "../../src.specpr/common/label1"
	include "../../src.specpr/common/lbl4"
	include "../../src.specpr/common/lundefs"
	include "../../src.specpr/common/alphabet"
	include "../../src.specpr/common/cmd"
	include "../../src.specpr/common/lblg"
	include "../../src.specpr/common/lblwav"
	include "../../src.specpr/common/blank"
#
	character*1536 dummy
 	equivalence (dummy,ititl)

	integer*4 fsize
	integer*4 recnum
	integer*4 idummy, filsiz, icrst
	integer*2 chkbit,ibit,bitnum

	real*4 xxn(NMINL),xxk(NMINL)   # temporary holding arrays for
				#   passing info to mrefl sub.

        real*4 xemn1  # index of refraction of the matrix (at one wavelength)
        real*4 xemk1  # absorption coefficient in inverse cm of the matrix (at one wavelength)

	real*4 param(18),pturb,fitcri,stndev, sd
	real*4 astep, deangl, diangl, gdeg, gmalb, sum1, sum2
	real*4 totw, x

	character*8 file1,namwav
	character*1 iform
	integer*4 iargc, nn, ilen, idlt(SPMAXCHAN)
	integer*4 il, i, ichn, icol, id, ier, ifl, im
	integer*4 iminr, ipoint, irec, itemp1, itmp
	integer*4 iwavd, iwavr, j, k, lun, imode, num
#
#	ttyout = screen output
#	ttyin = keyboard input


      write (ttyout,5) NMINL
5     format (/, 9x, 'REFLECTANCE COMPUTATIONS ',
			'for EMBEDDED INTIMATE MIXTURES', //,
       ' This routine computes reflectance of an ',
			'intimate mixture',/,
        ' for materials in another matrix (e.g. rocks, oil)',/,
	'i      given:',5x,/,
	' wavelength, index of refraction,',
	' absorption coefficient, grain size',
			' and', /,
        ' abundance for up to ',i4,' minerals.',/,
	' The computation can be either bidirectional ',
		'or Geometric Albedo.',/)

	ipoint = 0

6	write (ttyout,7)
7	format (/,
           ' Enter  1  to compute a reflectance spectrum',
         /,'           (you give mineral abundances and grain sizes ',
      	   		'absorption coefficients).',
         /)

#7	format (/,
#           ' Enter  1  to compute a reflectance spectrum',
#         /,'           (you give mineral abundances and grain sizes).',
#         /,' Enter  2  to iterate to an optimum grain size on one mineral',/,
#           '           (you give a reference spectrum and ',
#      	   		'absorption coefficients).',
#         /)

##########(future)
#/,' To derive mineral abundances and grain sizes ',
#		'from a reflectance spectrum:',
#/,' Enter  3  to use another method of convergence',
##########

	call crtin
	i = 1
	call wjfren (i,x,il)
	if (id == ihe || id == ihx) go to 10000
	if (il == ihe || il == ihx) go to 10000
	if (x == 0.0) x=1.0
	if (x < 1.0 || x > 2.0) {          # imode 2, 3 are NOT VALID YET
                                           # (program under development)
		write (ttyout,"(/,'INVALID MODE, REENTER',/)")
		go to 6
	}
	imode = x

#
#  Read file for input comparison method
#

	if (imode != 1) {
8		write (ttyout,9)
9		format (/,' Enter the file id and record number for',
			' the spectrum to be used as the reference',/,
			'     for the comparison method.')
	
		call crtin
		i = 1
		call wjfren (i,x,id)
		call wjfren (i,x,il)
		if (id == ihe || id == ihx) go to 10000
		if (il == ihe || il == ihx) go to 10000
		if (il != 0) {
			call what(i)
			go to 8
		}
		irecxi = x    #record number for xi set iminr
		idxi = id
		call devok(4,id,irecxi,lun, ier)
		if (ier != 0) go to 8
		itmp = irecxi
		call redfil (itmp,lun,ier)
		if (ier != 0) go to 8
		write (ttyout,11) ititl, itchan
11		format (79('*'),/,
			' iteration specra set',/,5x,
			a, 5x, 'channels=',i6,/,79('-'))
		do j = 1, nchans {
			xi(j) = data(j)
		}
		xititl = ititl
	}

#
# get wavelengths
#

	call getwav(iwavd,iwavr,il)
	if (il == ihe || il == ihx) go to 10000

#
# set default values for n, k
#

        do j = 1, nchans {
                xemn(j) = 1.0    # index of refraction
                xemk(j) = 0.0    # absorption coefficient in inverse cm
                xemkm(j) = 0.0   # index of refraction imaginary part

                xvmn(j) = 1.0    # vacuum index of refraction
                xvmk(j) = 0.0    # vacuum absorption coefficient in inverse cm
                xvmkm(j) = 0.0   # vacuum index of refraction imaginary part
        }


#
# get n, k for medium to embed particles
#

	call getembn(il)   # get index of refraction for embedding medium
	if (il == ihe || il == ihx) go to 10000

	call getembk(il)   # get absorption coefficients for embedding medium
	if (il == ihe || il == ihx) go to 10000

#
# get number of minerals
#

25	if (imode == 2) {  # one mineral, iterate on grain size
		nminer = 1
		write (ttyout,63) nminer
63		format (/, 1x, i6, ' optical embedded index set(s) allowed',
			' in this computation')
	} else {
		call getnset(il)
		if (il == ihe || il == ihx) go to 10000
	}

	do iminr = 1, nminer {
		call getn(iminr,il)
		if (il == ihe || il == ihx) go to 10000
	
		call getk(iminr,il)
		if (il == ihe || il == ihx) go to 10000
#
# if imode is not 1 or 2 set up array containing channel numbers to define band
#         used in calculating convergence 
#
	if (imode > 2) {
75		write (ttyout,76) iminr
76		format (/,' Three channel numbers are used to define',
			' continuum and band center',/,' for up to three', 
     			' absorption bands for mineral ',i2,//,' Enter',
			' the channel numbers for the continuum and band',
			' center',/,' on one line in numeric order.  Enter',
			' up to three sets.')

		do j=1,3 {
			call crtin
			i = 1
			call wjfren (i,x,il)
			if (il == ihe || il == ihx) go to 10000
			if (il != 0) {
				call what(i)
				go to 75
			}
			if (x == 0) {
				naband(iminr) = j-1
				go to 80
			}
			ibandn(iminr,j,1)=x

			call wjfren (i,x,il)
			if (il == ihe || il == ihx) go to 10000
			if (il != 0) {
				call what(i)
				go to 75
			}
			ibandn(iminr,j,2)=x
			if (x < ibandn(iminr,j,1)) {
				write (ttyout,77)
77				format (/,' ERROR, enter channel numbers',
					' in increasing order')
				go to 75
			}
			
			call wjfren (i,x,il)
			if (il == ihe || il == ihx) go to 10000
			if (il != 0) {
				call what(i)
				go to 75
			}
			ibandn(iminr,j,3)=x
			if (x < ibandn(iminr,j,2)) {
				write (ttyout,77)
				go to 75
			}

			if (j == 3) naband(iminr)=3
		} # end do j=1,3
	} # end if imode > 2
#
# if imode 3 or 4 selected determine mask for this absorption coeff set.
#
	if (imode == 3) {
		call abpeak (data,iminr)
	}
	
80	call getdfp(iminr,il)
	if (il == ihe || il == ihx) go to 10000

	}  # end do iminr=1,nminer

	totw = 0.
	do i= 1,nminer {
		totw = totw + weight(i)
	}
	if (abs(totw-1.0) > 0.000001) {
		write (6,96) totw
96		format (/,' ERROR: TOTAL OF WEIGHTS ',
				'is NOT EQUAL TO 1.0:', f10.7)
		go to 10000  # THIS IS EXTREME, should check along way
	}

	call getmu(diangl,deangl,gmalb,astep,il)
	if (il == ihe || il == ihx) go to 10000

	call getdefs(sd,il)
	if (il == ihe || il == ihx) go to 10000

#
# if imode == 1 or 2, all channels are potentially valid
#
	if (imode != 3) {
		do i = 1, nchans {
			imask(i) = 1
		}
	}
#
# set mask value to 0 for invalid channels
#
	do i = 1, nchans {
		if (wav(i) < 0.1e-30) imask(i) = 0
		do im = 1, nminer {
			if (xn(im,i) < 0.1e-30) imask(i) = 0
			if (xk(im,i) < 0.1e-30) imask(i) = 0
		}
	}
#
#****************************************************************
#  call simplx subroutines for comparison method
#
	if (imode > 2) {
#		if (nminer > 1) {
#			do i=1,nminer-1 {
#				param(i)=weight(i)
#			}
#			do i=1,nminer {
#				param(i+nminer-1)=dlog10(d(i))
#			}
#		} else {
#			param(1) = d(1)
#		}
#		n = 2*nminer -1
229		write (ttyout,230)
230		format (/,'type in the PERTURBATION FACTOR and',/,5x,
			'the FIT CRITERION per data point',/)
		call crtin
		icol=1
		call wjfren (icol,x,il)
		if (il == ihe || il == ihx) go to 10000
		if (icol >= 80 || il != 0) {
			call what (icol)
			go to 229
		}
		if (x < 0.00001 || x > 100000.) {
			call what (icol)
			write (ttyout, 231)
231			format ('OUT OF RANGE, REENTER')
		}
		pturb = x
		call wjfren (icol, x, il)
		if (il == ihe || il == ihx) go to 10000
		if (icol >= 80 || il != 0) {
			call what (icol)
			go to 229
		}
		if (x < 0.00000001 || x > 1.) {
			call what (icol)
			write (ttyout, 231)
		}
		fitcri = x
		num = 1

		if (imode == 3) {
			call cnvgbr (pturb, fitcri)
		}

		if (nminer > 1) {
			do k=1,nminer-1 {
				weight(k)=param(k)
			}
			do k=1,nminer {
				d(k)=1.0d1**(param(k+nminer-1))
			}
		} else {
#			d(1) = 1.0d1**(param(1))
			k=1
			d(k) = 1.0d1**(param(k))
		}
	}
#
#*****************************************************************
#
	if (imode == 2) {  # iterate to get to best grain size for 1 mineral

		call cnvg1g (sum1, sum2)

	}

300     do ichn= 1, nchans {

           if (imask(ichn) > 0) {   # do the reflectance calculation
#
	      do ifl= 1,nminer {
#
#**debug	      write statement
		if(ichn == 50)  {
     	      write (6,18) ichn, ifl, wav(ichn), xn(ifl,ichn), xk(ifl,ichn)
18            format (1x,' DEBUG: chan=',i5,3x, 'set=',i4,' input: w=',
			f8.4,'  xn=',f7.4,'  xk=',f10.4)
		}
	      }
#
	      do ifl= 1,nminer {  # copy optical constants to 1D arrays for calculations

			xxn(ifl) = xn(ifl,ichn)
			xxk(ifl) = xk(ifl,ichn)
	      }

                xemn1=xemn(ichn)  # index of refraction of the matrix (at one wavelength)
                xemk1=xemk(ichn)  # absorption coefficient in inverse cm of the matrix (at one wavelength)


#		if (mod(ichn,5) == 0) write(ttyout,*) 'Just starting ', ichn

		call  calcr(ichn,r(ichn),rfirst(ichn),xxn,xxk,xemn1,xemk1,gmalb,astep,sd)

           } else {

		r(ichn)      = -1.23e34    # deleted value
		rfirst(ichn) = -1.23e34

           }

	      if(ichn == 50)  {
			write (ttyout,411) ichn, wav(ichn), r(ichn), rfirst(ichn),
				 wsmean(ichn)
	      }
411	      format (1x,' DEBUG:', i4, 2x, 4(1pe15.6))
#
        }
	do i = 1,nchans {
		datac(i) = r(i)
		dataa(i) = wav(i)
	}
#
# write results
#
	irec = recnum + 1
500	write (ttyout, 501)
501	format ('Type in a TITLE',/,39('-'),'I')
	call crtin
	i = 1
	call wjfren (i,x,il)
	if (i >= 80) {
		write (ttyout,502)
502		format (/,'NO TITLE entered, try again')
		go to 500
	}
	ititl = iopcon(1:40)

	call plotit(ipoint)
	call eralph
	if (ipoint == ihx) go to 10000
	if (ipoint == ihw) go to 25
	if (ipoint == ihu) {
		call getdfp(1,il)
		go to 300
	}
	if (ipoint == ihv) {
		call getdfp(2,il) 
		go to 300
	}
#
# determine history
#
	mhist = ' '   # to be written
	call namdwv(iwavd,namwav)
	write (ihist, 601) nminer, namwav, iwavr, nchsav
601	format('radtran:        ',i2,' sets; wave:',a,' rec',
		i6,' ch=',i5,'   ')
	if (gmalb == 1.) {
		write(mhist(1:52),602) sd,df,w0
602		format('gm alb',' sd=',f6.2,' df=',f7.3,' w0=',f7.3)
	} else {
	write (mhist(1:52),603) diangl,deangl,sd,df,w0
603	format('i=',f7.3,' e=',f7.3,' sd=',f6.2,' df=',f7.3,' w0=',f7.3)
	}

	do i = 1, nminer {
		if (i > 4) next    # rest won't fit in mhist
					# temp solution
		itemp1= 52 + (i-1)*50
		write (mhist(itemp1:itemp1+50),604) i,idxn(i),irecxn(i),
			idxk(i),irecxk(i),ddflt(i),weight(i),dens(i),
			scoef(i)
604		format('(set',i2,':',a1,i5,a1,i5,';',
		f8.5,1x,f7.5,1x,f5.3,1x,f5.2,')')
	}
	if (imode == 2) {   # grain size iteration imode, give fit stats
		itemp1 = itemp1 + 51
		write(mhist(itemp1:itemp1 +56), 607) sum1, sum2
607		format (' sum of residuals= ',1pe12.5,
			' sqrt(sumsq)= ',1pe12.5)
	}

	if (nminer > 4) {    #print message in mhist (temp solution)
		mhist(253:296)=' rest will not fit    '
	}
	if (gmalb != 1.) {
		siangl = int(diangl*21600000.0)
		seangl = int(deangl*21600000.0)
		gdeg = g*57.29578
		sphase = int(gdeg*5400000.0)
	   }
	nruns = 1
	revs = 1
	iwtrns = 1
	xnrm = 1.0
#
# write results to file
#
	call spcwri(r,il)
	
2000  continue
3000  continue
10000	return
	end

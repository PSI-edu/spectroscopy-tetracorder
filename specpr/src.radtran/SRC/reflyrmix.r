	subroutine reflyrmix(il)

#     RATFOR
#
#     this program computes the reflectance and mean optical path length
#     for layered media,
#     given the wavelength, the material index of refraction,
#     absorption coefficient in inverse cm, and a grain size.
#     and works for a multimineralic surface with up to NMINL minerals
#     and computes molecular mixtures,
#     and includes areal mixtures for NAREF spectra (note that
#     the third areal component is the intimate mix
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

	include "../../src.specpr/common/label1"
	include "../../src.specpr/common/lbl4"
	include "../../src.specpr/common/lundefs"
	include "../../src.specpr/common/alphabet"
	include "../../src.specpr/common/cmd"
	include "../../src.specpr/common/lblg"
	include "../../src.specpr/common/lblwav"
	include "../../src.specpr/common/blank"
	include "../../src.specpr/common/inputhistory"
#
	character*1536 dummy
 	equivalence (dummy,ititl)

	integer*4 fsize, intx, mmixflag
	integer*4 recnum
	integer*4 idummy, filsiz, icrst
	integer*2 chkbit,ibit,bitnum
        integer*4 irecaf     #record number for areal frac set
        integer*4 idaf       # file id for areal frac set
	integer*4 iandx      # index for areal fraction loop
	integer*4 mlayers    # temp variable for layers in actual use
	integer*4 jlyr, iprt, irecou, ista

	integer*4  olayers       # temporary variable

	real*4   mdensum                    # temporary variable
	real*4   dmdensum                   # temporary variable
	real*4   xx, x1, rm, o1, ext, r11   # temporary variables
	real*4   pi4, ext1, ext0, e1, e0    # temporary variables

	real*4 mmixabund   # molecular mix abundance component 2
	real*4 mmix1       # molecular mix abundance component 1
	real*4 r1          # working variable for first surface reflection
	real*4 totafrac    # total areal fraction component of areal fraction
	real*4 totifrac    # area fraction of the intimate mix component.

### may not be needed:
	real*4 xxn(NMINL),xxk(NMINL)    # temporary holding arrays for
					#   passing info to mrefl sub.
		# xxn = index of refraction    for one layer, one wavelength, all components
		# xxk = absorption coefficient for one layer, one wavelength, all components

        real*4 xemn1  # index of refraction of the matrix (at one wavelength)
        real*4 xemk1  # absorption coefficient in inverse cm of the matrix (at one wavelength)

	real*4 param(18),pturb,fitcri,stndev
        real*4 astep, deangl, diangl, gdeg, gmalb, sum1, sum2
        real*4 totw, x, sd, totmol

	real*4 pi    # this section temp variable used in effective medium theory
	real*4 nh
	real*4 nfe
	real*4 ah
	real*4 afe
	real*4 frac
	real*4 kfe 
	real*4 z
	real*4 alphafe 
	real*4 alpha 

	character*8 file1,namwav
	character*1 iform
	integer*4 iargc, nn, ilen, idlt(SPMAXCHAN)
        integer*4 il, i, ichn, icol, id, id2, ier, ifl, im
        integer*4 iminr, ipoint, irec, itemp1, itmp
        integer*4 iwavd, iwavr, j, k, lun, imode, num
	integer*4 jloop, jstart
#
#
#	ttyout = screen output
#	ttyin = keyboard input


      write (ttyout,5) MAXLAYERS, NMINL,  NMMIX
5     format (/,/,'============',/,
              3x, 'REFLECTANCE COMPUTATIONS ',
		'for LAYERED INTIMATE, MOLECULAR and AREAL MIXTURES', //,
       ' This routine computes reflectance of an ',
			'intimate mixture given:',5x,/,
	' wavelength, index of refraction,',
	' absorption coefficient, grain size',
			' and', /,
        ' abundance for up to', i3,' LAYERS, each containing up to',
	  i4,' minerals,',/,
	' each with up to',i3,' embedded components.',/,
	' The computation is bidirectional ')

	write (ttyout,*) 'NOTE: current limit = 2 layers. 6/2019 (3rd layer still needs programming.) '


	#  not yet: 	'or Geometric Albedo,',/,           - 6/2019
	#            is bidirectional or Geometric Albedo for non layerd media

	write (ttyout,4) nlayers
4	format ('This is a', i3,' layer calculation')

	ipoint = 0

6	write (ttyout,7)
7	format (/,
  ' Enter  1  to compute a reflectance spectrum',
/,'           (you give mineral abundances and grain sizes).',
/,'        1 IS THE CURRENT ONLY OPTION.',
/)

##############
#/,' Enter  2  to iterate to an optimum grain size on one mineral',/,
#  '           (you give a reference spectrum and ',
#			'absorption coefficients).',
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
	intx = nint(x)  
	x=intx         # convert back in case of not quite an integer value
	if (intx < 1 || intx > 1) {          # imode 3 IS NOT VALID YET
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
# get wavelength
#

	call getwav(iwavd,iwavr,il)    # get wavelengths
	if (il == ihe || il == ihx) go to 10000

#
# get layer thickness
#
	call getlayerthick(il)

#
# set default matrix = vacuum
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
# get number of minerals per layer
#

25	if (imode == 2) {  # one mineral, iterate on grain size
		nminer = 1
		write (ttyout,63) nminer
63		format (1x, i6, ' optical index set allowed',
			' in this computation',/)
	} else {
		call getnst2(il)    # get the number of optical constants sets
		if (il == ihe || il == ihx) go to 10000
	}

	mlayers = 1            # temp variable for layers in actual use
	if ( nlayers > 1 ) {
		mlayers = nlayers
	}

       for (jlyr=1; jlyr<=mlayers; jlyr=jlyr+1) {      # layers

	nminer = nminerlyr(jlyr)
	do iminr = 1, nminer {

		# mmixflag =0 = no molecular mixture
		# mmixflag =1 = molecular mix of abs coefs
		# mmixflag =2 = effective medium mix
		# mmixflag =3 = effective medium mix + molecular mix of abs coefs
		#		effective medium mix is only component 2 of the mix

70		mmixflag=0     # default: no molecular mix

		call getn2(jlyr, iminr,il,mmixflag)          # get real indices of refraction
		if (il == ihe || il == ihx) go to 10000
	
71		call getk2(jlyr, iminr,il,mmixflag)          # get absorption coefficients
		if (il == ihe || il == ihx) go to 10000

		# now if nmixflag >0, molecular mix the components.

		if (mmixflag > 0) {  # molecular mix, effective medium mix, or both

			if (xmolfra(1,iminr) < 0.0 ) {  # floating first abundance
				totmol=0.0              # compute first molecular abunance
				do jloop = 2, nmolmix(iminr) {
					totmol = totmol + xmolfra(jloop,iminr)
				}
				xmolfra(1,iminr) = 1.0 - totmol

				if (xmolfra(1,iminr) > 1.0 || xmolfra(1,iminr) < 0.0 ) {

					write (ttyout, 72) iminr, xmolfra(1,iminr) 
72					format (' ERROR: xmolfra(1,',i3,') =',f12.6,
						' is OUT PF RANGE, reenter',/)
					go to 71
				}
				write(ttyout,"(' first molecular mix fraction='f12.6,/)") xmolfra(1,iminr)
			}
			totmol=0.0              # compute first molecular abunance
			do jloop = 1, nmolmix(iminr) {
				totmol = totmol + xmolfra(jloop,iminr)
			}
			write (ttyout, 73) iminr, totmol
73			format (' Total molecular fractions for component',i3,' =',f12.6,//)

			# now compute the optical constants of the
			# molecular mix or effective medium mix.

			do j = 1, nchans {
				xn(iminr,j)         = 0.0
				xnlyr(jlyr,iminr,j) = 0.0
				xk(iminr,j)         = 0.0
				xklyr(jlyr,iminr,j) = 0.0
			}

			if (mmixflag == 2 || mmixflag == 3) {   #  effective medium mix


				# Rayleigh absorbers are highly absorbing
				# particles with sizes much smaller than
				# the wavelength of light.  This routine
				# adds the absorption due to nano-sized
				# Rayleigh absorbing particles to the
				# absorption coefficient of the host
				# medium using effective medium theory
				# as outlined by Hapke, Space Weathering
				# from Mercury to the Aateroid Belt, JGR,
				# v106, p10039-10073, 2001.

				# Use Fromulation from Hapke, JGR, 2001, p10061
				# equations 11 and 12.

				# alphafe = 36 pi * frac *z / wave    eqn 11
				#
				# z = nh^3 nfe kfe / ((nfe^2 - kfe^2 + 2nh^2)^2 + (2nfe*kfe)^2)
				
				# where nh = index of refraction of the medium
				#       nfe= index of refraction of the Rayleigh absorber  (e.g. iron)
				#       kfe= the absorption, as in n = nfe + ikfe
				#       
				#       frac = mass fraction
				#
				# above variable nomenclature: variable that begind with:
				#
				# n* = index of refractions
				# a* = absorption coefficients in per centimeter
				# k* = complex index of refraction, as in n + ik
				#
				# wavelengths MUST be in microns
				
				# alpha combined = aplhah + alphafe

				pi=3.14159265

				write (ttyout,"(' DEBUG: computing effective medium mix')")

			    do j = 1, nchans {

				nh  = xmindx(1,iminr,j)
				nfe = xmindx(2,iminr,j)
				ah  = xmabsc(1,iminr,j)
				afe = xmabsc(2,iminr,j)
				frac= xmolfra(2,iminr)

				# DEBUG:
				if (j == 10) {
					write (ttyout, 123) j, wav(j), nh, ah, nfe, afe, kfe
123					format ('ch=',i3,'  wave=',f12.6,'  nh=',f12.6,
                                             '  ah=',f12.6,'  nfe=',f12.6 ,'  afe=',f12.6,
                                             '  kfe=',f12.6)
				}

				kfe = afe*wav(j)/(4.0*pi*10000.)
				z=(nh**3) * nfe * kfe / ((nfe**2 - kfe**2 + 2.0*(nh**2))**2 + (2.0*nfe*kfe)**2)
				alphafe = 36.0 * pi * frac *z *10000. / wav(j)   # the 10000 factor converts 
									    #microns to cm in the wavelengths
				alpha = ah + alphafe
				
				xk(iminr,j)         = alpha
				xklyr(jlyr,iminr,j) = alpha
				xn(iminr,j)         = nh     # note this assumes component 2 is so small
				xnlyr(jlyr,iminr,j) = nh     # note this assumes component 2 is so small
							     # it doesn't affect the index of refraction.
							     # probably should add the general case.

			    }
			}  # end effective medium mix block

			if (mmixflag == 1 || mmixflag == 3  ) {   # linear absorption coefficient mix

			    jstart = 1
			    if (mmixflag == 3) {   # already did 1 and 2 above

				jstart = 3         #  molecular + effective medium mix
			    }

			    do j = 1, nchans {

				do jloop = jstart, nmolmix(iminr) {

					xn(iminr,j) = xn(iminr,j) + xmolfra(jloop,iminr) *xmindx(jloop,iminr,j)
					xk(iminr,j) = xk(iminr,j) + xmolfra(jloop,iminr) *xmabsc(jloop,iminr,j)

					xnlyr(jlyr,iminr,j) = xnlyr(jlyr,iminr,j) + xmolfralyr(jlyr,jloop,iminr) *xmindxlyr(jlyr,jloop,iminr,j)
					xklyr(jlyr,iminr,j) = xklyr(jlyr,iminr,j) + xmolfralyr(jlyr,jloop,iminr) *xmabsclyr(jlyr,jloop,iminr,j)
				}
			    }
			} 

			if ( mmixflag < 0 || mmixflag > 3)  {

				write (ttyout,"(' mol mix unknown ',i6,/)") mmixflag
				go to 70

			}
		}
#
# if imode is not 1 or 2 set up array containing channel numbers to define band
#         used in calculating convergence 
#
	if (imode > 2) {
75		write (ttyout,76) iminr
76		format (' Three channel numbers are used to define',
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
				go to 80           # ZZZ RED is this correct go to RNC 11/2010???
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
77				format (' ERROR, enter channel numbers',
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
	
80	call getdfp(iminr,il)  # get grain size, weight, density
	   if (il == ihe || il == ihx) go to 10000

	   weightlyr(jlyr,iminr) = weight(iminr)
	   denslyr(jlyr,iminr)   = dens(iminr)
	   dlyr(jlyr,iminr)      = d(iminr)
	   ddfltlyr(jlyr,iminr)  = ddflt(iminr)
	
	

	}  # end do iminr=1,nminer
       }   # end of layers loop

	for (jlyr=1; jlyr<=mlayers; jlyr=jlyr+1) {      # layers

		nminer = nminerlyr(jlyr)
		totw = 0.
		do i= 1,nminer {
			totw = totw + weight(i)
		}
		if (abs(totw-1.0) > 0.000001) {
			write (6,96) totw, jlyr
96			format (' ERROR: TOTAL OF WEIGHTS ',
				'in layer', i3,
					'is NOT EQUAL TO 1.0:', f10.7,
			' press return to start over')
			call what(1)
			call crtin
			go to 10000  # THIS IS EXTREME, should check along way
		}
	}  # end jlyr loop

	# get the incidence, emission, and phase angles
	# gmalb = 1 means compute geometric albedo
	call getmu(diangl,deangl,gmalb,astep,il)
	if (il == ihe || il == ihx) go to 10000

	# get normalization go ahead, and grain size wavelength dependence parameters
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
#  ZZZZZZZZZZZZ need to add layers loop
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
#  ZZZZZZZZZZZZ need to add layers loop
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
230		format ('type in the PERTURBATION FACTOR and',/,5x,
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
#  ZZZZZZZZZZZZ need to add layers loop ????? or delete this option
#
	if (imode == 2) {  # iterate to get to best grain size for 1 mineral

		call cnvg1g (sum1, sum2)

	}

############### LAYER Calculations #########################################
#  This is the main reflectance calculation in this block.

	write (ttyout,*) 'DEBUG: reflyrmix start layer calculations'

        for (jlyr=1; jlyr<=mlayers; jlyr=jlyr+1) {      # layers

	  write (ttyout,*) 'DEBUG: reflyrmix starting layer ', jlyr

	  nminer = nminerlyr(jlyr)
	  do ifl= 1,nminer {
		weight(ifl) = weightlyr(jlyr, ifl)
		dens(ifl)   = denslyr(jlyr, ifl)
	  }

300      do ichn= 1, nchans {

           if (imask(ichn) > 0) {
#
#	      do ifl= 1,nminer {
#
#**debug	      write statement
#     	      write (6,18) ichn, ifl, wav(ichn), xn(ifl,ichn), xk(ifl,ichn)
#18            format (1x,'chan=',i5,3x, 'set=',i4,' input: w=',
#			f8.4,'  xn=',f7.4,'  xk=',f10.4)
#
#	      }
#
	# xxn = index of refraction    for one layer, one wavelength, all components
	# xxk = absorption coefficient for one layer, one wavelength, all components

	      do ifl= 1,nminer {
			xxn(ifl) = xnlyr(jlyr, ifl, ichn)
			xxk(ifl) = xklyr(jlyr, ifl, ichn)
	      }

		xemn1=xemnlyr(jlyr, ichn)  # index of refraction of the matrix (at one wavelength)
		xemk1=xemklyr(jlyr, ichn)  # absorption coefficient in inverse cm of the matrix (at one wavelength)

#		if (mod(ichn,5) == 0) write(ttyout,*) 'Just starting ', ichn
		# gmalb = 1 means compute geometric albedo, using step size astep
		
		call  calcrlyr(jlyr, ichn,r(ichn),rfirst(ichn),xxn,xxk,xemn1,xemk1,gmalb,astep,sd)


           } else {

		r(ichn) = -1.23e34
		rfirst(ichn) = -1.23e34
           }


	   if(ichn == 50)  {  # debug
			write (ttyout,411) ichn, wav(ichn), r(ichn),
				rfirst(ichn), wsmean(ichn)
411			format (1x,' DEBUG:', i4, 2x, 4(1pe15.6))
	   }

	   # Now collect data for the layer just computed



		rlyr(jlyr, ichn) = r(ichn)
		rfirstlyr(jlyr, ichn) = rfirst(ichn)

         } # end loop for channels

        }   # end loop for layers

###### Check to see if we should subtract off first surface reflection
###### for certain viewing geometries.

429	write (ttyout, 430)
430	format (' Type  nf  for no first-surface reflection',/,
		'           For viewing geometries where the surface ',/,
		'           is glassy smooth and we would not see the',/,
		'           specular component'/)

	call crtin
	i = 1
	call wjfren (i,x,il)
	if (il == ihe || il == ihx) go to 10000
	if (il != 0 && il != ihn) {
		call what(i)
		go to 429
	}
	if ( iopcon(i-1:i) == 'nf' ) {

		do i = 1,nchans {                # subtract first sruface reflection
			rlyr(1,i) = rlyr(1,i) - rfirstlyr(1,i)
		}

	}

######################################################################
####### OK, so now we have the intimate mixture calculation completed for layers, now comute for the surface.

	# for the layer calculations, we need (using Clark and Roushm JGR, 1984, =C&R)

	# by definition r = w^m, r=reflectance, w=single scattering albedo, m =number of particles
	#               ln(r) = m * ln(w), thus m = ln(r) / ln(w)

	# lbarprime   = (-1 / xkmeanlyr) * ln(wsmean)        (C&R eqn 18) = mean optical path in a single grain
	# mparticlyr  = ln(r)   / ln(wsmean)                 (C&R eqn 19) = mean number of particles encountered by photons
	# lbar        = m * lbarprime  = mopllyr             (C&R eqn 20) = mean optical path length = mopl
	# odlyr       = (pi/4) * ddfltmeanlyr * sqrt(mparticlyr/2) (C&R eqn 24) = mean penetrated layer thickness = optical depth
	#                      ds = mean distance between particles.
	# argue: ds = mean grain diameter, dmean, * some surface density factor, but = dmean for close packing.

	# lbar = mopl not used  to get optcal depth   

	# NEED TO COMPUTE IF we want mopl: mean abs coef per layer   xkmeanlyr
	# NEED TO COMPUTE: mean grain size per layer ddfltmeanlyr

	# ddfltmeanlyr = sum ovet i components: (di * mi /rhoi) / sum over i: (mi /rhoi)

	# given the number of grains encountered, mparticlyr and the mean grain diameter, ddfltmeanlyr
	# derive mopl:  TBD

	# NOTE: the odlyr thickness, while in cm, is cm of vertical material, excluding spaces
	#       between particles.  For fairycastle low density surface, the actual thickness
	#       would be greater.   (Need to derive relationship.  06/04/2019 - RNC)

	pi4 = 3.14159265/4.0

	if (mlayers > 1) {
		olayers= mlayers-1	
		for (jlyr=1; jlyr<=mlayers; jlyr=jlyr+1) {      # layers

			mdensum  = 0.0  # mass / density sum in one layer
			dmdensum = 0.0  # mass / density / diameter sum in one layer
			do iminr = 1, nminer {
				xx = weightlyr(jlyr,iminr) / denslyr(jlyr,iminr)
				mdensum  = mdensum  + xx
				dmdensum = dmdensum + xx / ddfltlyr(jlyr,iminr)
			}
			if (dmdensum < 0.1e-18 ) {

				write (ttyout,*) 'ERROR: layer',jlyr,' sum mass / density /diam < 10^-19, TOO SMALL'
				dmdensum = 0.1e-18
			}
			ddfltmeanlyr(jlyr) = mdensum / dmdensum  # mean grain diameter in cm

			do i = 1,nchans {

				mparticlyr(jlyr,i) = alog(rlyr(jlyr,i)) / alog(wsmeanlyr(jlyr,i))              # C&R eqn 19
				# lbarprime          = (-1.0 /xkmeanlyr(jlyr,i)) * alog(wsmean(jlyr,i))          # C&R eqn 18
				# mopllyr(jlyr,i)    = mparticlyr(jlyr,i) * lbarprime                            # C&R eqn 20

				# C&R eqn 24 is penetration thickness, 1/e depth, not optical depth
				pdepthlyr(jlyr,i)  =  pi4 * ddfltmeanlyr(jlyr) * sqrt(mparticlyr(jlyr,i)/2.0)  # C&R eqn 24

				# optical depth:  transmission = exp (-1 *odepth * thickness)
				# pdepthlyr is one optical depth = 1/e
				# optical depth = thickness / pdepthlyr

				if(pdepthlyr(jlyr,i) < 0.1e-25) pdepthlyr(jlyr,i) = 0.1e-25

				if (jlyr < mlayers) {   # not the bottom layer
					odlyr(jlyr,i) = tlyr(jlyr) / pdepthlyr(jlyr,i)
				} else {
					odlyr(jlyr,i) = 999.0   # essentially completely opaque
				}
			}
		}
		if (mlayers == 2) {

			x1 = -1.0 * tlyr(1)                         # thickness of 1st layer

			do i = 1,nchans {

				rm   = rlyr(1,i) - rfirstlyr(1,i)
				r11  = 1.0 - rfirstlyr(1,i)
				o1   = odlyr(1,i) * x1                   # 
				ext0 = exp(o1)
				ext1 = 1.0 - ext0
				e1   = r11*ext0

				rtop(i)   = rfirstlyr(1,i) + rm * ext1

				rsecnd(i) = e1 * rlyr(2,i) * ext0

				rthird(i) = e1 * rlyr(2,i)*ext1 * rlyr(2,i)*ext0

				rfourth(i) = e1 * rlyr(2,i)*ext1 * rlyr(2,i)* ext1 *rlyr(2,i) *ext0

				# rfifth and sixth probably not needed.  (rfifth not yet defined)

				r(i) = rtop(i) + rsecnd(i) + rthird(i) + rfourth(i)

					#	 rlyr(1,i)     * (rlyr(2,i)**2) * ext1 * ext0**2 +
					#	(rlyr(1,i)**2) * (rlyr(2,i)**3) * ext1**4 +
					#	(rlyr(1,i)**3) * (rlyr(2,i)**4) * ext1**5 +
					#	rsixth(i)
					
				if (i == 800 || i ==  1810 || i == 2240) {   # write diagnostics

					write (ttyout,*) ' '
					write (ttyout,*) 'debug reflyrmix: wav        =', wav(i),' i=',i
					write (ttyout,*) 'debug reflyrmix: rfirst     =', rfirstlyr(1,i)
					write (ttyout,*) 'debug reflyrmix: rm         =', rm
					write (ttyout,*) 'debug reflyrmix: rlyr(1,i)  =', rlyr(1,i)
					write (ttyout,*) 'debug reflyrmix: rlyr(2,i)  =', rlyr(2,i)
					write (ttyout,*) 'debug reflyrmix: odlyr(1,i) =', odlyr(1,i)
					write (ttyout,*) 'debug reflyrmix: o1         =', o1
					write (ttyout,*) 'debug reflyrmix: ext0       =', ext0
					write (ttyout,*) 'debug reflyrmix: ext1       =', ext1
					write (ttyout,*) 'debug reflyrmix: e1         =', e1,'   = layer 1 transmitted'
					write (ttyout,*) 'debug reflyrmix: rsecnd(i)  =', rsecnd(i)
					write (ttyout,*) 'debug reflyrmix: rthird(i)  =', rthird(i)
					write (ttyout,*) 'debug reflyrmix: rfourth(i) =', rfourth(i)
					write (ttyout,*) 'debug reflyrmix: r(i)       =', r(i),'     final reflectance'
				}

			}
		} else {

			write (ttyout,*) 'ERROR: at present can only do 2 layers'
			stop
		}
		# optical depths

		write (ttyout,*)  'writing optical depths to file v'
		ititl = "radtran penetration depth, thick layer 1"
                        #         1111111111222222222233333333334
                        #1234567890123456789012345678901234567890
			do i = 1,nchans {
				data(i) = pdepthlyr(1,i)
			}
			id2 = ihv  # write to v
			call devlun(4,id2,lun)
			call devsta(lun,ista,ier,iprt)
			if (iprt < 0) {

				write (ttyout,*) 'ERROR: v is not writeable protection >= 0'
				stop
			}
			irecou = iprt+1
			itmp = irecou
			call wrifil(itmp,lun,ier)

		ititl = "radtran penetration depth, thick layer 2"
			do i = 1,nchans {
				data(i) = pdepthlyr(2,i)
			}
			call devsta(lun,ista,ier,iprt)
			irecou = iprt+1
			itmp = irecou
			call wrifil(itmp,lun,ier)

		ititl = "radtran layer 1 optical depth           "
			do i = 1,nchans {
				data(i) = odlyr(1,i)
			}
			call devsta(lun,ista,ier,iprt)
			irecou = iprt+1
			itmp = irecou
			call wrifil(itmp,lun,ier)

		write (ttyout,*)  'writing layer reflectance to file v'
		ititl = "radtran layer 1 reflectance             "
			do i = 1,nchans {
				data(i) = rlyr(1,i)
			}
			call devsta(lun,ista,ier,iprt)
			irecou = iprt+1
			itmp = irecou
			call wrifil(itmp,lun,ier)

		ititl = "radtran layer 2 reflectance             "
			do i = 1,nchans {
				data(i) = rlyr(2,i)
			}
			call devsta(lun,ista,ier,iprt)
			irecou = iprt+1
			itmp = irecou
			call wrifil(itmp,lun,ier)

		ititl = "radtran layer 1 first surface reflection"
			do i = 1,nchans {
				data(i) = rfirstlyr(1,i)
			}
			call devsta(lun,ista,ier,iprt)
			irecou = iprt+1
			itmp = irecou
			call wrifil(itmp,lun,ier)

		ititl = "radtran top component of layer calc     "
			do i = 1,nchans {
				data(i) = rtop(i)
			}
			call devsta(lun,ista,ier,iprt)
			irecou = iprt+1
			itmp = irecou
			call wrifil(itmp,lun,ier)

		ititl = "radtran 2nd component of layer calc     "
			do i = 1,nchans {
				data(i) = rsecnd(i)
			}
			call devsta(lun,ista,ier,iprt)
			irecou = iprt+1
			itmp = irecou
			call wrifil(itmp,lun,ier)

		ititl = "radtran 3rd component of layer calc     "
			do i = 1,nchans {
				data(i) = rthird(i)
			}
			call devsta(lun,ista,ier,iprt)
			irecou = iprt+1
			itmp = irecou
			call wrifil(itmp,lun,ier)

		ititl = "radtran 4th component of layer calc     "
			do i = 1,nchans {
				data(i) = rfourth(i)
			}
			call devsta(lun,ista,ier,iprt)
			irecou = iprt+1
			itmp = irecou
			call wrifil(itmp,lun,ier)


	}



######################################################################
####### Now add in areal mixtures.

435	write (ttyout, 436)
436	format (' Enter the number of areal mixture spectra to add in areal fractions')

	numaref=0
	call crtin
	i = 1
	call wjfren (i,x,il)
	if (il == ihe || il == ihx) go to 10000
	if (il != 0) {
	call what(i)
		go to 435
	}
	numaref = x    # number of areal mix spectra in use
	if (numaref > NAREF) {
		write (ttyout,437) NAREF
437		format (' ERROR: number of areal mixtures is >',
			i5,' re-enter',/)
		go to 435
	}

	

	if (numaref > 0) {   # get first areal fraction spectrum

	    do iandx=1, numaref {

439		write (ttyout,440) iandx
440		format (' Enter the file id and record number ',
			' followed by the areal fraction (0.0 to 0.99999',/,
			' for areal spectrum', i5,/)
		
		call crtin
		i = 1
		call wjfren (i,x,id)
		call wjfren (i,x,il)
		if (id == ihe || id == ihx) go to 10000
		if (il == ihe || il == ihx) go to 10000
		if (il != 0) {
			call what(i)
			go to 439
		}
		irecaf = x    #record number for areal frac set
		idaf = id
		call devok(4,id,irecaf,lun, ier)
		if (ier != 0) go to 439
		itmp = irecaf
		call redfil (itmp,lun,ier)
		if (ier != 0) go to 439

		call wjfren (i,x,il)
		if (il == ihe || il == ihx) go to 10000
		if (i >=80) {
			call what(i)
                        go to 439
		}
		if (X < 0.0 || x > 0.99999 ) {
			write (ttyout,"(' Fraction out of range, re-enter')")
			go to 439
		}
		xafrac(iandx) = x   # areal fraction for component iandx


		write (ttyout,441) ititl, itchan
441		format (79('*'),/,
			' areal mix 1',/,5x,
			a, 5x, 'channels=',i6,/,79('-'))
		do j = 1, nchans {
			xaref(iandx,j) = data(j)  # areal mix spectrum 
		}
	    }
	    # now that we have read in the areaf fraction spectra, compute the final spectrum
	    totafrac =0.0
	    do iandx=1, numaref {

		totafrac = totafrac + xafrac(iandx)
	    }
	    if (totafrac > 1.0) {
		write (ttyout,450)
450		format (' Total of areal fractions is > 1, ',
			' re-enter ALL areal fractions',/)
		go to 435
	    }
	    totifrac=1.0-totafrac   # intimate mixture area fraction

	    write (ttyout,460) totifrac
460	    format (' Note: Areal fraction of the intimate mix component=',
			f9.5,//)

	    do j = 1, nchans {      # compute the final areal mix spectrum

		xatspec(j) = 0.0
		do iandx=1, numaref {

			xatspec(j) =  xatspec(j) + 
					xafrac(iandx) * xaref(iandx,j)   # add the areal mix components
		}
		r(j) = totifrac * r(j) + xatspec(j)  # final areal+intimate mix spectrum.
	    }
	}


# copy results fo specpr arrays for output
490	do i = 1,nchans {
		datac(i) = r(i)
		dataa(i) = wav(i)
	}
#
# write results
#
	irec = recnum + 1
500	write (ttyout, 501)
501	format ('Type in a TITLE',/,1x,39('-'),'I   ',
		'optional: plot scale goes here: w min max vmin vmax')
	call crtin
	i = 1
	call wjfren (i,x,il)
	if (i >= 80) {
		write (ttyout,502)
502		format ('NO TITLE entered, try again')
		go to 500
	}
	ititl = iopcon(1:40)

	# check for plot scale: wmina, wmaxa, bbnd, ubnd

	i=41
	call wjfren (i,x,il)
	if (i <  80) {

		if (il == ihw) {  # get wavelengths
			call wjfren (i,x,il)
			if (i >= 80) { 
				call what(i)
				write (ttyout,503)
503				format ('ERROR in plot scale wavelength range,',
					' reenter title and range')
				go to 500
			}
			wmina =x
			call wjfren (i,x,il)
			if (i >= 80) { 
				call what(i)
				write (ttyout, 503)
				go to 500
			}
			wmaxa =x

			if (wmaxa <= wmina) {

				write (ttyout, 504)
504				format ('ERROR  wmax <= wmin, ',
					'reenter title and range')
				go to 500
			}

			if (i >= 80) { 
				call what(i)
				write (ttyout, 505)
505				format ('ERROR in plot scale vertical range,',
					' reenter title and range')
				go to 500
			}
			# now get vertical range

			call wjfren (i,x,il)
			if (i >= 80) { 
				call what(i)
				write (ttyout, 505)
				go to 500
			}
			bbnd = x
			call wjfren (i,x,il)
			if (i >= 80) { 
				call what(i)
				write (ttyout, 505)
				go to 500
			}
			ubnd = x

			if (ubnd <= bbnd) {

				write (ttyout, 506)
506				format ('ERROR plot max <= min, ',
					'reenter title and range')
				go to 500
			}

		} else {       # just vertical range

			bbnd = x
			call wjfren (i,x,il)
			if (i >= 80) { 
				call what(i)
				write (ttyout, 505)
				go to 500
			}
			ubnd = x
			if (ubnd <= bbnd) {

				write (ttyout, 506)
				go to 500
			}

		}
	}

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
	if (imode == 2) {   # grain size iteration mode, give fit stats
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

#
# write history to text record.
#
	if (numaref > 0 || mmixflag > 0) {

		write (ttyout, 910)
910		format (//,' NOTE: HISTORY IS TOO LARGE TO FIT IN ',
			'THE HISTORY FIELD IN THE DATA RECORD',/)
	}

	call spctwri(il)   # write a text history record
	
2000  continue
3000  continue
10000	return
	end

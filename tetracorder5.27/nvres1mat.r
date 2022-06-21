	subroutine nvres1mat(imat, xdat1sp)

######	implicit integer*4 (i-n)
	implicit none

#ccc  name:         nvres1mat
#ccc  version date: 12/12/1994
#ccc  author(s): Roger N. Clark
#ccc  language: ratfor
#ccc
#ccc  short description: does nvres and bandmp over 1 feature for 1 material
#ccc                     including thresholding decisions.
#ccc
#ccc  algorithm description: See Clark et al, 1990, JPL AVIRIS Conf.
#ccc  system requirements: Unix
#ccc  subroutines called: many specpr routines, need specpr.a library
#ccc  argument list description: see below
#ccc  parameter description: see below
#ccc  common description: see below
#ccc  message files referenced: none
#ccc  internal variables: see below
#ccc  file description: see below
#ccc  user command lines: see below
#ccc  update information: see below
#ccc  NOTES:
#ccc
#ccc         imat = material number (see multmap.h
#ccc         xdat1sp = spectrum to analyze

	include "../specpr/src.specpr/common/spmaxes"   # max parameters, must be first

	include 	"../specpr/src.specpr/common/label1"
	include 	"../specpr/src.specpr/common/lbl3"
	include 	"../specpr/src.specpr/common/lbl4"
	include 	"../specpr/src.specpr/common/lbl7"
	include 	"../specpr/src.specpr/common/lundefs"
	include 	"../specpr/src.specpr/common/alphabet"
	include 	"../specpr/src.specpr/common/cmd"
	include 	"../specpr/src.specpr/common/lblg"
	include 	"../specpr/src.specpr/common/lblwav"
	include 	"../specpr/src.specpr/common/cmdarg"
	include 	"../specpr/src.specpr/common/dscrch"
	include 	"../specpr/src.specpr/common/ioftyp"
	include 	"../specpr/src.specpr/common/blank"
	include		"../specpr/src.specpr/common/lblvol"

# arrays for multiple materials

	include "multmap.h"

	include "tricube.h"

# basic tetracorder parameters

	include "tri1.h"

	real*4    sumf, sumd, sumfd, fit, yintcp
	real*4    conref, xfeat, xx, xbdxdn, xdn, xx2
	real*4    xxfact, xxr, zz1, zz2
	integer*4 imat, itmpfeat, intmp, ifeat, ixx2
	integer*4 ntmpnotmat, ntmpnotfeat, ii, tmpx1
	real*4    xdat1sp(maxpix)
	real*4    xdat1bsp(maxpix)
	real*4    xdatrsp(maxpix)
	real*4 a1, b1, xndvivegspec,xndviobs,xfactor,bdnorm


	#if (imat > 460) {
	#	write (ttyout,*) 'DEBUG: starting nvres1mat material ',imat,' pixel:',xel
	#}

	    sumf = 0.0
	    sumd = 0.0
	    sumfd= 0.0

	    do ifeat = 1, nfeat(imat) {

		#write (ttyout,*) 'DEBUG: nvres1mat called, ifeat=',ifeat,' imat=',imat,' enable=',ifeatenable(ifeat,imat)

		#write (ttyout,*) 'DEBUG: starting call nvres band analysis, feat',ifeat

		# Calculate observed continuum removed, band depth, 
		# k factor, slope, intercept and error fit
		# dataa = wavelengths

		# copy to temp arrays, because these arrays get modified by nvres

		if (ifeatenable(ifeat,imat) == 1 & cchans(1,ifeat,imat) > 0 & cchans(4,ifeat,imat) <=nchans) {

			do ii = cchans(1,ifeat,imat), cchans(4,ifeat,imat) {
				xdat1bsp(ii) = xdat1sp(ii)
				xdatrsp(ii)  = rratio(ii,imat)
			} 

			call nvres (dataa,rlbc(1,ifeat,imat),
					xdat1bsp,xdatrsp,
					cchans(1,ifeat,imat),
					cchans(2,ifeat,imat),
					cchans(3,ifeat,imat),
					cchans(4,ifeat,imat),
					emtogl,nchmin(ifeat,imat),
					nchmax(ifeat,imat),
					nftype(ifeat,imat),
					imgflg, xel,yel,obscrm,kfactr,
					bdepth, fit,
					slope,yintcp,datac,conref,a1,b1,
					xndvivegspec,xndviobs,xfactor,bdnorm)

			if ( tetmode < 2 ) {
				#write (ttyout,*) 'DEBUG: nvres1mat tetmode=', tetmode
				#write (ttyout,*) 'DEBUG: nvres1mat ifeat=',ifeat,' material',imat,' fit=',fit,' depth=',bdepth
				tmpx1 = 0  # dummy to do something
			}

		} else {
			if ( tetmode < 2 ) {
			   # needed to check bounds.  
			   if (cchans(1,ifeat,imat) < 1) {
				write (ttyout,*) 'DEBUG: nvres1mat ERROR 1st contin chan: cchans(1,',ifeat,',',imat,') =',cchans(1,ifeat,imat)
			   }
			   if (cchans(4,ifeat,imat) > nchans) {
				write (ttyout,*) 'DEBUG: nvres1mat ERROR last contin chan > nchans: cchans(1,',ifeat,',',imat,') =',cchans(1,ifeat,imat)
			   }
			   write (ttyout,*) 'DEBUG: nvres1mat output set to zero for ifeat=',ifeat,' material',imat
			}
		
			bdepth = 0.0
			fit    = 0.0
			bdnorm = 0.0
		}

		# nvres: want normalized band depth bdnorm

################################
#		write (ttyout,*) 'nvres1mat DEBUG: ',xndvivegspec,xndviobs,xfactor,bdnorm
#		write (ttyout,*) 'nvres1mat DEBUG: ',a1,b1,fit,bdepth
#		write (ttyout,*) 'nvres1mat DEBUG: ',conref,kfactr
#		do ii = cchans(1,ifeat,imat), cchans(4,ifeat,imat) {
#
#			write (ttyout,101) ii,dataa(ii),rlbc(ii,ifeat,imat),
#					xdat1sp(ii),rratio(ii,imat),
#					xdat1bsp(ii),xdatrsp(ii)
#101			format ('DEBUG:',i5,6(1x,f10.5))
#		}   # end DEBUG
################################

		# use < -1.e34 in case -1.23e34 doesn't get deleted
		#	point due to inexact floating point

		if (bdnorm < -1.e34 || 
				abs(bdnorm) < 0.0e-9 ||
				fit < -1.e34) {
			bdnorm = 0.0
			fit    = 0.0
		} else if (conref < zcontmn(ifeat,imat) ||
			   conref > zcontmx(ifeat,imat)) { # continuum
							# is beyond
							# limits
			bdnorm = 0.0
			fit    = 0.0

		}
			xdn = dln(ifeat,imat)          # weighted area

			ixx2=nftype(ifeat,imat)
			xfeat=real(ixx2)               # feature type

			if (ixx2 == 0) {               # this should never be the case
                                                       # because nftype = -1 or +1
							# do this test to prevent divide by 0
				xx2 = 0.0

			} else {
				xx2 = bdnorm/xfeat
			}


			xx=1.0                         # fit factor

			if (xx2 <= 0.1e-5) {  # fit factor=0
				xx=0.0                 # feature is
						       # negative of
						       # expected.

				# if feature is a diagnostic one,
				# and in this section does not exist,
				# then this material can not be present,
				# so zero it all out, and go to next
				# material.
				#
				# featimprt = 0  Optional
				# featimprt = 1  Weak, muts be present
				# featimprt = 2  Diagnostic
				# featimprt = 3  Must have diagnosdtic, unconditional
				#
				if (featimprt(ifeat,imat) == 2 || featimprt(ifeat,imat) == 3) {

					# set remaining features to zero
					do itmpfeat = ifeat, nfeat(imat) {
						zfit(itmpfeat,imat)   = 0.0
						zdepth(itmpfeat,imat) = 0.0
						zfd(itmpfeat,imat)    = 0.0
						zcompf(itmpfeat,imat) = 0.0
					}
					ofit(imat,xel)   = 0.0
					odepth(imat,xel) = 0.0
					ofd(imat,xel)    = 0.0

					return
				}

			}
			#write (ttyout,*) 'DEBUG:',fit,xdn,bdnorm,xbdxdn,xfeat,xx
			sumf = sumf + fit *xx* xdn     # sum fits
			xbdxdn = bdnorm * xdn * xfeat
			sumd = sumd + xbdxdn           # sum depths
			sumfd= sumfd+ xbdxdn * fit     # sum fit*depth
		

		zfit(ifeat,imat)   = fit
		zdepth(ifeat,imat) = bdnorm
		zfd(ifeat,imat)    = fit * bdnorm
		zcompf(ifeat,imat) = xx

		#write (ttyout,*) 'DEBUG: weighted sum analysis complete'

	    } # end ifeat do loop

	    # output fit, depth, f*d before decisions


	    ofit(imat,xel)   = sumf
	    odepth(imat,xel) = sumd
	    ofd(imat,xel)    = sumfd

	    # Check if values are below thresholds, or in fuzzy logic regime

            # first check the "all" thresholds so 
            #            we can skip others if they are done.

		##############   FITALL> nnn constraint

	    xxr = ofit(imat,xel)
	    zz1 = thrshfitall(1,imat)
	    zz2 = thrshfitall(2,imat)
	    #write (ttyout,*) 'DEBUG: nvres1mat: imat',imat,' obs fit: ',
	    #			xxr,' thrshfitall:',zz1, zz2
	    #write (lunresult,*) 'DEBUG: nvres1mat: imat',imat,' obs fit: ',
	    #			xxr,' thrshfitall:',zz1, zz2
	    if (xxr < zz1) {
			ofit(imat,xel) = 0.0
			odepth(imat,xel) = 0.0
			ofd(imat,xel) = 0.0
			go to 4050
	    } else if (xxr < zz2) { # fuzzy logic regime

			xxfact = (xxr - zz1) / (zz2 - zz1)
			ofit(imat,xel)   = ofit(imat,xel)   * xxfact
			odepth(imat,xel) = odepth(imat,xel) * xxfact
			ofd(imat,xel)    = ofd(imat,xel)    * xxfact
			#go to 4040  # if commented out, then cumulative

	    }

		##############   DEPTHALL> nnn constraint

	    xxr = abs(odepth(imat,xel))   # is abs needed?
	    zz1 = thrshdepthall(1,imat)
	    zz2 = thrshdepthall(2,imat)
	   #write (ttyout,*) 'DEBUG: nvres1mat: imat',imat,' obs depth: ',
           #                   xxr,' thresh:',zz1, zz2
	   #write (lunresult,*) 'DEBUG: nvres1mat: imat',imat,' obs depth: ',
           #                   xxr,' thresh:',zz1, zz2
	    if (xxr < zz1) {
			ofit(imat,xel) = 0.0
			odepth(imat,xel) = 0.0
			ofd(imat,xel) = 0.0
			#write (ttyout,*) 'DEBUG: nvres1mat: obs depth: ',xxr,
			#	' is less than threshold depth 1: ',zz1
			#write (lunresult,*) 'DEBUG: nvres1mat: obs depth: ',xxr,
			#	' is less than threshold depth 1: ',zz1
			go to 4050
	    } else if (xxr < zz2) { # fuzzy logic regime

			xxfact = (xxr - zz1) / (zz2 - zz1)
			ofit(imat,xel)   = ofit(imat,xel)   * xxfact
			odepth(imat,xel) = odepth(imat,xel) * xxfact
			ofd(imat,xel)    = ofd(imat,xel)    * xxfact
			#write (ttyout,*) 'DEBUG: nvres1mat: imat',imat,' obs depth: ',xxr,
			#	' is less than threshold depth 2: ',zz2,
                        #        ' reduction factor: ',xxfact
			#write (lunresult,*) 'DEBUG: nvres1mat: imat',imat,' obs depth: ',xxr,
			#	' is less than threshold depth 2: ',zz2,
                        #        ' reduction factor: ',xxfact
			#go to 4040  # if commented out, then cumulative

	    }

		##############   FDALL> nnn constraint

	    xxr = ofd(imat,xel)
	    zz1 = thrshfdall(1,imat)
	    zz2 = thrshfdall(2,imat)
	    if (xxr < zz1) {
			ofit(imat,xel) = 0.0
			odepth(imat,xel) = 0.0
			ofd(imat,xel) = 0.0
			go to 4050
	    } else if (xxr < zz2) { # fuzzy logic regime

			xxfact = (xxr - zz1) / (zz2 - zz1)
			ofit(imat,xel)   = ofit(imat,xel)   * xxfact
			odepth(imat,xel) = odepth(imat,xel) * xxfact
			ofd(imat,xel)    = ofd(imat,xel)    * xxfact
			#go to 4040  # if commented out, then cumulative

	    }

            # now check thresholds on specific items.

		##############   FIT> nnn constraint

	    xxr = ofit(imat,xel)
	    zz1 = thrshfit(1,imat)
	    zz2 = thrshfit(2,imat)
	    if (xxr < zz1) {
			ofit(imat,xel) = 0.0

	    } else if (xxr < zz2) { # fuzzy logic regime

			xxfact = (xxr - zz1) / (zz2 - zz1)
			ofit(imat,xel)   = ofit(imat,xel)   * xxfact
	    }

		##############   DEPTH> nnn constraint

	    xxr = abs(odepth(imat,xel))     # is abs needed?
	    zz1 = thrshdepth(1,imat)
	    zz2 = thrshdepth(2,imat)
	    if (xxr < zz1) {
			odepth(imat,xel) = 0.0

	    } else if (xxr < zz2) { # fuzzy logic regime

			xxfact = (xxr - zz1) / (zz2 - zz1)
			odepth(imat,xel) = odepth(imat,xel) * xxfact
	    }

		##############   DEPTH-FIT> nnn constraint

	    xxr = ofit(imat,xel)
	    zz1 = thrshdepthfit(1,imat)
	    zz2 = thrshdepthfit(2,imat)
	    if (xxr < zz1) {
			odepth(imat,xel) = 0.0

	    } else if (xxr < zz2) { # fuzzy logic regime

			xxfact = (xxr - zz1) / (zz2 - zz1)
			odepth(imat,xel) = odepth(imat,xel) * xxfact
	    }

		##############   FD> nnn constraint

	    xxr = ofd(imat,xel)
	    zz1 = thrshfd(1,imat)
	    zz2 = thrshfd(2,imat)
	    if (xxr < zz1) {
			ofd(imat,xel) = 0.0

	    } else if (xxr < zz2) { # fuzzy logic regime

			xxfact = (xxr - zz1) / (zz2 - zz1)
			ofd(imat,xel)    = ofd(imat,xel)    * xxfact
	    }

		##############   FD-FIT> nnn constraint

	    xxr = ofit(imat,xel)
	    zz1 = thrshfdfit(1,imat)
	    zz2 = thrshfdfit(2,imat)
	    if (xxr < zz1) {
			ofd(imat,xel) = 0.0

	    } else if (xxr < zz2) { # fuzzy logic regime

			xxfact = (xxr - zz1) / (zz2 - zz1)
			ofd(imat,xel)    = ofd(imat,xel)    * xxfact
	    }

		##############   FD-DEPTH> nnn constraint

	    xxr = odepth(imat,xel)
	    zz1 = thrshfddepth(1,imat)
	    zz2 = thrshfddepth(2,imat)
	    if (xxr < zz1) {
			ofd(imat,xel) = 0.0

	    } else if (xxr < zz2) { # fuzzy logic regime

			xxfact = (xxr - zz1) / (zz2 - zz1)
			ofd(imat,xel)    = ofd(imat,xel)    * xxfact
	    }

	    # check for NOT features  (initial nvres does not allow NOTs 12/94-RNC)

	    if (numnotfeat(imat) > 0 & ofit(imat,xel) > 0.0) { # have NOT features
		do intmp = 1, numnotfeat(imat) {
			ntmpnotmat = notmat(intmp,imat)
			ntmpnotfeat= notfeat(intmp,imat)
			# NOT feature is possible
			if (zdepth(ntmpnotfeat,ntmpnotmat) > 
					thrshdnot(intmp,imat) &
					zfit(ntmpnotfeat,ntmpnotmat) > 
					thrshfnot(intmp,imat)) {  # NOT is found

				ofit(imat,xel) = 0.0
				odepth(imat,xel) = 0.0
				ofd(imat,xel) = 0.0
				notflg(ntmpnotfeat,ntmpnotmat) = 1

			} else {
				notflg(ntmpnotfeat,ntmpnotmat) = 0
			}
		}
	    }

4050 	return
	end


	subroutine tp1cse (icubflg,xdat1sp,kcase)

######	implicit integer*4 (i-n)
	implicit none

#ccc  name:         tp1cse
#ccc  version date: 
#ccc  author(s): Roger N. Clark
#ccc  language: ratfor
#ccc
#ccc  short description: tetracorder/tricorder primary algorithm for cases:
#ccc                     does full spectral analysis of multiple features
#ccc                     and multiplae materials, groups, cases, etc.
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
#ccc         icubflg: flag to write cube (=1) or single spectrum (=0)
#ccc                  diagnostics 
#ccc         xdat1sp: the spectrum to analyze
#ccc         kcase  : which case to  analyze
#ccc---------------------------------------------------------------------

	include "../specpr/src.specpr/common/spmaxes"   # max parameters, must be first

	include 	"../specpr/src.specpr/common/label1"
	include 	"../specpr/src.specpr/common/lundefs"
	include 	"../specpr/src.specpr/common/alphabet"
	include 	"../specpr/src.specpr/common/blank"
	include 	"../specpr/src.specpr/common/dscrch"
	include 	"../specpr/src.specpr/common/lblg"

# arrays for multiple materials

	include "multmap.h"

	include "tricube.h"

# basic tetracorder parameters

	include "tri1.h"

	real*4    xdat1sp(imaxch)
	real*4    xdat2sp(imaxch)
	real*4    xx

	integer*4 cmdverbose   # function cmdverbose
	integer*4 icubflg, imat, jmat, ibest, isecnd

# RED 07/07/2008 Added declaration of i since no longer global in tricube.h
	integer*4 kcase, ii, i, idonothing

	character*24 tmptitle

######## now loop each material and feature

	if (nmatcse(kcase) < 1) {
		write (ttyout,*) 'ERROR: tp1cse: case called, but number of materials = 0'
		call what(-1)
		return
	}
	do ii = 1, nmatcse(kcase) {  # do all materials in one case

		imat = matcse(ii,kcase)

		#write (ttyout,*) 'tp1cse DEBUG: algorithm=',ialgorithm(imat),
		#		' mat:',imat,' case:',kcase

		if (ialgorithm(imat) == 0) {     # tetracorder/tricorder primary algorithm

			if (flguratio(imat) == 1) {   # perform URATIO

				do i = 1, nchans {
					if (xdat1sp(i) != -1.23e34 & 
						uratio(i,imat) != -1.23e34 &
						abs(uratio(i,imat)) > 0.1e-12) {

						xdat2sp(i) = xdat1sp(i) / uratio(i,imat)
					} else {
						xdat2sp(i) = -1.23e34
						
					}
					
				}

				call tp1mat(imat, xdat2sp)

			} else {

				call tp1mat(imat, xdat1sp)
			}

		} else if (ialgorithm(imat) == 1) {   # nvres  red edge pos algorithm

			#write (ttyout,*) 'tp1cse DEBUG: calling ',
			#		'nvres for mat ',imat,
			#		' case ',kcase

			call nvres1mat(imat, xdat1sp)

			#write (ttyout,*) 'tp1cse DEBUG: fit, d, fd:',
			#		ofit(imat,xel),odepth(imat,xel),
			#		ofd(imat,xel)

		} else {    # WARNING: should not get here, but set to 0 just in case

			ofit(imat,xel) = 0.0
			odepth(imat,xel) = 0.0
			ofd(imat,xel) = 0.0
		}


	} # end imat icase do loop

	# now make decisions for each group as to best material

		ibest  = matcse(1,kcase)
		isecnd = matcse(1,kcase)
		##write (ttyout,*) 'DEBUG: tp1all, nmatcse= ',nmatcse(kcase),
		##			' case:',kcase
		do imat = 1, nmatcse(kcase) {  # find best fit

			jmat = matcse(imat,kcase)
			if (ofit(jmat,xel) > ofit(ibest,xel)) {
				isecnd= ibest
				ibest = jmat
			}
		}
		csebest(kcase)  = ibest
		csesecnd(kcase) = isecnd
		##write (ttyout,*) 'DEBUG: tp1cse, best ',ibest,
		##			' group:',kcase
		##			' second:',isecnd

			# now compare best and second best according to rules

			if (mclass(isecnd) < mclass(ibest)) {    # 2nd is better class

				xx = ofit(ibest,xel) - dclass(isecnd)
				if (xx < ofit(isecnd,xel)) {

					ibest = isecnd
					csebest(kcase)  = ibest
					##write (ttyout,*) 'DEBUG: tp1cse, second is really best',
					##		'because of class difference'
					##write (ttyout,*) 'DEBUG: tp1cse, new best ',ibest,
					##			' case:',kcase

				}

			}

		
	# Write diagnostic output at user specified intervals
	if (icubflg == 1) {   # image cube mode
		if (xel == dx2 & mod(yel,nth) == 0) {

		#	call wrtcrdrout (lunresult, ttyout)

		# if fit or depth and f*d ar too low, say none
			tmptitle = otitle(ibest)(1:24)
			if (ofit(ibest,xel) < 0.00001 ||
                                odepth(ibest,xel) < 0.00001 &&
                                ofd(ibest,xel) < 0.00001) {

				tmptitle = 'none                    '
			}

			write (ttyout,111) kcase, casename(kcase),
                                ibest,ofit(ibest,xel),
                                odepth(ibest,xel),
                                ofd(ibest,xel),
                                tmptitle
111                     format ('               case:',
                                i6,1x,a,1x,i6,1x,f7.3,2x,
                                f7.3,1x,f7.3,1x,a)
		}
	} else {              # single spectrum mode
		#call wrtspcrdrout (diaflg, inamr, ifils, ititl,
		#		xel, lunresult, ttyout)
			#write (ttyout,111) kcase,
                        #        ibest,ofit(ibest,xel),
                        #        odepth(ibest,xel),
                        #        ofd(ibest,xel),

                        #        otitle(ibest)(1:24)
		#write (ttyout,*) 'DEBUG: case=', kcase,' caseenable=', caseenable(kcase)

		#if (kcase == 1) {
		#	write (ttyout,*) 'DEBUG: case 1: ',casename(kcase), otitle(ibest)(1:32),
                #                        'fit=',ofit(ibest,xel),
                #                        'depth=',odepth(ibest,xel),
                #                        'f*d=',ofd(ibest,xel)
		#}

		if (caseenable(kcase) != 0 & nmatcse(kcase) != 0) {
			if (ofit(ibest,xel) < 0.1e-4 || odepth(ibest,xel) < 0.1e-4 && ofit(ibest,xel)  < 1.1) {

				if (caseenable(kcase) > 0 ) {
					write (ttyout, 2071) kcase, casename(kcase)
2071					format ('     cse',i2,3x,a12,'  none        ')
				
					#old write (lunresult,207) inamr, ifils, ititl,kcase
					write (lunresult, 2071) kcase, casename(kcase)
207					format (1x,a,i6,1x,a,'     cse',i2,3x,a12,' none         ')
				}
			} else if (ofit(ibest,xel) >= 0.1e-4 && ofit(ibest,xel)  < 1.1) {

				# was: write (ttyout,2081) kcase, casename(kcase), otitle(ibest)(1:32),
				write (ttyout,2081) kcase, casename(kcase), mfile(ibest),
					ofit(ibest,xel),
					odepth(ibest,xel),
					ofd(ibest,xel)
2081				format ('     cse',i2,3x,a12,5x,'  MATCHES:  ',a,
					' Fit=',f7.4,' D=',f8.4,' FD=',f8.4)
208				format ('Spectrum: ',a,i7,2x,a,
					'MATCHES ',a,
					' Fit=',f7.4,' D=',f8.4,
					' FD=',f8.4)
				write (lunresult,2081) kcase, casename(kcase), otitle(ibest)(1:32),
					ofit(ibest,xel),
					odepth(ibest,xel),
					ofd(ibest,xel)
				###########  future do sound
			  	#if (dosound(ibest) == 1 && soundenable == 1) {    # output answer as sound

				#	soundstring= 'tetracordersound ' // sound1fil(ibest) // ' ' // char(0)
				#	#write (ttyout,*) 'DEBUG: ', soundstring

				#	call system (soundstring)
			  	#}
			} else {
				idonothing = 0  # do no output
			}
		}
	}

	# now that best is found, others are zero
	if (nmatcse(kcase) > 1) {
		do imat = 1, nmatcse(kcase) {  # check best fit

			jmat = matcse(imat,kcase)
			if (jmat == csebest(kcase)) next
			ofit(jmat,xel)   = 0.0
			odepth(jmat,xel) = 0.0
			ofd(jmat,xel)    = 0.0
		}
	}

	# add material found to pixel statistics
	ibest = csebest(kcase)
	if (nint(ofit(ibest,xel)*qfscal(ibest)) > 0) {
		statsmapfit(ibest) = statsmapfit(ibest) + 1
	}
	if (nint(odepth(ibest,xel)*bdscal(ibest)) > 0) {
		statsmapdepth(ibest) = statsmapdepth(ibest) + 1
	}
	if (nint(ofd(ibest,xel)*bdscal(ibest)) > 0) {
		statsmapfd(ibest) = statsmapfd(ibest) + 1
	}

	#write (ttyout,*) 'DEBUG: best fit analysis complete'

	# sum the fit to get the mean fit per material

	if (ofit(ibest,xel) > 0.0 & odepth(ibest,xel) > 0.0) {

		fitmean(ibest) = fitmean(ibest) + 
				dble(ofit(ibest,xel))
	}

	return
	end

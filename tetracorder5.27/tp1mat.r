	subroutine tp1mat(imat,xdat1sp)

######	implicit integer*4 (i-n)
	implicit none

#ccc  name:         tp1mat
#ccc  version date: 12/12/1994
#ccc  author(s): Roger N. Clark
#ccc  language: ratfor
#ccc
#ccc  short description: does bandmp over multiple features for 1 material
#ccc                     including thresholding and NOT decisions.
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
	real*4    xxfact, xfeat, xx, xxr, zz1, zz2, zz3, zz4
	real*4    xbdxdn, xdn, xx1, xx2
	real*4    conref, conrefl, conrefr
	integer*4 imat, itmpfeat, intmp, ifeat, i1, i2, fitvalid
	integer*4 ntmpnotmat, ntmpnotfeat, ntmpfrn, jchn, ijfeat
	real*4    xdat1sp(maxpix), xtmpdepth, xtmp1, xtmp2, cwv(4)

	real*4    rfcon(maxpix)   # computet curved continuum


	if (imatenable(imat) == 0) {   # material not enabled, so do not open a file

		#if (cmdverbose(-1) <= 1) write (ttyout,*) 'NOTE: material',imat,' is disabled, tp1mat: not analyzing'
		return
	}

	#write (ttyout,*) 'DEBUG: starting material ',imat

	    sumf = 0.0
	    sumd = 0.0
	    sumfd= 0.0

	    do ifeat = 1, nfeat(imat) {

		
		#write (ttyout,*) 'DEBUG: starting call band analysis, feat',ifeat
		#if (imat == 146 | imat == 147 | imat == 148 | imat > 460) {
		#	write (ttyout,*) ' '
		#	write (ttyout,*) 'DEBUG: starting tp1mat, feat',ifeat,'  imat=',imat, otitle(imat)
		#	# write (ttyout,*) 'DEBUG: tp1mat, pixel(x,y):', xel, yel
		#	write (ttyout,*) 'DEBUG: tp1mat: feature',ifeat,' contin type=',icontype(ifeat,imat)
		#	write (ttyout,*) 'DEBUG: tp1mat: feature',ifeat,' feature enable=',ifeatenable(ifeat,imat)
		#}

		#  curved continuum:

		if ( icontype(ifeat,imat) == 1 && ifeatenable(ifeat,imat) == 1) {      ######## curved continuum, enabled feature
 
			do jchn = 1, 4 {   # Put the 4 continuum wavelengths into an array for passing

				cwv(jchn) = cvwav(jchn, ifeat,imat)
			}
			#write (ttyout,*) 'DEBUG: tp1mat  cwv= ', cwv(1), cwv(2), cwv(3), cwv(4),
			#		' ifeat=',ifeat,'    imat=',imat

			#write (ttyout,*) 'DEBUG: tp1mat  cvwav= ', cvwav(1, ifeat,imat),
			#		cvwav(2, ifeat,imat), cvwav(3, ifeat,imat),
			#		cvwav(4, ifeat,imat),
			#		' ifeat=',ifeat,'    imat=',imat,'  xel=',xel

			##### NOTE: cvwav is computed BEFORE this subroutine, in getifeat

			call bandmpcv (dataa,rlbc(1,ifeat,imat),
				xdat1sp,
				icurvchans(1,ifeat,imat),
				icurvchans(2,ifeat,imat),
				icurvchans(3,ifeat,imat),
				icurvchans(4,ifeat,imat),
				icurvchans(5,ifeat,imat),
				icurvchans(6,ifeat,imat),
				icurvchans(7,ifeat,imat),
				icurvchans(8,ifeat,imat),
				cwv,
				emtogl,nchmin(ifeat,imat),
				nchmax(ifeat,imat),
				nftype(ifeat,imat),
				imgflg, xel,yel,obscrm,rfcon,
				kfactr, bdepth, fit,
				slope,yintcp,datac,conref,
				conrefl,conrefr)

				#if (imat > 460) {
				#	write (ttyout,*) 'DEBUG: tp1mat curvc: imat=',imat,' fit=', fit,' depth=',bdepth
				#}
			if (fit > 0.0 & fit < 1.1) {   # check for for NaN
				fitvalid = 1
			} else {

				#if (imat > 460) {
				#	write (ttyout,*) 'DEBUG: tp1mat curvc: fit=invalid  imat=',imat,'reset to 0'
				#}
				fitvalid = 0
				bdepth = 0.0
				fit    = 0.0
				go to 2000    # done with this feature
			}
			if (xel == 300 ) {   # testing curved continuum 3.5 micron region 3/2015 sample = 300
			   if (imat > 65 & imat < 72 &  yel >100 & yel < 120 ) {   # testing curved continuum 3.5 micron region 3/2015

				write(ttyout, 2044)  xel, yel, imat, ifeat,
						icurvchans(1,ifeat,imat),
						icurvchans(2,ifeat,imat),
						icurvchans(3,ifeat,imat),
						icurvchans(4,ifeat,imat),
						icurvchans(5,ifeat,imat),
						icurvchans(6,ifeat,imat),
						icurvchans(7,ifeat,imat),
						icurvchans(8,ifeat,imat),
						cwv(1),cwv(2),cwv(3),cwv(4),
						kfactr,bdepth, fit,
						slope,yintcp,conref,conrefl,
						conrefr

2044					format (' DEBUG: x,y=', i5, 1x, i5,' imat=', i5,
						' ifeat=', i5,
						' icurvchans=', 8(i5, 1x), /,
						' cwv=', f11.5, 2x, f11.5, 2x, f11.5, 2x, f11.5, /,
						' kfactr=', f15.5, 2x, ' bdepth=', f15.5, 2x,
						' fit=', f15.5, 2x,/,
						' slope=', f11.5, 2x, ' yintcp=', f11.5, 2x, /,
						' conref=', f11.5, 2x, ' conrefl=', f11.5, 2x, 
						' conrefr=', f11.5, 
						// )
					write (ttyout,*) ' kfactr, bdepth fit=', kfactr,bdepth, fit

					#write (ttyout,*) 'rfcon(380:450)=', rfcon(380:450)

			   }
			}

		} else if (ifeatenable(ifeat,imat) == 1) {                   ############# linear continuum, enabled feature

			# Calculate observed LINEAR continuum removed, band depth, 
			# k factor, slope, intercept and error fit
			# dataa = wavelengths
			call bandmp (dataa,rlbc(1,ifeat,imat),
				xdat1sp,
				cchans(1,ifeat,imat),
				cchans(2,ifeat,imat),
				cchans(3,ifeat,imat),
				cchans(4,ifeat,imat),
				emtogl,nchmin(ifeat,imat),
				nchmax(ifeat,imat),
				nftype(ifeat,imat),
				imgflg, xel,yel,obscrm,kfactr,
				bdepth, fit,
				slope,yintcp,datac,conref,
				conrefl,conrefr)
		} else {                         ##################   feature was not enabled
			bdepth = 0.0
			fit    = 0.0
			go to 2000    # done with this feature
		}

		#if (imat == 146 | imat == 147 | imat == 148 | imat > 460) {
		#	write (ttyout,*) 'DEBUG: bandmp call complete, fit=',fit,' bdepth=', bdepth
		#	write (ttyout,*) 'DEBUG: conrefl,conrefr=',conrefl,conrefr
		#}

		###                added fit > 1.1 to check for NaN  2019-10-12 - RNC
		if ( fit != fit ) {    # NaN check

				bdepth = 0.0
				fit    = 0.0
				#if (imat > 460) {
				#	write (ttyout,*) 'DEBUG: tp1mat: fit=NaN  imat=',imat,'reset to 0'
				#}
				go to 2000    # done with this feature
		}
		fitvalid = 0
		if (fit > 0.0 & fit < 1.1) {
			fitvalid = 1
		}
		if (bdepth == -1.23e34 | fitvalid == 0 |
			abs(bdepth) < 0.1e-6 | 
			conref < zcontmn(ifeat,imat) |
			conref > zcontmx(ifeat,imat) |
			conrefl < zcontlmn(ifeat,imat) |
			conrefl > zcontlmx(ifeat,imat) |
			conrefr < zcontrmn(ifeat,imat) |
			conrefr > zcontrmx(ifeat,imat) ) { # continuum
							# is beyond
							# limits
				bdepth = 0.0
				fit    = 0.0
		    #if (imat == 146 | imat == 147 | imat == 148 | imat > 460) {
		#	write (ttyout,*) 'DEBUG: tp1mat,ZEREOD depthL feat',ifeat,'  imat=',imat
		    #}
				go to 2000    # done with this feature

		}

		##############   lct/rct> n1 n2 constraint
      #================================================================

		zz2 = zcontlgtr(2,ifeat,imat)
		if (zz2 > 0.1e-7) {   # test if left > right
                                                        # in correct amount

                    # the two zcontlgtr values are the fuzzy logic limits

			xtmp1 = conrefr
			xtmp2 = conrefl
			
			if (xtmp1 < 0.1e-6) xtmp1 = 0.1e-6  # range check
			if (xtmp2 > 0.1e20) xtmp2 = 0.1e20  # range check
			xxr = xtmp2/xtmp1
			zz1 = zcontlgtr(1,ifeat,imat)
			if (xxr < zz1) {
							# not > enough, so
							# zero fit, depth
				bdepth = 0.0
				fit    = 0.0
				#if (imat == 146 | imat == 147 | imat == 148) {
				#	write (ttyout,*) 'DEBUG: point 1'
				#}
				go to 2000    # done with this feature

			} else if (xxr < zz2) { # fuzzy logic regime

				xxfact = (xxr - zz1) / (zz2 - zz1)

				bdepth =  bdepth * xxfact
				fit    = fit * xxfact
			}

		}
		#if (imat == 146 | imat == 147 | imat == 148) {
		# 	write (ttyout,*) 'DEBUG: after zcontlgtr: fit=',fit,' depth=',bdepth,' imat=',imat
		#}

		##############   rct/lct> n1 n2 constraint

		zz2 = zcontrgtl(2,ifeat,imat)
		if (zz2 > 0.1e-7) {   # test if right > left
                                                        # in correct amount
			xtmp1 = conrefl
			xtmp2 = conrefr
			if (xtmp1 < 0.1e-6) xtmp1 = 0.1e-6  # range check
			if (xtmp2 > 0.1e20) xtmp2 = 0.1e20  # range check
			xxr = xtmp2/xtmp1
			zz1 = zcontrgtl(1,ifeat,imat)
			if (xxr < zz1) {
							# not > enough, so
							# zero fit, depth
				bdepth = 0.0
				fit    = 0.0
				#if (imat == 146 | imat == 147 | imat == 148) {
				#	write (ttyout,*) 'DEBUG: point 2'
				#}
				go to 2000    # done with this feature

			} else if (xxr < zz2) { # fuzzy logic regime

				xxfact = (xxr - zz1) / (zz2 - zz1)

				bdepth =  bdepth * xxfact
				fit    = fit * xxfact
			}
		}
		#if (imat == 146 | imat == 147 | imat == 148) {
		#	write (ttyout,*) 'DEBUG: after zcontrgtl: fit=',fit,' depth=',bdepth
		#}

		##############   zrcbblcgt: (rc-bb)/(lc-bb) > fuzz(1,2)
			# -------------------------------------------------
			# conref*(1.0-bdepth) = reflectance at band bottom = bb
			# rc = right continuum
			# lc = left continuum


		zz2 = zrcbblcgt(2,ifeat,imat)
		if (zz2 > -0.1e+7) {   # test if right > left
                                                        # in correct amount
			xtmp2 = conrefr - conref*(1.0-bdepth)
			xtmp1 = conrefl - conref*(1.0-bdepth)

			if (abs(xtmp1) < 0.1e-6) xtmp1 = 0.1e-6  # range check
			if (xtmp2 > 0.1e20) xtmp2 = 0.1e20  # range check
			xxr = xtmp2/xtmp1
			zz1 = zrcbblcgt(1,ifeat,imat)
			if (xxr < zz1) {
							# not > enough, so
							# zero fit, depth
				bdepth = 0.0
				fit    = 0.0
				#if (imat == 146 | imat == 147 | imat == 148) {
				#	write (ttyout,*) 'DEBUG: point 3'
				#}
				go to 2000    # done with this feature

			} else if (xxr < zz2) { # fuzzy logic regime

				xxfact = (xxr - zz1) / (zz2 - zz1)

				bdepth =  bdepth * xxfact
				fit    = fit * xxfact
			}
		}
		#if (imat == 146 | imat == 147 | imat == 148) {
		#	write (ttyout,*) 'DEBUG: after zrcbblcgt: fit=',fit,' depth=',bdepth
		#}

		##############   zrcbblclt: (rc-bb)/(lc-bb) < fuzz(1,2)
			# -------------------------------------------------
			# conref*(1.0-bdepth) = reflectance at band bottom = bb
			# rc = right continuum
			# lc = left continuum


		zz2 = zrcbblclt(1,ifeat,imat)
		if (zz2 < 0.1e+8) {   # test if right < left
                                                        # in correct amount
			xtmp2 = conrefr - conref*(1.0-bdepth)
			xtmp1 = conrefl - conref*(1.0-bdepth)

			if (abs(xtmp1) < 0.1e-6) xtmp1 = 0.1e-6  # range check
			if (xtmp2 > 0.1e20) xtmp2 = 0.1e20  # range check
			xxr = xtmp2/xtmp1
			zz1 = zrcbblclt(2,ifeat,imat)
			#write(*,*)'********DEBUG: tp1mat zrcbblclt ifeat imat:',ifeat, imat
			#write(*,*)'DEBUG: tp1mat zrcbblclt cont:',conrefl,conref*(1.0-bdepth),conrefr
			#write(*,*)'DEBUG: tp1mat zrcbblclt xxr, zz1, zz2:',xxr, zz1, zz2
			if (xxr > zz2) {
							# not > enough, so
							# zero fit, depth
				bdepth = 0.0
				fit    = 0.0
				#if (imat == 146 | imat == 147 | imat == 148) {
				#	write (ttyout,*) 'DEBUG: point 4'
				#}
				go to 2000    # done with this feature

			} else if (xxr > zz1) { # fuzzy logic regime

				xxfact = (xxr - zz2) / (zz1 - zz2)

				bdepth =  bdepth * xxfact
				fit    = fit * xxfact
			}
		}
		#if (imat == 146 | imat == 147 | imat == 148) {
		#	write (ttyout,*) 'DEBUG: after zrcbblclt: fit=',fit,' depth=',bdepth,' imat=',imat
		#}

		##############   zlcbbrcgt: (lc-bb)/(rc-bb) > fuzz(1,2)
			# -------------------------------------------------
			# conref*(1.0-bdepth) = reflectance at band bottom = bb
			# rc = right continuum
			# lc = left continuum


		zz2 = zlcbbrcgt(2,ifeat,imat)
		if (zz2 > -0.1e+7) {   # test if right > left
                                                        # in correct amount
			xtmp1 = conrefr - conref*(1.0-bdepth)
			xtmp2 = conrefl - conref*(1.0-bdepth)

			if (abs(xtmp1) < 0.1e-6) xtmp1 = 0.1e-6  # range check
			if (xtmp2 > 0.1e20) xtmp2 = 0.1e20  # range check
			xxr = xtmp2/xtmp1
			zz1 = zlcbbrcgt(1,ifeat,imat)
			#write(*,*)'DEBUG: tp1mat zlcbbrcgt imat:',imat
			#write(*,*)'DEBUG: tp1mat zlcbbrcgt cont:',conrefl,conref*(1.0-bdepth),conrefr
			if (xxr < zz1) {
							# not > enough, so
							# zero fit, depth
				bdepth = 0.0
				fit    = 0.0
				go to 2000    # done with this feature

			} else if (xxr < zz2) { # fuzzy logic regime

				xxfact = (xxr - zz1) / (zz2 - zz1)

				bdepth =  bdepth * xxfact
				fit    = fit * xxfact
			}
		}
		#if (imat == 146 | imat == 147 | imat == 148) {
		#	write (ttyout,*) 'DEBUG: after zlcbbrcgt: fit=',fit,' depth=',bdepth,' imat=',imat
		#}

		##############   zrcbblclt: (lc-bb)/(rc-bb) < fuzz(1,2)
			# -------------------------------------------------
			# conref*(1.0-bdepth) = reflectance at band bottom = bb
			# rc = right continuum
			# lc = left continuum


		zz2 = zlcbbrclt(1,ifeat,imat)
		#write(*,*)'DEBUG: tp1mat zlcbbrclt zz2:', zz2
		#if (imat == 146 | imat == 147 | imat == 148) {
		#	write (ttyout,*) 'DEBUG: tp1mat: got to zlcbbrclt'
		#}
		if (zz2 < 0.1e+8) {   # test if right < left
                                                        # in correct amount
			xtmp1 = conrefr - conref*(1.0-bdepth)
			xtmp2 = conrefl - conref*(1.0-bdepth)

			if (abs(xtmp1) < 0.1e-6) xtmp1 = 0.1e-6  # range check
			if (xtmp2 > 0.1e20) xtmp2 = 0.1e20  # range check

			xxr = xtmp2/xtmp1
			#write(*,*)'********DEBUG: tp1mat zlcbbrclt ifeat imat:',ifeat, imat
			#write(*,*)'DEBUG: tp1mat zlcbbrclt cont:',conrefl,conref*(1.0-bdepth),conrefr
			#write(*,*)'DEBUG: tp1mat zlcbbrclt xxr, zz1, zz2:',xxr, zz1, zz2
			zz1 = zlcbbrclt(2,ifeat,imat)
			if (xxr > zz2) {
							# not > enough, so
							# zero fit, depth
				bdepth = 0.0
				fit    = 0.0
				go to 2000    # done with this feature

			} else if (xxr > zz1) { # fuzzy logic regime

				xxfact = (xxr - zz2) / (zz1 - zz2)

				bdepth =  bdepth * xxfact
				fit    = fit * xxfact
			}
		}
		#if (imat == 146 | imat == 147 | imat == 148) {
		#	write (ttyout,*) 'DEBUG: after zlcbbrclt: fit=',fit,' depth=',bdepth,' imat=',imat
		#}

		##############   zrtimesbd: r*bd > fuzz(1,2) # do abs(bd) for positive feats
			# -------------------------------------------------
			# conref*bdepth = r*bd or actusally r*abs(bd)
			# r*abs(bd) added tetracorder 4.1 8/29/2003 - R. N. Clark

		zz2 = zrtimesbd(2,ifeat,imat)
		if (zz2 > 0.11e-9) {   # test if r*bd > threshold

                    # the two zrtimesbd values are the fuzzy logic limits

			xtmp1 = conref*abs(bdepth)
			
			zz1 = zcontlgtr(1,ifeat,imat)
			if (xtmp1 < zz1) {
							# not > enough, so
							# zero fit, depth
				bdepth = 0.0
				fit    = 0.0
				#if (imat == 146 | imat == 147 | imat == 148) {
				#	write (ttyout,*) 'DEBUG: point 1'
				#}
				go to 2000    # done with this feature

			} else if (xtmp1 < zz2) { # fuzzy logic regime

				xxfact = (xtmp1 - zz1) / (zz2 - zz1)

				bdepth =  bdepth * xxfact
				fit    = fit * xxfact
			}

		}
		#if (imat == 146 | imat == 147 | imat == 148) {
		#	write (ttyout,*) 'DEBUG: after zrtimesbd: fit=',fit,' depth=',bdepth,' imat=',imat
		#}


		# this is the skip point if bdepth and fit are set to 0.0


2000		if ( ifeatenable(ifeat,imat) == 1) {      ######## 
			xdn = dln(ifeat,imat)          # weighted area
			xfeat=real(nftype(ifeat,imat)) # feature type, =1 absorption, =-1 emission
			#if (imat == 146 | imat == 147 | imat == 148 | imat > 460) {
			#	write (ttyout,*) 'DEBUG: tp1mat past label 2000: xdn=',
			#			xdn,' feature type=',nftype(ifeat,imat)
			#}

			xx=1.0                         # fit factor
			if (bdepth/xfeat <= 0.1e-5) {  # fit factor=0
				xx=0.0                 # feature is
						       # negative of
						       # expected.

				# if feature is a diagnostic one,
				# featimprt = 2, or 3
				# and in this section does not exist,
				# then this material can not be present,
				# so zero it all out, and go to next
				# material.
				# same with weak features:featimprt=1
				#
				# featimprt = 0  Optional
				# featimprt = 1  Weak, muts be present
				# featimprt = 2  Diagnostic
				# featimprt = 3  Must have diagnosdtic, unconditional
				#

				if (featimprt(ifeat,imat) > 0) {

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
		} else {                 # feature is not enabled
			xdn    = 0.0
			xx     = 0.0
			fit    = 0.0
			xfeat  = 0.0
			bdepth = 0.0
		}

		if (featimprt(ifeat,imat) != 1) {      # not a weak feature
                                                       #     so don't include
						       #     in sums

			sumf = sumf + fit *xx* xdn     # weighted sum fits
			xbdxdn = bdepth * xdn * xfeat
			sumd = sumd + xbdxdn           # weighted sum depths
			sumfd= sumfd+ xbdxdn * fit     # weighted sum fit*depth
		}
		

		zfit(ifeat,imat)   = fit
		zdepth(ifeat,imat) = bdepth
		zfd(ifeat,imat)    = fit * bdepth
		zcompf(ifeat,imat) = xx

		#if (imat == 146 | imat == 147 | imat == 148 | imat > 460) {
		#      write (ttyout,*) 'DEBUG: weighted sum analysis complete, imat=',imat
		#}


	    } # end ifeat do loop

	    # output fit, depth, f*d before decisions

		#if (imat == 146 | imat == 147 | imat == 148 | imat > 460) {
		#if (imat <= 23 ) {
		#	write (ttyout,*) 'DEBUG: tp1mat summed results before constraints ',
		#			imat,': weighted fit=',sumf,' weighted depth=',sumd
		#}

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
	    #write (ttyout,*) 'DEBUG: tp1mat: imat',imat,' obs fit: ',
	    #			xxr,' thrshfitall:',zz1, zz2
	    #write (lunresult,*) 'DEBUG: tp1mat: imat',imat,' obs fit: ',
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
	    #if (imat == 146 | imat == 147 | imat == 148) {
	    #if (imat <= 23 ) {
	#	write (ttyout,*) 'DEBUG: tp1mat at imat=',imat
	#	write (ttyout,*) 'DEBUG: after FITALL: fit=',ofit(imat,xel),' depth=',odepth(imat,xel)
	    #}

		##############   DEPTHALL> nnn constraint

	    xxr = abs(odepth(imat,xel))   # is abs needed?
	    zz1 = thrshdepthall(1,imat)
	    zz2 = thrshdepthall(2,imat)
	   #write (ttyout,*) 'DEBUG: tp1mat: imat',imat,' obs depth: ',
           #                   xxr,' thresh:',zz1, zz2
	   #write (lunresult,*) 'DEBUG: tp1mat: imat',imat,' obs depth: ',
           #                   xxr,' thresh:',zz1, zz2
	    if (xxr < zz1) {
			ofit(imat,xel) = 0.0
			odepth(imat,xel) = 0.0
			ofd(imat,xel) = 0.0
			#write (ttyout,*) 'DEBUG: tp1mat: obs depth: ',xxr,
			#	' is less than threshold depth 1: ',zz1
			#write (lunresult,*) 'DEBUG: tp1mat: obs depth: ',xxr,
			#	' is less than threshold depth 1: ',zz1
			go to 4050
	    } else if (xxr < zz2) { # fuzzy logic regime

			xxfact = (xxr - zz1) / (zz2 - zz1)
			ofit(imat,xel)   = ofit(imat,xel)   * xxfact
			odepth(imat,xel) = odepth(imat,xel) * xxfact
			ofd(imat,xel)    = ofd(imat,xel)    * xxfact
			#write (ttyout,*) 'DEBUG: tp1mat: imat',imat,' obs depth: ',xxr,
			#	' is less than threshold depth 2: ',zz2,
                        #        ' reduction factor: ',xxfact
			#write (lunresult,*) 'DEBUG: tp1mat: imat',imat,' obs depth: ',xxr,
			#	' is less than threshold depth 2: ',zz2,
                        #        ' reduction factor: ',xxfact
			#go to 4040  # if commented out, then cumulative

	    }
	    #if (imat == 146 | imat == 147 | imat == 148) {
		#write (ttyout,*) 'DEBUG: after DEPTHALL: fit=',ofit(imat,xel),' depth=',odepth(imat,xel),' imat=',imat
	    #}

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
	    #if (imat == 146 | imat == 147 | imat == 148) {
	#	write (ttyout,*) 'DEBUG: after FDALL: fit=',ofit(imat,xel),' depth=',odepth(imat,xel),' imat=',imat
	#    }


      #================================================================
		##############   feature ratio constraints

	    if (nfeatratio(imat) > 0 & ofit(imat,xel) > 0.0) {
		do intmp = 1, nfeatratio(imat) {

		    i1  = featidratio (1,intmp,imat)
		    i2  = featidratio (2,intmp,imat)
		    xx1 = zdepth(i1,imat)
		    xx2 = zdepth(i2,imat)
		    if (abs(xx2) < 0.1e-14) xx2 = 0.1e-14
		    xxr = xx1/xx2   # feature ratio

		    zz1 = featratio(1,intmp,imat)
		    zz2 = featratio(2,intmp,imat)
		    if (xxr < zz1) {                # feat ration < min
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

		    zz3 = featratio(3,intmp,imat)
		    zz4 = featratio(4,intmp,imat)
		    if (xxr > zz4) {
				ofit(imat,xel) = 0.0
				odepth(imat,xel) = 0.0
				ofd(imat,xel) = 0.0
				go to 4050
		    } else if (xxr > zz3) { # fuzzy logic regime
	
				xxfact = (xxr - zz3) / (zz4 - zz3)
				ofit(imat,xel)   = ofit(imat,xel)   * xxfact
				odepth(imat,xel) = odepth(imat,xel) * xxfact
				ofd(imat,xel)    = ofd(imat,xel)    * xxfact
				#go to 4040  # if commented out, then cumulative

		    }
		}
	    }
	    #if (imat == 146 | imat == 147 | imat == 148) {
	#	write (ttyout,*) 'DEBUG: after feature ratio: fit=',ofit(imat,xel),' depth=',odepth(imat,xel)
	#    }

      #================================================================
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

	    #if (imat == 146 | imat == 147 | imat == 148) {
	    #if (imat <= 23 ) {
	#	write (ttyout,*) 'DEBUG: after individual constraints: imat= ',imat,'  fit=',ofit(imat,xel),' depth=',odepth(imat,xel)
	    #}

	    # check for NOT features

4040	    if (numnotfeat(imat) > 0 & ofit(imat,xel) > 0.0) { # have NOT features
		do intmp = 1, numnotfeat(imat) {
			ntmpnotmat = notmat(intmp,imat)
			ntmpnotfeat= notfeat(intmp,imat)
			ntmpfrn    = notfrn(intmp,imat)
			# NOT feature is possible
			#write (6,*) "DEBUG: not feature:",ntmpfrn,
			#		" imat=",imat," not feat#",intmp
			if (ntmpfrn == 0) {    # absolute depth

				xtmpdepth = abs(zdepth(ntmpnotfeat,ntmpnotmat))

			} else {               # relative depth

						# check if denominator is zero
						#  use 0.1e-12 as zero

				#write (6,*) "DEBUG: relative not feature:",
				#	ntmpfrn," imat=",imat," not feat#",intmp

				xtmp1 = abs(zdepth(ntmpfrn,imat))
				if (xtmp1 > 0.1e-12) {

					xtmpdepth = abs(zdepth(ntmpnotfeat,
							ntmpnotmat)) / xtmp1

				} else {
					xtmpdepth = abs(zdepth(ntmpnotfeat,
							ntmpnotmat))/0.1e-12
				}
			}
			if (xtmpdepth > thrshdnot(intmp,imat) &
					zfit(ntmpnotfeat,ntmpnotmat) > 
					thrshfnot(intmp,imat)) {  # NOT is found

				ofit(imat,xel) = 0.0
				odepth(imat,xel) = 0.0
				ofd(imat,xel) = 0.0
				notflg(intmp,imat) = 1
				#if (imat == 146 | imat == 147 | imat == 148) { 
				#	write (ttyout,*) 'DEBUG: imat=',imat,
				#		'  ofit=',ofit(imat,xel),
				#		'  notflg=',notflg(intmp,imat)
				#	write (lunresult,*) 'DEBUG: imat=',imat,
				#		'  ofit=',ofit(imat,xel),
				#		'  notflg=',notflg(intmp,imat)
				#	pause 1  # DEBUG
				#}

			} else {
				notflg(intmp,imat) = 0
			}
				#if (imat == 146 | imat == 147 | imat == 148) { 
				#	write (ttyout,*) 'DEBUG: imat=',imat,
				#		'  ofit=',ofit(imat,xel),
				#		'  notflg=',notflg(intmp,imat)
				#	write (lunresult,*) 'DEBUG: imat=',imat,
				#		'  ofit=',ofit(imat,xel),
				#		'  notflg=',notflg(intmp,imat)
				#	pause 2  # DEBUG
				#}
		}
	    }
	#    if (imat == 146 | imat == 147 | imat == 148 | imat > 462) {
	#	write (ttyout,*) 'DEBUG: after NOT feats: fit=',ofit(imat,xel),' depth=',odepth(imat,xel)
	    #}

4050 	return
	end


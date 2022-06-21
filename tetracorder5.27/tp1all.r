	subroutine tp1all (icubflg,xdat1sp)

######	implicit integer*4 (i-n)
	implicit none

#ccc  name:         tp1all
#ccc  version date: 
#ccc  author(s): Roger N. Clark
#ccc  language: ratfor
#ccc
#ccc  short description: tetracorder/tricorder primary algorithm:
#ccc                     does full spectral analysis of multiple features
#ccc                     and multiple materials, groups, cases, etc.
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

	integer*4 icubflg, imat, jmat
	integer*4 ibest, isecnd
	integer*4 igroup
# RED 07/07/2008 Added declaration of i since no longer global in tricube.h
	integer*4 i, ii, jj
	integer*4 igrpb
	integer*4 kcase, nc

######## now loop each material and feature

	do imat = 1, nmats {  # do all materials in all groups,
				# but skip cases

	   if (ialgorithm(imat) == 0) {   # tetracorder/tricorder primary algorithm

		if (group(imat) > -1) {

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
			#if (imat == 146 | imat == 147 | imat == 148) {
			#	write (ttyout,*) 'DEBUG: in tp1all, after tp1mat called'
			#	write (ttyout,*) 'DEBUG: bdepth=', odepth(jmat,xel), ' fit=',ofit(jmat,xel)
			#}
		}

	   } else if (ialgorithm(imat) == 1) {   # nvres  red edge pos algorithm

		call nvres1mat(imat, xdat1sp)

	   } else {    # WARNING: should not get here, but set to 0 just in case

		ofit(imat,xel) = 0.0
		odepth(imat,xel) = 0.0
		ofd(imat,xel) = 0.0
	   }

	} # end imat do loop

	ibest  = 1
	isecnd = 1
	if (nmats > 1) {   #### FUTURE: nmats includes cases, this test should not

		# now make decisions for each group as to best material

		do igroup = 1, nzgroup {

			ibest = matgrp(1,igroup)
			##write (ttyout,*) 'DEBUG: tp1all, nmatgrp= ',nmatgrp(igroup),
			##			' group:',igroup
			if (nmatgrp(igroup) == 0) next

			do imat = 1, nmatgrp(igroup) {  # find best fit

				    #if (imat == 146 | imat == 147 | imat == 148) {
				#	write (ttyout,*) 'DEBUG: in tp1all, finding best',
				#					' material=',imat
				#	write (ttyout,*) 'DEBUG: bdepth=', odepth(jmat,xel),
				#					' fit=',ofit(jmat,xel)
				#    }
				jmat = matgrp(imat,igroup)
				if (ofit(jmat,xel) > ofit(ibest,xel)) {
					isecnd= ibest
					ibest = jmat
				}
			}
			if (incgrp0(igroup) == 1 & 
					nmatgrp(0) > 0) { # include group 0

				do imat = 1, nmatgrp(0) {  # find best fit

					jmat = matgrp(imat,0)
					if (ofit(jmat,xel) > ofit(ibest,xel)) {
						isecnd= ibest
						ibest = jmat
				    		#if (imat == 146 | imat == 147 | imat == 148) {
						#	write (ttyout,*) 'DEBUG: best=',ibest
						#}
					}
				}
			}
			grpbest(igroup)  = ibest
			grpsecnd(igroup) = isecnd
			##write (ttyout,*) 'DEBUG: tp1all, best ',ibest,
			##			' group:',igroup
			##			' second:', isecnd

			# now compare best and second best according to rules

			if (mclass(isecnd) < mclass(ibest)) {    # 2nd is better class

				xx = ofit(ibest,xel) - dclass(isecnd)
				if (xx < ofit(isecnd,xel)) {

					ibest = isecnd
					grpbest(igroup)  = ibest
					##write (ttyout,*) 'DEBUG: tp1all, second is really best',
					##		'because of class difference'
					##write (ttyout,*) 'DEBUG: tp1all, new best ',ibest,
					##			' group:',igroup

				}

			}

		}


		
			
		# Write diagnostic output at user specified intervals
		if (icubflg == 1) {   # image cube mode
			if (xel == dx2 & mod(yel,nth) == 0) {

				call wrtcrdrout (diaflg, xel, yel,
						lunresult, ttyout)

			}
		} else {              # single spectrum mode
			call wrtspcrdrout (diaflg, inamr, ifils, ititl,
					xel, lunresult, ttyout)
		}

		# now that best is found, others are zero
		do igroup = 1, nzgroup {
			if (nmatgrp(igroup) == 0) next
			do imat = 1, nmatgrp(igroup) {  # check best fit

				jmat = matgrp(imat,igroup)

#	KEL set below replaces following line (GFORTRAN error)
#				if (jmat == grpbest(igroup)) next

#	previous coding DEBUG group
				#if (jmat == grpbest(igroup)) {
				#	write(*,*) 'DEBUG: igroup, jmat = ', igroup, jmat
				#	write(*,*) 'DEBUG: fit, dep, f*d = ',
				#		ofit(jmat,xel), odepth(jmat,xel), ofd(jmat,xel)
				#next
				#}

				#if (imat == 146 | imat == 147 | imat == 148) {
				#	write (ttyout,*) 'DEBUG: in tp1all, zeroing fit, depth',
				#					' material=',imat
				#	write (ttyout,*) 'DEBUG: bdepth=', odepth(jmat,xel),
				#					' fit=',ofit(jmat,xel)
				#}

#	KEL following if statements added to the three zero assignment statements
#		(to fix GFORTRAN - ratfor77 logic error)
				if (jmat != grpbest(igroup)) {
					ofit(jmat,xel)   = 0.0
					odepth(jmat,xel) = 0.0
					ofd(jmat,xel)    = 0.0
				}
			}
		}

		# add material found to pixel statistics
		do igroup = 1, nzgroup {
			if (nmatgrp(igroup) == 0) next
			ibest = grpbest(igroup)
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
		}

	} else {

		# add material found to pixel statistics
		if (nint(ofit(ibest,xel)*qfscal(ibest)) > 0) {
			statsmapfit(ibest) = statsmapfit(ibest) + 1
		}
		if (nint(odepth(ibest,xel)*bdscal(ibest)) > 0) {
			statsmapdepth(ibest) = statsmapdepth(ibest) + 1
		}
		if (nint(ofd(ibest,xel)*bdscal(ibest)) > 0) {
			statsmapfd(ibest) = statsmapfd(ibest) + 1
		}

		# sum the fit to get the mean fit per material

		if (ofit(ibest,xel) > 0.0 & odepth(ibest,xel) > 0.0) {

			fitmean(ibest) = fitmean(ibest) + dble(ofit(ibest,xel))
		}

	}

	#
	# now check if a case must be done
	#
	if (nzcase > 0) {            # check cases if cases defined
	   do ii = 1, nzcase {   # zero cases
		nc = nmatcse(ii)
		if (nc > 0) {
			do jj = 1, nc {
				imat=matcse(jj,ii)
				if (imat > 0) {
					ofit(imat,xel)   = 0.0
					odepth(imat,xel) = 0.0
					ofd(imat,xel)    = 0.0
				}
			}
		}
	   }

	   do igrpb = 1, ngroups {   # check each group to see if must do a case

		imat = grpbest(igrpb)

		if (imat > 0) {
		   if (abs(odepth(imat,xel)) > 0.0 & 
			iaction(imat,1) > 0) {   # do a case

			if (nzcase > 0) {
				do ii = 1, nzcase {

				   kcase = iaction(imat,ii) # which case
				   if (kcase > 0) {
					call tp1cse (icubflg,xdat1sp,kcase)

				   }
				}
			}
		   }
		}
800		continue
	   }
	}

	return
	end

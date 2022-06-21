	subroutine wrtspcrdrout (diaflg, inamr, ifils, ititl,
				xel, lunresult, ttyout)

#######	implicit integer*4 (i-n)
	implicit none

#ccc  name:         wrtspcrdrout
#ccc  version date: 
#ccc  author(s): Roger N. Clark
#ccc  language: ratfor
#ccc
#ccc  short description: write out diagnostic results from specorder.
#ccc
#ccc  algorithm description: 
#ccc  system requirements: Unix
#ccc  subroutines called:
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
#ccc    diagflg: how much to poutput
#ccc    inamr: name of specpr file where spectrum came from
#ccc    ifils: file id of specpr file where spectrum came from
#ccc    ititl: title of spectrum
#ccc    lunresult: lun of results file
#ccc    ttyout: lun for std out
#ccc---------------------------------------------------------------

# arrays for multiple materials

	include "../specpr/src.specpr/common/spmaxes"   # max parameters, must be first

	include "multmap.h"

	integer*4 diaflg, ifils, ibest, lunresult, ttyout
	integer*4 xel, jj, ii, intmp, igroup, imcase
	integer*4 ntmpnotmat, ntmpnotfeat, idonothing
	integer*4 tmplength
	character*8 inamr
	character*40 ititl
	character*50 tmptitle   # this should = length of mfile (in multmap.h)
	character*512 soundstring

	character*1 imch(5)

	# define feature importance characters

	imch(1) = 'O'
	imch(2) = 'W'
	imch(3) = 'D'
	imch(4) = 'M'
	imch(5) = '?'

	# diaflg:
	#        0    (default) for one-line answers only'
	#        1    for abreviated answers only'
	#        2    for weighted fit + answer'
	#        3    for full diagnostic output:'
	#                 (individual fits, weighted fits'
	#                 and answer'
	#        4    one-line answers to screen, and '
	#                  full diagnostic output to results file'



# Write diagnostic output at user specified intervals

	if (diaflg == 0 || diaflg == 2 || diaflg == 4) {
		#write (ttyout,*) 'DEBUG: diaflg =', diaflg
		# call eralph
		write (ttyout,55)
55		format (/,/,/,/,1x,60('#'))
		do igroup = 1, nzgroup {                ########### group output
			if (nmatgrp(igroup) == 0) next
			if (groupenable(igroup) == 0) next
			ibest = grpbest(igroup)

			if (igroup == 1) {
				write (ttyout,*) ' '
				write (ttyout,*) ' '
				write (ttyout,*) '####### CHOSEN OUTPUT:'
				write (ttyout,106) inamr, ifils, ititl
106				format ('Spectrum: ',a,i7,2x,a)

				write (ttyout,1061)
1061				format ('    Grp/Cse' 14x,'Material',63x,
					'Fit     Depth    F*D')

				write (lunresult,*) ' '
				write (lunresult,*) ' '
				write (lunresult,*) '####### CHOSEN OUTPUT:'
				write (lunresult,106) inamr, ifils, ititl
				write (lunresult,1061)
			}

			# ofit(ibest,xel)  < 1.1 catches NaN (I hope)
			if (ofit(ibest,xel) < 0.1e-4 || odepth(ibest,xel) < 0.1e-4 && ofit(ibest,xel)  < 1.1) {
				#old: write (ttyout,107) inamr, ifils, ititl,igroup
				if (groupenable(igroup) > 0 ) {
					write (ttyout, 1071) igroup, groupname(igroup)
1071					format ('     grp',i2,3x,a12,'  none        ')
				
					#old write (lunresult,107) inamr, ifils, ititl,igroup
					write (lunresult, 1071) igroup, groupname(igroup)
107					format (1x,a,i6,1x,a,'     grp',i2,3x,a12,' none         ')
				}
			} else if (ofit(ibest,xel) >= 0.1e-4 && ofit(ibest,xel)  < 1.1) {
				#old write (ttyout,108) inamr, ifils, ititl,

				tmptitle = '                                                  '
				tmptitle = mfile(ibest)
				#tmplength  = lnb(tmptitle)

				write (ttyout,1081) igroup, groupname(igroup), ibest,
					tmptitle,
					ofit(ibest,xel),
					odepth(ibest,xel),
					ofd(ibest,xel)
1081				format ('     grp',i2,3x,a12,i5,'  MATCHES:  ',a,
					1x,f7.4,' ',f8.4,' ',f8.4)
108				format ('Spectrum: ',a,i7,2x,a,
					'MATCHES ',a,
					' Fit=',f7.4,' D=',f8.4,
					' FD=',f8.4)
				# old:write (lunresult,108) inamr, ifils, ititl,
				#	otitle(ibest),
				#	ofit(ibest,xel),
				#	odepth(ibest,xel),
				#	ofd(ibest,xel)
				write (lunresult,1081) igroup, groupname(igroup), ibest,
					tmptitle(1:30),
					ofit(ibest,xel),
					odepth(ibest,xel),
					ofd(ibest,xel)
			  	if (dosound(ibest) == 1 && soundenable == 1) {    # output answer as sound

					soundstring= 'tetracordersound ' // sound1fil(ibest) // ' ' // char(0)
					#write (ttyout,*) 'DEBUG: ', soundstring

					call system (soundstring)
			  	}
			} else {
				idonothing = 0  # do no output
			}
		}  # end igroup loop

		#################################
		#
		#  case ouput is doen in tp1cse
		#
		#################################
	}
	if (diaflg == 1 ) {
		#write (ttyout,*) 'DEBUG: diaflg =', diaflg
		#if (diaflg != 4) call eralph
		do igroup = 1, nzgroup {
			if (nmatgrp(igroup) == 0) next
			ibest = grpbest(igroup)
			if (ofit(ibest,xel) < 0.1e-4) {
			  if (diaflg != 4) write (ttyout,107) inamr, ifils, ititl,igroup
			  write (lunresult,107) inamr, ifils, ititl,igroup
			} else {
			  if (diaflg != 4) write (ttyout,111) inamr, ifils, ititl,
				otitle(ibest)(1:32),
				ibest,
				ofit(ibest,xel),
				odepth(ibest,xel),
				ofd(ibest,xel), mfile(ibest)
111			  format ('The spectrum: ',a,i7,3x,a,//,
				'is best matched by',14x,a,/,
				32x, 40('='),//,
				' Material  Fit  ',
				'    Depth        F*D',/,
				(i6,1x,f7.4,3x,
					f8.4,3x,f7.4, 1x,a,/))
			  write (lunresult,111) inamr, ifils, ititl,
				otitle(ibest)(1:32),
				ibest,
				ofit(ibest,xel),
				odepth(ibest,xel),
				ofd(ibest,xel), mfile(ibest)

			  if (dosound(ibest) == 1 && soundenable == 1) {     # output answer as sound

				soundstring= 'tetracordersound ' // sound1fil(ibest) // ' ' // char(0)
				#write (ttyout,*) 'DEBUG: ', soundstring

				call system (soundstring)
			  }
			}
		}
	} 
	#if (diaflg != 40) call eralph
	if (diaflg == 3 || diaflg == 4) {
		  # write (ttyout,*) 'DEBUG: diaflg =', diaflg,' 2nd section'
		  if (diaflg == 3) write (ttyout,109)
		  write (lunresult,109)
109		  format (/,17x,' FITS, DEPTHS, F*D',
			' before best fit selection:',/,
			10x,'Title',19x,
			'Mat Feat',
			1x,'T    Fit   Depth',
			'    f*d      Nrmlz')
		  do jj = 1, nmats {
			if (group(jj) > -1) { # valid group
			    if (diaflg == 3) write (ttyout, 112) (otitle(jj)(1:32),
				jj,ii,
				imch(featimprt(ii,jj)+1),
				zfit(ii,jj),
				zdepth(ii,jj),
				zfd(ii,jj),dln(ii,jj),
				zcompf(ii,jj), mfile(jj),
				ii=1,nfeat(jj))
			    write (lunresult, 112) (otitle(jj)(1:32),
				jj,ii,
				imch(featimprt(ii,jj)+1),
				zfit(ii,jj),
				zdepth(ii,jj),
				zfd(ii,jj),dln(ii,jj),
				zcompf(ii,jj), mfile(jj),
				ii=1,nfeat(jj))
112			    format (a,1x,i4,1x,
				i4,1x,a,1x,f6.3,
				1x,f7.4,1x,f7.4,1x,
				f6.3,',',f4.1, 1x, a)
			#   write (ttyout,*) 'DEBUG: numnotfeat=',numnotfeat(jj),
			#			' ofit=',ofit(jj,xel)
			#   write (lunresult,*) 'DEBUG: numnotfeat=',numnotfeat(jj),
			#			' ofit=',ofit(jj,xel)
			    if (numnotfeat(jj) > 0) { # have NOT features
				do intmp = 1, numnotfeat(jj) {
					ntmpnotmat = notmat(intmp,jj)
					ntmpnotfeat= notfeat(intmp,jj)
					# NOT feature is possible
					if (notflg(intmp,jj) > 0) {
								# NOT is found

					   if (diaflg == 3) write (ttyout,221) ntmpnotfeat,
						ntmpnotmat, jj

					   write (lunresult,221) ntmpnotfeat,
						ntmpnotmat, jj
221				format (' NOT condition:       feat', i4,
					', material', i4, ' excludes material ',i4)
					} else {

					   if (diaflg == 3) write (ttyout,222) jj,
						ntmpnotfeat, ntmpnotmat
					   write (lunresult,222) jj,
						ntmpnotfeat, ntmpnotmat
222				format (' NO NOT condition:    on material', i4,
					' feat', i4, ', material', i4)
					}
				}
			    }
			}     # end if statement for valid group
		  }           # end for do loop jj
		  if (diaflg == 3) write (ttyout,*) ' '
		  write (lunresult,*) ' '

		if (diaflg == 3) write (ttyout,114)
		write (lunresult,114)
114		format ('Weighted Fits, Depths,',
			' and F*Ds values:',/,
			10x,'Title',21x,' Group Material',
			2x,'Fit',4x,'Depth',4x,'F*D')
		do jj = 1, nmats {
			if (group(jj) > -1) { # valid group
			   if (diaflg == 3) write (ttyout,113) otitle(jj)(1:32),
				group(jj),jj,
				ofit(jj,xel),
				odepth(jj,xel),
				ofd(jj,xel), mfile(jj)
			   write (lunresult,113)  otitle(jj)(1:32),
				group(jj),jj,
				ofit(jj,xel),
				odepth(jj,xel),
				ofd(jj,xel), mfile(jj)
113			   format (a,5x,i5,1x,i5,3x,f6.3,1x,f7.4,1x,f7.4,1x,a)
			}
		}
		if (diaflg == 3) write (ttyout,*) ' '
		if (diaflg == 3) write (ttyout,*) ' '
		write (lunresult,*) ' '
		write (lunresult,*) ' '

		do igroup = 1, nzgroup {
			#write (ttyout,*) 'DEBUG: group:',igroup,nmatgrp(igroup),nzgroup,grpbest(igroup)
			if (groupenable(igroup) == 0) next
			if (nmatgrp(igroup) == 0) next
			ibest = grpbest(igroup)
			if (ofit(ibest,xel) < 0.1e-4) {
			  if (diaflg == 3) write (ttyout,107) inamr, ifils, ititl,igroup
			  write (lunresult,107) inamr, ifils, ititl,igroup
			} else {
			  if (diaflg == 3) write (ttyout,111) inamr, ifils, ititl,
				otitle(ibest)(1:32),
				ibest,
				ofit(ibest,xel),
				odepth(ibest,xel),
				ofd(ibest,xel), mfile(ibest)
			  write (lunresult,111) inamr, ifils, ititl,
				otitle(ibest)(1:32),
				ibest,
				ofit(ibest,xel),
				odepth(ibest,xel),
				ofd(ibest,xel), mfile(ibest)
			  if (dosound(ibest) == 1 && soundenable == 1) {     # output answer as sound

				soundstring= 'tetracordersound ' // sound1fil(ibest) // ' ' // char(0)
				#write (ttyout,*) 'DEBUG: ', soundstring

				call system (soundstring)
			  }
			}
		}

	} 

###DEBUG:
	do igroup = 1, nzgroup {
		#write (ttyout,*) 'DEBUG: group: ',igroup,' best mat= ',grpbest(igroup)
	}

	return
	end

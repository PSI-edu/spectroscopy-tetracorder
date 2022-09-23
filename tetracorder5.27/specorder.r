	subroutine specorder

	implicit integer*4 (i-n)

#ccc  name:         cubecoredr
#ccc  version date: 
#ccc  author(s): Roger N. Clark
#ccc  language: ratfor
#ccc
#ccc  short description: analyze features in a spectrum with the tetracorder algorithm
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
#ccc---------------------------------------------------------------


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
	include         "../specpr/src.specpr/common/lblprt"

# arrays for multiple materials

	include "multmap.h"

	include "tricube.h"

# basic tetracorder parameters

        include "tri1.h"

	character*40 otitl2

	integer*4 ikl, ista, ire, iprt, lun, icheck, waitcount

	integer*4 fnb     # function
	integer*4 lnb     # function
	integer*4 iprtvsav  # protection of file v before following.
	integer*4 ifilsav   # save the record  number of the spectrum to be analyzed
	integer*4 aiprtv    # absolute value of v-protection, iprtv

	integer*2	ibit, chkbit

	character*16  sleepstr

	real*4 waittime   # how long to sleep waiting for file growth.

	#  vfollow:    follow a growing v file (in lblprt common)

#RED
# changed 'call er'to 'call eralph'
# segmentation-fault error when display set to X-Windows
#    due to a screen clear 'XWIN call xclear'
#    in 'src.specpr/hpgraph/respag.r'
#69	call er
	call eralph

	call whedr

	waittime = 1.0   # wait time in seconds

69	write(ttyout,70)   dtemperature(1), dtemperature(2), dpressure(1), dpressure(2)
70      format (///,40('-'),/,
			' Enter file id and record number for',
			' the SPECTRUM to be analyzed',/,5x,
			' type  f  to follow a growing file (v-file only, and v must be read only',/,5x,
                        ' type  d  to delete points)',/,5x,
			' type  s  to include answers by voice sound',/,5x,
			' type  n  to turn off sound',/,5x,
			' type  t N  for time to wait when following, N= value in seconds',/,5x,
			'            devault = 1.0 second.',/,
			' or e or x to exit.'//,
			'NOTE temperature range=',f6.1,' to ',f6.1,' Kelvin',/,
			'       pressure range =',f6.1,' to ',f6.1,' Bars')

	

	chdltflg = ' '  # default: no delete points
	vfollow  = 0   # do not follow.

	call crtin
	i = 1
	x1 = 0.0
	call wjfren(i,x1,idevs)
	if (i >= 79 && idevs == 0) {
		call what(i)
		write (ttyout,*) 'expected a file letter ID'
		call crtin
		go to 69
	}
	call wjfren(i,xfilb,ic2)

	if (ic2 == 0 && i < 80) call wjfren(i,x,ic2)

75	if (ic2 == ihd) {          # look for delete points
		chdltflg = 'd'
	} else if (ic2 == ihf) {   # look for follow flag
		vfollow  =  1
	} else if (ic2 == ihs) {   # enable sound
		soundenable = 1
	} else if (ic2 == ihn) {   # no sound
		soundenable = 0    # no sound
	} else if (ic2 == iht) {   # wait time when following
		call wjfren(i,x,ic2)
		if (x > 0.001 && x < 100.0) {
			waittime = x
		} else {
			call what(i)
			write (ttyout,*) 'time out of range, must be 0.001 to 100.'
			write(ttyout,66)
			go to 69
		}
		if (ic2 != 0) i = i-1   # back up so next scan sees it.
	} else if (ic2 != 0) {
		call what(i)
		write (ttyout,*) 'expected d, f, s, t'
		write (ttyout,"(' got: ',a1)") ic2
		write(ttyout,66)    # invalid input, reenter go to 69
	}
	ic2=0
	if (i < 80) {
		call wjfren(i,x,ic2) 
		go to 75	 # cycle until the whole line is scanned
	}

	if (vfollow == 1 && idevs != ihv) {    #  must be v file

		call what(i)
		write(ttyout, 45)
45		format ('Following a growing file can only be done with file v')
		write(ttyout,66)
		go to 69
	} else {

		iprtvsav = iprtv    # save the current protection on the v-file
	}
	if (vfollow == 1 && idevs == ihv) {    #  must be v file

		write(ttyout, 46)
46		format ('Following file v if it grows.  Terminate by killing the program.')
	}

	write (ttyout,*) 'vfollow=', vfollow

#        *** check for hard or soft exit ***
	if (ic2==ihx || ic2==ihe)  {
		ic=ic2
		icrst = 1
		call rstart(icrst)
		return
	} else if (idevs==ihx || ic2==ihx) {
		ic=ic2
		icrst = 1
		call rstart(icrst)
		return

#       *** check for invalid input ***
	} else if (x1!=0.0 || ic2!=0 || xfilb<=0.0) {
		call what(i)
		write(ttyout,66)
66		format ('invalid input, reenter')
		#call crtin
		go to 69

#       *** looks ok so get file s ***
	} 
	ifils = xfilb
	ifilsav = ifils

	call devok (4,idevs,ifils,lun,ier)
	if (ier!=0) {
		write (ttyout,*) 'ERROR: invalid file+record no.'
		write (ttyout,*) '       press return to try again'
		call crtin
		go to 69
	}
	itmp = ifils
	call redfil(itmp,lun,ier)
	if (ier != 0) {
		write (ttyout,*) 'ERROR: reading file+record no.'
		write (ttyout,*) '       press return to try again'
		call crtin
		go to 69
	}
	iform = 1
	write (ttyout,68) idevs, ifils, ititl
68	format (' Spectrum:','  ',a,i7,5x,a,//)
	call namdev (idevs, inamr)

	# note: already checked above.  Maybe not needed here?
	call wjfren(i,x1,ic2)  # check for delete points indicator
	if (ic2 == ihd) {    # want to delete points
		chdltflg='d'
		ic2=0
	}

# write history
	write (lunhist,3050) ihbcksl, idevs, ifils,chdltflg, ihbcksl,
				 inamr, ihbcksl, ititl
3050    format (a,'#',17x,'Analyzing spectrum:',//,
                a1,i7,1x,a,14x,a,
                '# file ID, rec no. (',a,')'/,
                a,'#',11('=-'),' TITLE=',a)

# enter thresholding

3139	write (ttyout,3140)
3140	format ('Enter the the minumum and maximum data thresholds.',//,
                5x,'The thresholds are in real number (scaled) values, and',/,
                5x,'if = 0.0  no thresholds are set')

	call crtin
	i = 1
	call wjfren (i,x,il)
	if (il==ihx || il==ihe)  {
		ic=il
		icrst = 1
		call rstart(icrst)
		return
	}
	if (il != 0) {
		call what(i)
		go to 3139
	}
        thrshmin=-1.0e35
        thrshmax= 1.0e35

        call wjfren (i,x,il)
        if (il != 0) {
                call what (i)
                go to 3139
        }
        if(abs(x) > 0.1e-20) thrshmin = x

        call wjfren (i,x,il)
        if (il != 0) {
                call what (i)
                go to 3139
        }
        if(abs(x) > 0.1e-20) thrshmax = x

#	write history

	if (thrshmin < -1.0e34 & thrshmax > 1.0e34) {
		write (lunhist, 3138) ihbcksl
3138		format (30x,a,'# no min or max data thresholds')
	} else {
		write (lunhist, 3137) thrshmin,thrshmax,ihbcksl
3137		format (f12.6,f12.6,15x,a,'# min, max data thresholds')
	}


# determine how much to output to user.

	write (ttyout,3120)
3120	format ('Enter the amount of output you desire:',/,
		'          0    (default) for one-line answers only',/,
		'          1    for abreviated answers only',/,
		'          2    for weighted fit + answer',/,
		'          3    for full diagnostic output:',/,
		'                   (individual fits, weighted fits',/,
		'                   and answer',/,
		'          4    one-line answers to screen, and ',/,
		'                    full diagnostic output to results file')
	call crtin
	i = 1
	call wjfren (i,x,il)
	if (il==ihx || il==ihe)  {
		ic=il
		icrst = 1
		call rstart(icrst)
		return
	}
	if (il != 0) {
		call what(i)
		go to 3059
	}
	diaflg = nint(x)
	if (diaflg < 0) diaflg = 0
	if (diaflg > 3) diaflg = 4

	write (lunhist, 3122) diaflg,ihbcksl
3122	format (i3,15x,a,'# diagnostic level of output')

	if (diaflg > 0) {
		write (lunresult,3051) inamr, ifils,chdltflg, ititl
3051		format (40('-='),/,27x,'Analyzing spectrum:',/,
		a,i7,1x,a,15x,a)
	}

# delete points

	if (chdltflg == 'd') {
		write (ttyout,*) 'Enter channels to delete, then a c to continue'
		call crtin
		i=1
		call dltpts(i,jdlt,idlt,nchans,ic2)
		if (ic2==ihx)  {
			icrst = 1
			call rstart(icrst)
			return
		}
		if (ic2!=ihe & jdlt > 1) {
			mxstrdel=240   # 3 lines of 80 characters
			call delhist(jdlt,idlt,mhist,mxstrdel)

			write (lunhist,3055) mhist(1:lnb(mhist(1:80)))
			write (lunresult,3055) mhist(1:lnb(mhist(1:80)))
3055			format (a)
			if (mhist(81:81) != ' ') {
				write (lunhist,3055) mhist(81:lnb(mhist(81:160)))
				write (lunresult,3055) mhist(81:lnb(mhist(81:160)))
			}
			if (mhist(161:161) != ' ') {
				write (lunhist,3055) mhist(161:lnb(mhist(161:240)))
				write (lunresult,3055) mhist(161:lnb(mhist(161:240)))
			}
			do jj = 1, jdlt {               # delete the channels
				data(idlt(jj))=-1.23e34
			}
		} else {
			write (lunhist,3055) 'c'
			write (lunresult,3055) 'c'
		}

	}

# Title 

	write (ttyout,162)
162	format (' Type in an output comment',/,
		'---------------------------------------|')
	do ij = 1, 40 {    # initialize
		otitl2(ij:ij) = ' '
	}
	otitl2(1:1) = '#'
	call crtin
	jfirst=fnb(iopcon)
	if (jfirst < 1) jfirst = 1 # no blank at beginning of line
	jend=lnb(iopcon)
	if (jend == 0) jend = 80 # no blank at end of line
	length = jend - jfirst + 1
	if (length > 40) length = 40
	if (length > 0) {
		jend = jfirst + length - 1
		if (jfirst >= 1 && jend <= 80 && jfirst < jend && jend - jfirst <= 39) {   # valid comment
			otitl2(1:length) = iopcon(jfirst:jend)
		}
		if (length < 40) otitl2(length+1:40)=' '
		do ij = 1, 40 {
			if(otitl2(ij:ij) == char(0)) {
				otitl2(ij:ij) = ' '
			}
		}
	}

# write history
3059	write (lunhist, 3060) otitl2,ihbcksl
3060	format (a, 2x,a,'# comment ')
	write (lunresult, 3060) otitl2,ihbcksl

################
nth = 1    # print the spectrum analyzed in single spectrum mode
xel = 1    # one pixel only in single spectrum mode
yel = 1    # one line only in single spectrum mode

# set stats to zero

	#write (ttyout,*) 'DEBUG: specorder test point 1'

	# Cycle to label 101 is following a growing file
101	do i = 1, nmats {
		statsmapfit(i) = 0
		statsmapdepth(i) = 0
		statsmapfd(i) = 0
		fitmean(i) = 0
	}
	#write (ttyout,*) 'DEBUG: specorder test point 1b'

	icubflg = 0
        # check thresholds and delete out of min max range

        if (thrshmin > -1.0e34 | thrshmax < 1.0e34 ) {  # do range check
                do ikl = 1, nchans {
                        if (data(ikl) < thrshmin) {
                                data(ikl) = -1.23e34
				#write (ttyout,*) 'DEBUG: NOTE ch:',ikl,
				#		' below threshold: DELETED'
                        }
                        if (data(ikl) > thrshmax) {
                                data(ikl) = -1.23e34
				#write (ttyout,*) 'DEBUG: NOTE ch:',ikl,
				#		' above threshold: DELETED'
                        }
                }
        }



    ######## now analyze the spectrum #########################

	#write (ttyout,*) 'DEBUG: specorder test point 2'

	call tp1all (icubflg,data)

	#write (ttyout,*) 'DEBUG: specorder test point 3'

	if (vfollow == 1) {   # follow a growing v-file (added 3/2019)

		# We need to know how much a file must grow before the next
		# spectrum is fully in the file
		# specpr file type 2 is 1536 bytes per record.
		#        1st record contains 512 bytes header, 256 channels of data
		#        subsequent records contain 4 bytes bit flags, 383 channels data.
		nrecsperspec = 0
		if (nchans <= 256) {
			nrecsperspec = 1
		} else {
			nrecsperspec = 1 + int((float(nchans) - 256.0)/383.0 +0.999)
		}

		# iprtvsav  # protection of file v before following.
		# ifilsav   # record number of spectrum previously analyzed

		ifilnext = ifilsav + nrecsperspec   # record number of next spectrum to analyze
		iprtneed = ifilnext + nrecsperspec-1  # protection number needed to indicate next spectrum is ready

		ire  = 0  # for devsta call
		ista = 0  # for devsta call
		iprt = 0  # for devsta call

		# the wait loop
		#write (ttyout, *) 'debug: ifilsav=',ifilsav, 'iprtvsav=', iprtvsav
		#write (ttyout, *) 'debug: nrecsperspec=', nrecsperspec
		#write (ttyout, *) 'debug: checking for wait: iprtv=',iprtv,'  iprtneed=',iprtneed

1001		aiprtv = abs(iprtv)

		waitcount=1  # manage how often wait messages are printed

		while (aiprtv < iprtneed) {    # protection too low, so wait

			write (sleepstr, 111) waittime
111			format('sleep ',f7.3, ' ')
			if (waitcount == 1 || waitcount == 60 || waitcount == 200 || waitcount == 800 || waitcount == 1800 ) {
				write (ttyout, 112) waittime, ifilnext, aiprtv, iprtneed
			}
			if (waitcount == 3 || waitcount == 83 || waitcount == 303 || waitcount == 1003 || waitcount == 2003) {
				write (ttyout, *) '           still waiting....'
			}
112			format ("waiting ", f7.3 " sec. for record=",i6,"  v-protection=",i6," need:",i6)
			call system (sleepstr // char(0))  # sleep to wait for file to grow

			# check status and file size, update protection if growing and following
			call devsta (lun, ista, ire, iprt)
			aiprtv = abs(iprtv)
			waitcount = waitcount +1
			if (waitcount > 2003) waitcount = 302
		}

		# OK, next spectrum is in the file

		ifils   = ifilnext
		ifilsav = ifils

		#write (ttyout, *) 'DEBUG: nrecsperspec=', nrecsperspec, 'protection now:', iprtv

		write (ttyout, 115) idevs, ifils
115		format (' Following.  Next spectrum is now available: ', a1, i7)

		call devok (4,idevs,ifils,lun,ier)  # check file status, calls devsta + more
		if (ier!=0) {
			write (ttyout,*) 'ERROR: invalid file+record no.'
			write (ttyout,*) '       press return to try again'
			call crtin
			go to 69
		}
		itmp = ifils
		call redfil(itmp,lun,ier)   # reads the spectrum
		if (ier != 0) {
			write (ttyout,*) 'ERROR: reading file+record no.'
			write (ttyout,*) '       press return to try again'
			call crtin
			go to 69
		}
		ibit = 1
		icheck = chkbit(icflag,ibit)
		#write (ttyout, *) 'DEBUG: ibit 1=', icheck
		if (icheck == 1) {   # text file

			write (ttyout, *) ' Encountered a text record, skipping.'
			
			if (itxtch < 1) itxtch = 1
			if (itxtch > maxtxt) itxtch = maxtxt
			irect = 1 + int((float(itxtch) -1476.0)/1532.0 +0.999)
			ifils = ifils + irect   # next spectrum after this text record
			ifilnext = ifils
			iprtneed = ifilnext + nrecsperspec-1
			#write (ttyout, *) 'DEBUG: text file is ', irect,' records'
			write (ttyout, *) '      Now waiting for record ',ifilnext,' protection=',iprtv
			go to 1001
		}
		iform = 1
		# write history
		write (lunhist,3050) ihbcksl, idevs, ifils,chdltflg, ihbcksl,
					 inamr, ihbcksl, ititl

		go to 101   # analyze next spectrum
	}

###########################################################
# pause so user can see the result.

	write (ttyout,*) ' '
	write (ttyout,*) ' '
	write (ttyout,*) 'Analysis Complete.  Press return to continue'
	call crtin


	if (noflush == 1) {    # flush buffer
		write (ttyout, *) 'Flushing results buffer'
		call flushseqfile (lunresult,
			resultfile(1:lnb(resultfile)), ier)
		if (ier != 0) {
			call what (-1)
			return
		}

# End program

		write (ttyout,*)'Updating restart file.'
		icrst = 1
		call rstart (icrst)
	}
	return
	end

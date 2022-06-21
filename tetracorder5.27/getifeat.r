	subroutine getifeat (i, ifeat, imat, ndl, irtn)

	implicit integer*4 (i-n)

#ccc  name:         getifeat
#ccc  version date: 
#ccc  author(s): Roger N. Clark
#ccc  language: ratfor
#ccc
#ccc  short description: sets up the reference library features ifeats
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
#ccc            This routine generates a band depth map

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

	character*40 tmptitle

	integer*4 tmplength

	integer*4 ihchar     # function
	logical*4 fexist     # file exists: true, false if doesn't

	integer*4 cmdverbose   # function cmdverbose
	integer*4 fnb          # function fnb
	integer*4 lnb          # function lnb
	integer*4 ictlimflg, ictrlimflg, ictllimflg
	integer*4 ichtmp2, ichtmp1, ndl
	integer*4 itmpfr, nfr, irtn
	integer*4 itmpf, i, ifeat, imat

	character*1 imch(5)
	character*20 chtest
	character*110 chtmp1, chtmp2
	character cht1*3, cht2*5, cht3*2

	real*4 x
	real*4    wav1, wav2, wav3, wav4, wav5, wav6, wav7, wav8  # wavelengths of continuum intervals
	integer*4 nch1, nch2, nch3, nch4, nch5, nch6, nch7, nch8  # channels of continuum intervals
	integer*4 nch1left, nch2left, nch3right, nch4right        # channels of the contuum, central block


# define feature importance characters

	imch(1) = 'O'
	imch(2) = 'W'
	imch(3) = 'D'
	imch(4) = 'M'
	imch(5) = '?'

	irtn = 0 # return value good so far

#####################################################################
# The define featers block looks like below, as of 6/2011. 
#  This subroutine gets called when the "f" is found.

#define features                  \# Number of features, and not feats
#                  \# blank line recycles to same input line
#                  \# defining features outside of range of work is OK
#                  \# if outside, they are ignored
#   f1a Dw 0.902  0.930  1.080  1.110  ct 0.08 0.7
#   f1a                                rcbblc> .1 .2  \# shoulderness
#   f1a                                weight*2.2     \# multiply weight by 2.2
#   f1a ignore w 0.98 0.99  \# ignore wavelength range from 0.98 to 0.99
#   f1a ignore w 1.01 1.02  \# ignore wavelength range from 1.01 to 1.02
#                          \# can specify up to 3 ranges per feature.
#   f1a mode= all  \# could be airless, mars_orbit, earth_orbit, moon, lab, titan, comet ...
#   f1a move leftc left 2c    \# allow left continuum to move left up to 2 channels
#   f1a move rightc right 2c  \# allow left continuum to move right up to 2 channels
#
#   f2a Dw 1.150   1.178   1.315   1.345  ct 0.05 lct/rct> 0.9 1.1
#   f2a mode= all
#
#   f3b DW 3.4     3.5     3.6     3.7    ct 0.01 0.02   lct/rct> 0.9 1.1
#
#   n1a NOT [NOTGREENVEG]  1  0.50a 0.5   \# NOT veg, feat 1, depth .50, fit .5
#                \# the [NOTGREENVEG] could (should) be changed to the ID
#                \# no mode specified, so default = all
#
#    \# concept for future: need a "degrades to" so if diagnostic features are
#    \# all deleted, the entry degrades to a simpler answer.
#        \# how about a case: degrades to case X
#
#endfeatures


	itmp =cmdverbose(-1)

# we are already scanning the line at the time of entry to this subroutine
# scan position = i

	tmptitle = matid(imat)(1:40)        # use for diagnostic ouput
	tmplength = lnb(matid(imat)(1:40))

	# write (ttyout, 352) imat, ifeat, group(imat), tmptitle(1:tmplength), tmplength
352	format ('DEBUG: getifeat   Material',i6, '  feature ',i3, '  group', i4, '  ID= ', a,' tmplength=', i3)


100	i =1
   	call wjfren (i,x,il)  # scan new line

        if (i >= 80 & il == 0) {
		call crtin
		if (cmdverbose(-1) <= 2) write (ttyout,*) iopcon
		go to 100    # blank line, read again
	}
        if (il==ihe && iopcon(i-1:i+9) == 'endfeatures') { # endfeatures

                if (nfeat(imat) == 0 &  numnotfeat(imat) == 0) {
                        call what(i)
                        write (ttyout,*) 'ERROR: endfeatures found, but no features entered'
                        go to 100
                }

		write (lunhist,*) 'endfeatures'
		if (cmdverbose(-1) <= 1) write (ttyout,*) 'DEBUG: endfeatures found'
                irtn = 0    # features complete
		return
        }
        if (il==ihx || il==ihe) {
                ic=il
                icrst = 1
                call rstart(icrst)
                call what (-1)
			write (ttyout,*) 'exiting getifeat: e or x  in user input not expected'
		irtn=1
                return
        }
        if (il!=ihf && il!=ihn) {  # should be f for feature or n for notfeature

                write (ttyout,3041) iopcon
                call what (i)
		irtn=1
                return
3041            format (' ERROR: expecting f for feature or n for notfeature',/,
                        ' (routine getifeat.r  input line=',/, a)

		return
        }

        if (il == ihn) {  # not feature, but this is the regular feature subroutine

		irtn=2   #get not feature
		return
        }

	call wjfren (i,x,il)  # now get feature number

	itmpf=int(x+0.5)

	if (itmpf < 1 || itmpf > maxfeat) {
		call what(i)
		write (ttyout,*) 'ERROR: feature number out of range limits set at compile time:', itmpf
		irtn = 1
		return
	}
	ifeat=itmpf

	if (ifeat > nfeat(imat)) nfeat(imat) = ifeat   # new max number of features for material imat
	
	if (il == 0) call wjfren (i,x,il)  # scan for next character

        jsplib = 0                    # which reference library to use
        if (il == iha ) jsplib = 1
        if (il == ihb ) jsplib = 2
        if (il == ihc ) jsplib = 3
        if (il == ihd ) jsplib = 4
        if (il == ihe ) jsplib = 5
        if (il == ihf ) jsplib = 6
        if (il == ihg ) jsplib = 7
        if (il == ihh ) jsplib = 8
        if (il == ihi ) jsplib = 9
        if (jsplib > maxlibs  || jsplib == 0) {
                write (ttyout,*) 'ERROR: library letter code out of range ifeat=', ifeat,' for material=',imat
                call what(i)
		irtn = 1
		return
        }

	# jump to hear to continue scanning line

101	call wjfren (i,x,il)  # get feature importance or constraints
	#write(ttyout,*) 'DEBUG: after line 101 wjfren call, i=',i,' x=',x,' il=',il

        if (i >= 80 & il == 0) {  # end of line, so scan new line
		call crtin
		if (cmdverbose(-1) <= 2) write (ttyout,*) iopcon
		go to 100
	}

	#write (ttyout,*) "DEBUG: getifeat il= ", iopcon(i-1:i-1)
	if (il==ihcd) {                    # Diagnistic feature definition
		featimprt(ifeat,imat) = 2
		go to 501
	} else if (il==ihcm) {             # Must be present unconditionally Diagnistic feature definition
		featimprt(ifeat,imat) = 3
		go to 501
	} else if (il==ihco) {             # Optional feature definition
		featimprt(ifeat,imat) = 0
		go to 501
	} else if (il==ihcw) {             # Weak feature definition
		featimprt(ifeat,imat) = 1
		go to 501

	} else if (i < 72 & (il == ihl) & 
			(iopcon(i:i+6) == 'ct/rct>')) {  # lct/rct> n option found
				go to 1000  

	} else if ((i < 72) & (il == ihr) & 
			(iopcon(i:i+6) == 'ct/lct>')) {  # rct/rcl> n option found
				go to 2000

	} else if ((i < 72) & (il == ihr) & 
			(iopcon(i:i+5) == 'cbblc>')) {  # rcbblc> n option found
				go to 3000

	} else if ((i < 72) & (il == ihr) & 
			(iopcon(i:i+5) == 'cbblc<')) {  # rcbblc< n option found
				go to 4000

	} else if ((i < 72) & (il == ihl) & 
			(iopcon(i:i+5) == 'cbbrc>')) {  # rcbblc> n option found
				go to 5000

	} else if ((i < 72) & (il == ihl) & 
			(iopcon(i:i+5) == 'cbbrc<')) {  # rcbblc< n option found
				go to 6000

	} else if ((i < 72) & (il == ihr) & 
			(iopcon(i:i+3) == '*bd>')) {  # r*bd> n m option found
				go to 7000

	} else if ((i < 80) & (il == ihc) & (iopcon(i:i) == 't')) {  # ct option found
				#write (ttyout,*) 'DEBUG: ct continuum threshold found, i=', i
				#write (ttyout,*) 'DEBUG: iopcon='
				#write (ttyout,*) iopcon
				go to 8000

	} else if ((i < 79) & (il == ihr) &
			(iopcon(i:i) == 'c') & 
				(iopcon(i+1:i+1) == 't')) {  # rct option found
				go to 9000

	} else if ((i < 79) & (il == ihl) &
			(iopcon(i:i) == 'c') & 
				(iopcon(i+1:i+1) == 't')) {  # lct option found
				go to 10000

	} else if (i < 72 & il == ihi & iopcon(i-1:i+4) == 'ignore') {

				go to 11000

	} else if (i < 72 & il == ihm & iopcon(i-1:i+2) == 'move') {

				go to 12000

	} else if (i < 72 & il == ihw & iopcon(i-1:i+5) == 'weight*') {

				go to 13000

	} else if (i < 72 & il == ihm & iopcon(i-1:i+2) == 'mode') {

				go to 14000

	} else {
		call what(i)
		write (ttyout, 302)
302		format (' ERROR: unexpected keyword',
			/,' Press return to try again',//)
		call crtin
		if (cmdverbose(-1) <= 2) write (ttyout,*) iopcon
		irtn = 1
		return
	}

501	call wjfren (i,x,il)      # continuum type (L for linear or C for curved)
	if (il == ihcl || il == ihcc) {
		if (il == ihcl) {
			icontype(ifeat,imat) = 0  # linear continuum
			go to 600
		}
		if (il == ihcc) {
			icontype(ifeat,imat) = 1  # curved continuum
			go to 700
		}
	} else {
		call what(i)
		write (ttyout,502)  il
502		format ('ERROR, expected L or C for continuum type, got ', a1)
		irtn=1
		return
	}
		
	if (i >= 80) {
		call crtin
		if (cmdverbose(-1) <= 2) write (ttyout,*) iopcon
		go to 100   # read next line
	}
	go to 101   # continue scanning line

	##################################################
        # LINEAR continuum   linear

600	call wjfren (i,x,il)        # get channel 1
	if (il == ihw) {
		icmode = 1     # values are wavelenmgths
	} else {
		icmode = 0     # values are channels
	}

	if (icmode == 1) { # values in wavelengths
		call wjfren (i,x,il)      # get wav1
		if (il != 0 || x < 0.1e-30) {
			call what(i)
			write (ttyout, 311)
311			format ('ERROR: WAVELENGTH OUT OF RANGE (< 0.1e-30)', //)
			irtn = 1
			return
		}
		wav1 = x

		call wjfren (i,x,il)      # get wav2
		if (il != 0 || x < 0.1e-30) {
			call what(i)
			write (ttyout, 311)
			irtn = 1
			return
		}
		wav2 = x

		call wjfren (i,x,il)      # get wav3
		if (il != 0 || x < 0.1e-30) {
			call what(i)
			write (ttyout, 311)
			irtn = 1
			return
		}
		wav3 = x

		call wjfren (i,x,il)      # get wav4
		if (il != 0 || x < 0.1e-30) {
			call what(i)
			write (ttyout, 311)
			irtn = 1
			return
		}
		wav4 = x
		if (il == ihi) i = i -1  # i set for imaging mode (gotten later)

		ier=12 # set mode for no error is out of bounds
		call wtochbin (dataa, nchans, wav1, wav2, nch1, nch2, ier)
		if (ier != 0) {
			ifeatenable(ifeat,imat) = 0   # disable
			dlweight(ifeat,imat)    = 0.0

			write (ttyout, 354) imat, ifeat, group(imat), tmptitle(1:tmplength)
354			format ('   Material',i6, '  feature ',i3, '  group', i4, '  DISABLED, ID= ', a)

			write (lunhist, 355) ifeat, imat, otitle(imat)
355			format (11x,'WARNING: feature ',i6,' material ',i6, ' is DiSABLED, title=', a)

			if (featimprt(ifeat,imat) == 3) {  # Must be present, and is not, so disable this material in this group

				imatenable(imat) = 0  # disable
				write (ttyout,  353) imat, ifeat, group(imat), tmptitle(1:tmplength)
353				format ('   Material',i6, '  feature ',i3,' is a Must have Diagnostic feature but disabled in group',
					 i4,'  MATERIAL DISABLED, ID= ', a)
				write (lunhist, 353) imat, ifeat, group(imat), tmptitle(1:tmplength)
			}
			irtn = 12
			return
		}

		ier=12 # set mode for no error is out of bounds
		call wtochbin (dataa, nchans, wav3, wav4, nch3, nch4, ier)
		if (ier != 0) {
			ifeatenable(ifeat,imat) = 0   # disable
			dlweight(ifeat,imat)    = 0.0

			write (ttyout, 354) imat, ifeat, group(imat), tmptitle(1:tmplength)

			write (lunhist, 355) ifeat, imat, otitle(imat)

			if (featimprt(ifeat,imat) == 3) {  # Must be present, and is not, so disable this material in this group

				imatenable(imat) = 0  # disable
				write (ttyout,  353) imat, ifeat, group(imat), tmptitle(1:tmplength)
				write (lunhist, 353) imat, ifeat, group(imat), tmptitle(1:tmplength)
			}
			irtn = 12
			return
		}

		if (cmdverbose(-1) <= 1) {
			write (ttyout,309) wav1, wav2, wav3, wav4,
                                   nch1, nch2, nch3, nch4
309			format (' NOTE: Wavelengths      ',
				3(f11.5,4x),f11.5,/,
	                        '  translate to channels:',3(i6,9x),i6,/)
			#write (ttyout,*) 'DEBUG: i=',i,' iopcon=',iopcon
		}
		ifeatenable(ifeat,imat) = 1   # enable feature
		dlweight(ifeat,imat)    = 1.0 # default weight factor
		write (ttyout, 356) ifeat, imat, otitle(imat)
356		format (11x,'ENABLED: feature ',i6,' material ',i6, '  title=', a)

	} else {
	
		nch1=x
		if (nch1 < 1 || nch1 > nchans) {
			call what(i)
			write (ttyout, 310) nch1, nchans
310			format ('ERROR: CHANNEL',i7,'is OUT OF RANGE, min=1, max=',i7, //)
			irtn = 1
			return
		}
		call wjfren (i,x,il)      # get channel 2
		nch2=x
		if (nch2 < 1 || nch2 > nchans) {
			call what(i)
			write (ttyout, 310) nch2, nchans
			irtn = 1
			return
		}
		call wjfren (i,x,il)      # get channel 3
		nch3=x
		if (nch3 < 1 || nch3 > nchans) {
			call what(i)
			write (ttyout, 310) nch3, nchans
			irtn = 1
			return
		}
		call wjfren (i,x,il)      # get channel 4
		nch4=x
		if (nch4 < 1 || nch4 > nchans) {
			call what(i)
			write (ttyout, 310) nch4, nchans
			irtn = 1
			return
		}

		ifeatenable(ifeat,imat) = 1   # enable feature
		dlweight(ifeat,imat)    = 1.0 # default weight factor

		if (il == ihi) i = i -1  # i set for imaging mode (gotten later)
	}

	if (nch1 > nch2 || nch2+1 >= nch3 || nch3 > nch4) {
		write (ttyout,312)
312		format ('ERROR: channels are not in a valid sequence:',/,
			'       If channels = n1 n2 n3 n4, then the following',
					' must hold:',/,
			'       n1 <= n2, n2+1 < n3, and n3 <= n4',//,
			' REENTER ALL POINTS',//)
		irtn = 1
		return
	}
	cchans(1,ifeat,imat) = nch1
	cchans(2,ifeat,imat) = nch2
	cchans(3,ifeat,imat) = nch3
	cchans(4,ifeat,imat) = nch4
	if (nch1 < chfirst) chfirst = nch1
	if (nch4 > chlast ) chlast  = nch4

	go to 5550   # jump over curved continuaa block to set continuaa limits

	################# end of linear continuum block

	################# this is the curved continuum block

700	call wjfren (i,x,il)        # get channel 1
	if (il == ihw) {
		icmode = 1     # values are wavelenmgths
	} else {
		icmode = 0     # values are channels  (NOT ALLOWED AT PRESENT)
	}

	if (icmode == 1) { # values in wavelengths
		call wjfren (i,x,il)      # get wav1
		if (il != 0 || x < 0.1e-30) {
			call what(i)
			write (ttyout, 311)
			irtn = 1
			return
		}
		wav1 = x

		call wjfren (i,x,il)      # get wav2
		if (il != 0 || x < 0.1e-30) {
			call what(i)
			write (ttyout, 311)
			irtn = 1
			return
		}
		wav2 = x

		call wjfren (i,x,il)      # get wav3
		if (il != 0 || x < 0.1e-30) {
			call what(i)
			write (ttyout, 311)
			irtn = 1
			return
		}
		wav3 = x

		call wjfren (i,x,il)      # get wav4
		if (il != 0 || x < 0.1e-30) {
			call what(i)
			write (ttyout, 311)
			irtn = 1
			return
		}
		wav4 = x

		call wjfren (i,x,il)      # get wav5
		if (il != 0 || x < 0.1e-30) {
			call what(i)
			write (ttyout, 311)
			irtn = 1
			return
		}
		wav5 = x

		call wjfren (i,x,il)      # get wav6
		if (il != 0 || x < 0.1e-30) {
			call what(i)
			write (ttyout, 311)
			irtn = 1
			return
		}
		wav6 = x

		call wjfren (i,x,il)      # get wav7
		if (il != 0 || x < 0.1e-30) {
			call what(i)
			write (ttyout, 311)
			irtn = 1
			return
		}
		wav7 = x

		call wjfren (i,x,il)      # get wav8
		if (il != 0 || x < 0.1e-30) {
			call what(i)
			write (ttyout, 311)
			irtn = 1
			return
		}
		wav8 = x
		if (il == ihi) i = i -1  # i set for imaging mode (gotten later)

		ier=12 # set mode for no error is out of bounds
		call wtochbin (dataa, nchans, wav1, wav2, nch1, nch2, ier)
		if (ier != 0) {
			ifeatenable(ifeat,imat) = 0   # disable
			dlweight(ifeat,imat)    = 0.0

			write (ttyout, 354) imat, ifeat, group(imat), tmptitle(1:tmplength)

			write (lunhist, 355) ifeat, imat, otitle(imat)

			if (featimprt(ifeat,imat) == 3) {  # Must be present, and is not, so disable this material in this group

				imatenable(imat) = 0  # disable
				write (ttyout,  353) imat, ifeat, group(imat), tmptitle(1:tmplength)
				write (lunhist, 353) imat, ifeat, group(imat), tmptitle(1:tmplength)
			}
			irtn = 12
			return
		}
		curvcwaves(1,ifeat,imat) = wav1
		curvcwaves(2,ifeat,imat) = wav2
		cvwav(1,ifeat,imat)      = (wav1+wav2)/2.0

		ier=12 # set mode for no error is out of bounds
		call wtochbin (dataa, nchans, wav3, wav4, nch3, nch4, ier)
		if (ier != 0) {
			ifeatenable(ifeat,imat) = 0   # disable
			dlweight(ifeat,imat)    = 0.0

			write (ttyout, 354) imat, ifeat, group(imat), tmptitle(1:tmplength)

			write (lunhist, 355) ifeat, imat, otitle(imat)

			if (featimprt(ifeat,imat) == 3) {  # Must be present, and is not, so disable this material in this group

				imatenable(imat) = 0  # disable
				write (ttyout,  353) imat, ifeat, group(imat), tmptitle(1:tmplength)
				write (lunhist, 353) imat, ifeat, group(imat), tmptitle(1:tmplength)
			}
			irtn = 12
			return
		}
		curvcwaves(3,ifeat,imat) = wav3
		curvcwaves(4,ifeat,imat) = wav4
		cvwav(2,ifeat,imat)      = (wav3+wav4)/2.0

		ier=12 # set mode for no error is out of bounds
		call wtochbin (dataa, nchans, wav5, wav6, nch5, nch6, ier)
		if (ier != 0) {
			ifeatenable(ifeat,imat) = 0   # disable
			dlweight(ifeat,imat)    = 0.0

			write (ttyout, 354) imat, ifeat, group(imat), tmptitle(1:tmplength)

			write (lunhist, 355) ifeat, imat, otitle(imat)

			if (featimprt(ifeat,imat) == 3) {  # Must be present, and is not, so disable this material in this group

				imatenable(imat) = 0  # disable
				write (ttyout,  353) imat, ifeat, group(imat), tmptitle(1:tmplength)
				write (lunhist, 353) imat, ifeat, group(imat), tmptitle(1:tmplength)
			}
			irtn = 12
			return
		}
		curvcwaves(5,ifeat,imat) = wav5
		curvcwaves(6,ifeat,imat) = wav6
		cvwav(3,ifeat,imat)      = (wav5+wav6)/2.0

		ier=12 # set mode for no error is out of bounds
		call wtochbin (dataa, nchans, wav7, wav8, nch7, nch8, ier)
		if (ier != 0) {
			ifeatenable(ifeat,imat) = 0   # disable
			dlweight(ifeat,imat)    = 0.0

			write (ttyout, 354) imat, ifeat, group(imat), tmptitle(1:tmplength)

			write (lunhist, 355) ifeat, imat, otitle(imat)

			if (featimprt(ifeat,imat) == 3) {  # Must be present, and is not, so disable this material in this group

				imatenable(imat) = 0  # disable
				write (ttyout,  353) imat, ifeat, group(imat), tmptitle(1:tmplength)
				write (lunhist, 353) imat, ifeat, group(imat), tmptitle(1:tmplength)
			}
			irtn = 12
			return
		}
		curvcwaves(7,ifeat,imat) = wav7
		curvcwaves(8,ifeat,imat) = wav8
		cvwav(4,ifeat,imat)      = (wav7+wav8)/2.0

		if (cmdverbose(-1) <= 1) {
			write (ttyout,3090) wav1, wav2, wav3, wav4, wav5, wav6, wav7, wav8,
                                   nch1, nch2, nch3, nch4, nch5, nch6, nch7, nch8
3090			format (' NOTE: Wavelengths      ',
				7(f10.5,2x),f10.5,/,
	                        '  translate to channels:',7(i6,6x),i6,/)
			#write (ttyout,*) 'DEBUG: i=',i,' iopcon=',iopcon
			write (ttyout,3092) cvwav(1,ifeat,imat), cvwav(2,ifeat,imat),
						cvwav(3,ifeat,imat), cvwav(4,ifeat,imat)
3092			format (' NOTE: the 4 wavelengths for the splined continuum are:',
				3(f11.5,2x),f11.5,/)
		}
		if ( cvwav(1,ifeat,imat) >= cvwav(2,ifeat,imat) |
			cvwav(2,ifeat,imat) >= cvwav(3,ifeat,imat) |
			cvwav(3,ifeat,imat) >= cvwav(4,ifeat,imat) ) {

			ifeatenable(ifeat,imat) = 0   # disable feature
			dlweight(ifeat,imat)    = 0.0
			write (ttyout,*) 'ERROR: curved continuum averaged waves in each block are not consecutive'
			write (ttyout,*) 'DISABLING feature ', ifeat, ' material ', imat
			irtn = 1
			return
		}

						#### all is good, enable feature
		ifeatenable(ifeat,imat) = 1   # enable feature
		dlweight(ifeat,imat)    = 1.0 # default weight factor

	} else {
	
		write (ttyout,*) Tetracorder is not yet programmed to handle channels for curved continuaa
		irtn = 1
		return
	}

	if (nch1 > nch2 || nch2+1 >= nch3 || nch3 > nch4 || nch5 > nch6 || nch7 > nch8) {
		write (ttyout,314)
314		format ('ERROR: channels are not in a valid sequence:',/,
			'       If channels = n1 n2 n3 n4, then the following',
					' must hold:',/,
			'       n1 <= n2, n2+1 < n3, and n3 <= n4',//,
			' REENTER ALL POINTS',//)
		irtn = 1
		return
	}

        #enter cchans for linear if ever used
	# the following is for the central linear continuum.
	# it is used to determine the central block of channels where
	# emission/absorption feature is defined, and to do
	# linear continuum on the reference spectrum.

	cchans(1,ifeat,imat) = nch3
	cchans(2,ifeat,imat) = nch4
	cchans(3,ifeat,imat) = nch5
	cchans(4,ifeat,imat) = nch6

	#chfirst and chlast define how much of the spectral sheet to read.
	if (nch1 < chfirst) chfirst = nch1
	if (nch4 > chlast ) chlast  = nch4

	# set icurvchans
	icurvchans(1,ifeat,imat) = nch1
	icurvchans(2,ifeat,imat) = nch2
	icurvchans(3,ifeat,imat) = nch3
	icurvchans(4,ifeat,imat) = nch4
	icurvchans(5,ifeat,imat) = nch5
	icurvchans(6,ifeat,imat) = nch6
	icurvchans(7,ifeat,imat) = nch7
	icurvchans(8,ifeat,imat) = nch8

	################# end curved continuum block

#	set default continuaa limits

5550	zcontmn(ifeat,imat) = 0.1e-6
	zcontlmn(ifeat,imat) = 0.1e-6
	zcontrmn(ifeat,imat) = 0.1e-6
	zcontmx(ifeat,imat) = 0.1e+20
	zcontlmx(ifeat,imat) = 0.1e+20
	zcontrmx(ifeat,imat) = 0.1e+20
	zcontlgtr(1,ifeat,imat) = 0.0
	zcontlgtr(2,ifeat,imat) = 0.0
	zcontrgtl(1,ifeat,imat) = 0.0
	zcontrgtl(2,ifeat,imat) = 0.0

	zrcbblcgt(1,ifeat,imat) = -0.1e+9
	zrcbblcgt(2,ifeat,imat) = -0.2e+9
	zrcbblclt(1,ifeat,imat) =  0.1e+9
	zrcbblclt(2,ifeat,imat) =  0.2e+9
	zlcbbrcgt(1,ifeat,imat) = -0.1e+9
	zlcbbrcgt(2,ifeat,imat) = -0.2e+9
        zlcbbrclt(1,ifeat,imat) =  0.1e+9
        zlcbbrclt(2,ifeat,imat) =  0.2e+9

	zrtimesbd(1,ifeat,imat) =  0.1e-9
	zrtimesbd(2,ifeat,imat) =  0.1e-9

	ictlimflg = 0
	ictrlimflg = 0
	ictllimflg = 0
	ictlgtrflg = 0
	ictrgtlflg = 0
	ircbblcgtflg = 0
	ircbblcltflg = 0
	ilcbbrcgtflg = 0
	ilcbbrcltflg = 0

313	call wjfren (i,x,il)      # check if continuum limits set.
                                  # loop back to here until all options found

1000	continue
	#write (ttyout,*) 'DEBUG: statement 1000. i=',i,' x=',x,' il=',il

	if ( ifeatenable(ifeat,imat) == 0) { # disabled

			write (ttyout, 354) imat, ifeat, group(imat), tmptitle(1:tmplength)

			write (lunhist, 355) ifeat, imat, otitle(imat)

			if (featimprt(ifeat,imat) == 3) {  # Must be present, and is not, so disable this material in this group

				imatenable(imat) = 0  # disable
				write (ttyout,  353) imat, ifeat, group(imat), tmptitle(1:tmplength)
				write (lunhist, 353) imat, ifeat, group(imat), tmptitle(1:tmplength)
			}
		irtn = 12
		return
	}
	if ((i < 72) & (il == ihl) & 
			(iopcon(i:i+6) == 'ct/rct>')) {  # lct/rct> n option found
		i = i+7
		call wjfren (i,x,il)
		if (il==ihx || il==ihe) {
			ic=il
			icrst = 1
			call rstart(icrst)
			call what (-1)
			write (ttyout,*) 'exiting getifeat: e or x  in user input not expected'
			irtn = 1
			return
		}
		if (il != 0) {
			call what(i)
			write (ttyout,*) 'ERROR decoding lct/rct>'
			irtn = 1
			return
		}
		zcontlgtr(1,ifeat,imat) = x
		call wjfren (i,x2,il)
		if (il==ihx || il==ihe) {
			ic=il
			icrst = 1
			call rstart(icrst)
			call what (-1)
			write (ttyout,*) 'exiting getifeat: e or x  in user input not expected'
			irtn = 1
			return
		}
		if (il != 0) {
			call what(i)
			write (ttyout,*) 'ERROR decoding lct/rct>'
			irtn = 1
			return
		}
		ictlgtrflg = 1
		zcontlgtr(2,ifeat,imat) = x2
		#write (ttyout,*) 'DEBUG: lct/rct>', x, x2,' ifeat=',ifeat,' material=',imat
	}
2000	if ((i < 72) & (il == ihr) & 
			(iopcon(i:i+6) == 'ct/lct>')) {  # rct/rcl> n option found
		i = i+7
		call wjfren (i,x,il)
		if (il==ihx || il==ihe) {
			ic=il
			icrst = 1
			call rstart(icrst)
			call what (-1)
			write (ttyout,*) 'exiting getifeat: e or x  in user input not expected'
			irtn = 1
			return
		}
		if (il != 0) {
			call what(i)
			write (ttyout,*) 'ERROR decoding rct/lct>'
			irtn = 1
			return
		}
		zcontrgtl(1,ifeat,imat) = x
		call wjfren (i,x2,il)
		if (il==ihx || il==ihe) {
			ic=il
			icrst = 1
			call rstart(icrst)
			call what (-1)
			write (ttyout,*) 'exiting getifeat: e or x  in user input not expected'
			irtn = 1
			return
		}
		if (il != 0) {
			call what(i)
			write (ttyout,*) 'ERROR decoding rct/lct>'
			irtn = 1
			return
		}
		ictrgtlflg = 1
		zcontrgtl(2,ifeat,imat) = x2
		#write (ttyout,*) 'DEBUG: rct/lct>', x, x2,' ifeat=',ifeat,' material=',imat
	}
    	if ((i < 72) & (il == ihr) & 
			(iopcon(i:i+6) == 'ct/lct<')) {  # rct/rcl> n option found
			call what(i)
			write (ttyout,*) 'ERROR rct/lct< is not a valid option.  Use lct/rct>'
			irtn = 1
			return
	}
    	if ((i < 72) & (il == ihl) & 
			(iopcon(i:i+6) == 'ct/rct<')) {  # rct/rcl> n option found
			call what(i)
			write (ttyout,*) 'ERROR lct/rct< is not a valid option.  Use rct/lct>'
			irtn = 1
			return
	}
3000	if ( ifeatenable(ifeat,imat) == 0) { # disabled

			write (ttyout, 354) imat, ifeat, group(imat), tmptitle(1:tmplength)

			write (lunhist, 355) ifeat, imat, otitle(imat)

			if (featimprt(ifeat,imat) == 3) {  # Must be present, and is not, so disable this material in this group

				imatenable(imat) = 0  # disable
				write (ttyout,  353) imat, ifeat, group(imat), tmptitle(1:tmplength)
				write (lunhist, 353) imat, ifeat, group(imat), tmptitle(1:tmplength)
			}
			irtn = 12
			return
	}

	if ((i < 72) & (il == ihr) & 
			(iopcon(i:i+5) == 'cbblc>')) {  # rcbblc> n option found
		i = i+7
		call wjfren (i,x,il)
		if (il==ihx || il==ihe) {
			ic=il
			icrst = 1
			call rstart(icrst)
			call what (-1)
			write (ttyout,*) 'exiting getifeat: e or x  in user input not expected'
			irtn = 1
			return
		}
		if (il != 0) {
			call what(i)
			write (ttyout,*) 'ERROR decoding rcbblc>'
			irtn = 1
			return
		}
		zrcbblcgt(1,ifeat,imat) = x
		call wjfren (i,x2,il)
		if (il==ihx || il==ihe) {
			ic=il
			icrst = 1
			call rstart(icrst)
			call what (-1)
			write (ttyout,*) 'exiting getifeat: e or x  in user input not expected'
			irtn = 1
			return
		}
		if (il != 0) {
			call what(i)
			write (ttyout,*) 'ERROR decoding rcbblc>'
			irtn = 1
			return
		}
		ircbblcgtflg = 1
		zrcbblcgt(2,ifeat,imat) = x2
		#write (ttyout,*) 'DEBUG: rcbblc>', x, x2,' ifeat=',ifeat,' material=',imat
	}
4000	if ( ifeatenable(ifeat,imat) == 0) { # disabled

			write (ttyout, 354) imat, ifeat, group(imat), tmptitle(1:tmplength)

			write (lunhist, 355) ifeat, imat, otitle(imat)

			if (featimprt(ifeat,imat) == 3) {  # Must be present, and is not, so disable this material in this group

				imatenable(imat) = 0  # disable
				write (ttyout,  353) imat, ifeat, group(imat), tmptitle(1:tmplength)
				write (lunhist, 353) imat, ifeat, group(imat), tmptitle(1:tmplength)
			}
		irtn = 12
		return
	}

	if ((i < 72) & (il == ihr) & 
			(iopcon(i:i+5) == 'cbblc<')) {  # rcbblc< n option found
		i = i+7
		call wjfren (i,x,il)
		if (il==ihx || il==ihe) {
			ic=il
			icrst = 1
			call rstart(icrst)
			call what (-1)
			write (ttyout,*) 'exiting getifeat: e or x  in user input not expected'
			irtn = 1
			return
		}
		if (il != 0) {
			call what(i)
			write (ttyout,*) 'ERROR decoding rcbblc<'
			irtn = 1
			return
		}
		zrcbblclt(1,ifeat,imat) = x
		call wjfren (i,x2,il)
		if (il==ihx || il==ihe) {
			ic=il
			icrst = 1
			call rstart(icrst)
			call what (-1)
			write (ttyout,*) 'exiting getifeat: e or x  in user input not expected'
			irtn = 1
			return
		}
		if (il != 0) {
			call what(i)
			write (ttyout,*) 'ERROR decoding rcbblc<'
			irtn = 1
			return
		}
		ircbblcltflg = 1
		zrcbblclt(2,ifeat,imat) = x2
		#write (ttyout,*) 'DEBUG: rcbblc<', x, x2,' ifeat=',ifeat,' material=',imat
	}
5000	if ( ifeatenable(ifeat,imat) == 0) { # disabled

			write (ttyout, 354) imat, ifeat, group(imat), tmptitle(1:tmplength)

			write (lunhist, 355) ifeat, imat, otitle(imat)

			if (featimprt(ifeat,imat) == 3) {  # Must be present, and is not, so disable this material in this group

				imatenable(imat) = 0  # disable
				write (ttyout,  353) imat, ifeat, group(imat), tmptitle(1:tmplength)
				write (lunhist, 353) imat, ifeat, group(imat), tmptitle(1:tmplength)
			}
		irtn = 12
		return
	}

	if ((i < 72) & (il == ihl) & 
			(iopcon(i:i+5) == 'cbbrc>')) {  # rcbblc> n option found
		i = i+7
		call wjfren (i,x,il)
		if (il==ihx || il==ihe) {
			ic=il
			icrst = 1
			call rstart(icrst)
			call what (-1)
			write (ttyout,*) 'exiting getifeat: e or x  in user input not expected'
			irtn = 1
			return
		}
		if (il != 0) {
			call what(i)
			write (ttyout,*) 'ERROR decoding rcbblc>'
			irtn = 1
			return
		}
		zlcbbrcgt(1,ifeat,imat) = x
		call wjfren (i,x2,il)
		if (il==ihx || il==ihe) {
			ic=il
			icrst = 1
			call rstart(icrst)
			call what (-1)
			write (ttyout,*) 'exiting getifeat: e or x  in user input not expected'
			irtn = 1
			return
		}
		if (il != 0) {
			call what(i)
			write (ttyout,*) 'ERROR decoding rcbblc>'
			irtn = 1
			return
		}
		ilcbbrcgtflg = 1
		zlcbbrcgt(2,ifeat,imat) = x2
		#write (ttyout,*) 'DEBUG: lcbbrc>', x, x2,' ifeat=',ifeat,' material=',imat
	}
6000	if ( ifeatenable(ifeat,imat) == 0) { # disabled

			write (ttyout, 354) imat, ifeat, group(imat), tmptitle(1:tmplength)

			write (lunhist, 355) ifeat, imat, otitle(imat)

			if (featimprt(ifeat,imat) == 3) {  # Must be present, and is not, so disable this material in this group

				imatenable(imat) = 0  # disable
				write (ttyout,  353) imat, ifeat, group(imat), tmptitle(1:tmplength)
				write (lunhist, 353) imat, ifeat, group(imat), tmptitle(1:tmplength)
			}
		irtn = 12
		return
	}

	if ((i < 72) & (il == ihl) & 
			(iopcon(i:i+5) == 'cbbrc<')) {  # rcbblc< n option found
		i = i+7
		call wjfren (i,x,il)
		if (il==ihx || il==ihe) {
			ic=il
			icrst = 1
			call rstart(icrst)
			call what (-1)
			write (ttyout,*) 'exiting getifeat: e or x  in user input not expected'
			irtn = 1
			return
		}
		if (il != 0) {
			call what(i)
			write (ttyout,*) 'ERROR decoding rcbblc>'
			irtn = 1
			return
		}
		zlcbbrclt(1,ifeat,imat) = x
		call wjfren (i,x2,il)
		if (il==ihx || il==ihe) {
			ic=il
			icrst = 1
			call rstart(icrst)
			call what (-1)
			write (ttyout,*) 'exiting getifeat: e or x  in user input not expected'
			irtn = 1
			return
		}
		if (il != 0) {
			call what(i)
			write (ttyout,*) 'ERROR decoding rcbblc>'
			irtn = 1
			return
		}
		ilcbbrcltflg = 1
		zlcbbrclt(2,ifeat,imat) = x2
		#write (ttyout,*) 'DEBUG: lcbbrc>', x, x2,' ifeat=',ifeat,' material=',imat
	}
7000	if ( ifeatenable(ifeat,imat) == 0) { # disabled

			write (ttyout, 354) imat, ifeat, group(imat), tmptitle(1:tmplength)

			write (lunhist, 355) ifeat, imat, otitle(imat)

			if (featimprt(ifeat,imat) == 3) {  # Must be present, and is not, so disable this material in this group

				imatenable(imat) = 0  # disable
				write (ttyout,  353) imat, ifeat, group(imat), tmptitle(1:tmplength)
				write (lunhist, 353) imat, ifeat, group(imat), tmptitle(1:tmplength)
			}
		irtn = 12
		return
	}

	if ((i < 72) & (il == ihr) & 
			(iopcon(i:i+3) == '*bd>')) {  # r*bd> n m option found
		i = i+4
		call wjfren (i,x,il)
		if (il==ihx || il==ihe) {
			ic=il
			icrst = 1
			call rstart(icrst)
			call what (-1)
			write (ttyout,*) 'exiting getifeat: e or x  in user input not expected'
			irtn = 1
			return
		}
		if (il != 0) {
			call what(i)
			write (ttyout,*) 'ERROR decoding r*bd>'
			irtn = 1
			return
		}
		zrtimesbd(1,ifeat,imat) = x
		call wjfren (i,x2,il)
		if (il==ihx || il==ihe) {
			ic=il
			icrst = 1
			call rstart(icrst)
			call what (-1)
			write (ttyout,*) 'exiting getifeat: e or x  in user input not expected'
			irtn = 1
			return
		}
		if (il != 0) {
			call what(i)
			write (ttyout,*) 'ERROR decoding r*bd>'
			irtn = 1
			return
		}
		ilcbbrcltflg = 1
		zrtimesbd(2,ifeat,imat) = x2
		#write (ttyout,*) 'DEBUG: r*bd>', x, x2,' ifeat=',ifeat,' material=',imat
	}
	
8000	if ( ifeatenable(ifeat,imat) == 0) { # disabled

			write (ttyout, 354) imat, ifeat, group(imat), tmptitle(1:tmplength)

			write (lunhist, 355) ifeat, imat, otitle(imat)

			if (featimprt(ifeat,imat) == 3) {  # Must be present, and is not, so disable this material in this group

				imatenable(imat) = 0  # disable
				write (ttyout,  353) imat, ifeat, group(imat), tmptitle(1:tmplength)
				write (lunhist, 353) imat, ifeat, group(imat), tmptitle(1:tmplength)
			}
		irtn = 12
		return
	}

	if ((i < 80) & (il == ihc) & (iopcon(i:i) == 't')) {
		  # ct option found
		#write (ttyout,*) 'DEBUG: processing ct option, getting thresholds(s), i=', i
		i = i+1
		call wjfren (i,x,il)
		if (il==ihx || il==ihe) {
			ic=il
			icrst = 1
			call rstart(icrst)
			call what (-1)
			write (ttyout,*) 'exiting getifeat: e or x  in user input not expected'
			irtn = 1
			return
		}
		if (il != 0) {
			call what(i)
			write (ttyout,*) 'ERROR decoding ct min max'
			irtn = 1
			return
		}
		ictlimflg = 1
		zcontmn(ifeat,imat) = x

		call wjfren (i,x,il)  # now see if max continuum is set.
		if (il==ihx || il==ihe) {
			ic=il
			icrst = 1
			call rstart(icrst)
			call what (-1)
			write (ttyout,*) 'exiting getifeat: e or x  in user input not expected'
			irtn = 1
			return
		}
		if (il != 0 & il != ihr & il != ihl) {
			call what (-1)
			write (ttyout,*) 'ERROR decoding ct max'
			irtn = 1
			return
		}
		if (il != 0 & (il == ihr | il == ihl)) {
			#was: i = i -1
			#was: write (ttyout,*) 'DEBUG: another option found. setting i=i-1; now i=', i
			#write (ttyout,*) 'DEBUG: another option found, now i=', i
			# was: go to 313   # found another option, no max
			go to 1000   # found another option, no max
		}
		if (i < 80 & il != ihr & il != ihl) {
			if (x <= zcontmn(ifeat,imat)) {
				call what(i)
				write (ttyout,*) 'ERROR: max is less than min'
				irtn = 1
				return
			}
			ictlimflg = 2
			zcontmx(ifeat,imat) = x
		}
	} 
9000	if ( ifeatenable(ifeat,imat) == 0) { # disabled

			write (ttyout, 354) imat, ifeat, group(imat), tmptitle(1:tmplength)

			write (lunhist, 355) ifeat, imat, otitle(imat)

			if (featimprt(ifeat,imat) == 3) {  # Must be present, and is not, so disable this material in this group

				imatenable(imat) = 0  # disable
				write (ttyout,  353) imat, ifeat, group(imat), tmptitle(1:tmplength)
				write (lunhist, 353) imat, ifeat, group(imat), tmptitle(1:tmplength)
			}
		irtn = 12
		return
	}

	if ((i < 79) & (il == ihr) &
			(iopcon(i:i) == 'c') & 
				(iopcon(i+1:i+1) == 't')) {  # rct option found
		i = i+2
		call wjfren (i,x,il)
		if (il==ihx || il==ihe) {
			ic=il
			icrst = 1
			call rstart(icrst)
			call what (-1)
			write (ttyout,*) 'exiting getifeat: e or x  in user input not expected'
			irtn = 1
			return
		}
		if (il != 0) {
			call what(i)
			write (ttyout,*) 'ERROR decoding rct min max'
			irtn = 1
			return
		}
		ictrlimflg = 1
		zcontrmn(ifeat,imat) = x

		call wjfren (i,x,il)  # now see if max continuum is set.
		if (il==ihx || il==ihe) {
			ic=il
			icrst = 1
			call rstart(icrst)
			call what (-1)
			write (ttyout,*) 'exiting getifeat: e or x  in user input not expected'
			irtn = 1
			return
		}
		if (il != 0 & il != ihc & il != ihl) {
			call what (-1)
			write (ttyout,*) 'ERROR decoding rct max'
			irtn = 1
			return
		}
		if (il != 0 & (il == ihr | il == ihl)) {
			#was: i = i -1
			#was: go to 313   # found another option, no max
			go to 1000   # found another option, no max
		}
		if (i < 80 & il != ihr & il != ihl) {
			if (x <= zcontrmn(ifeat,imat)) {
				call what(i)
				write (ttyout,*) 'ERROR 2 decoding rct max'
				irtn = 1
				return
			}
			ictrlimflg = 2
			zcontrmx(ifeat,imat) = x
		}
	} 
10000	if ( ifeatenable(ifeat,imat) == 0) { # disabled

			write (ttyout, 354) imat, ifeat, group(imat), tmptitle(1:tmplength)

			write (lunhist, 355) ifeat, imat, otitle(imat)

			if (featimprt(ifeat,imat) == 3) {  # Must be present, and is not, so disable this material in this group

				imatenable(imat) = 0  # disable
				write (ttyout,  353) imat, ifeat, group(imat), tmptitle(1:tmplength)
				write (lunhist, 353) imat, ifeat, group(imat), tmptitle(1:tmplength)
			}
		irtn = 12
		return
	}

	if ((i < 79) & (il == ihl) &
			(iopcon(i:i) == 'c') & 
				(iopcon(i+1:i+1) == 't')) {  # lct option found
		i = i+2
		call wjfren (i,x,il)
		if (il==ihx || il==ihe) {
			ic=il
			icrst = 1
			call rstart(icrst)
			call what (-1)
			write (ttyout,*) 'exiting getifeat: e or x  in user input not expected'
			irtn = 1
			return
		}
		if (il != 0) {
			call what(i)
			write (ttyout,*) 'ERROR decoding lct min max'
			irtn = 1
			return
		}
		ictllimflg = 1
		zcontlmn(ifeat,imat) = x

		call wjfren (i,x,il)  # now see if max continuum is set.
		if (il==ihx || il==ihe) {
			ic=il
			icrst = 1
			call rstart(icrst)
			call what (-1)
			write (ttyout,*) 'exiting getifeat: e or x  in user input not expected'
			irtn = 1
			return
		}
		if (il != 0 & il != ihc & il != ihl) {
			call what (-1)
			write (ttyout,*) 'ERROR decoding lct max'
			irtn = 1
			return
		}
		if (il != 0 & (il == ihr | il == ihl)) {
			#was: i = i -1
			#was: go to 313   # found another option, no max
			go to 1000   # found another option, no max
		}
		if (i < 80 & il != ihr & il != ihl) {
			if (x <= zcontlmn(ifeat,imat)) {
				call what(i)
				write (ttyout,*) 'ERROR 2 decoding lct max'
				irtn = 1
				return
			}
			ictllimflg = 2
			zcontlmx(ifeat,imat) = x
		}
	}
11000	if ( ifeatenable(ifeat,imat) == 0) { # disabled

			write (ttyout, 354) imat, ifeat, group(imat), tmptitle(1:tmplength)

			write (lunhist, 355) ifeat, imat, otitle(imat)

			if (featimprt(ifeat,imat) == 3) {  # Must be present, and is not, so disable this material in this group

				imatenable(imat) = 0  # disable
				write (ttyout,  353) imat, ifeat, group(imat), tmptitle(1:tmplength)
				write (lunhist, 353) imat, ifeat, group(imat), tmptitle(1:tmplength)
			}
		irtn = 12
		return
	}

	if (i < 72 & il == ihi & iopcon(i-1:i+4) == 'ignore') {

		# ZZZZ Future: find ignore intervals
		# and delete those channels
		i = 80
	}

12000	if ( ifeatenable(ifeat,imat) == 0) { # disabled

			write (ttyout, 354) imat, ifeat, group(imat), tmptitle(1:tmplength)

			write (lunhist, 355) ifeat, imat, otitle(imat)

			if (featimprt(ifeat,imat) == 3) {  # Must be present, and is not, so disable this material in this group

				imatenable(imat) = 0  # disable
				write (ttyout,  353) imat, ifeat, group(imat), tmptitle(1:tmplength)
				write (lunhist, 353) imat, ifeat, group(imat), tmptitle(1:tmplength)
			}
		irtn = 12
		return
	}

	if (i < 72 & il == ihm & iopcon(i-1:i+2) == 'move') {

		# ZZZZ future
		i = 80
	}

13000	if ( ifeatenable(ifeat,imat) == 0) { # disabled

			write (ttyout, 354) imat, ifeat, group(imat), tmptitle(1:tmplength)

			write (lunhist, 355) ifeat, imat, otitle(imat)

			if (featimprt(ifeat,imat) == 3) {  # Must be present, and is not, so disable this material in this group

				imatenable(imat) = 0  # disable
				write (ttyout,  353) imat, ifeat, group(imat), tmptitle(1:tmplength)
				write (lunhist, 353) imat, ifeat, group(imat), tmptitle(1:tmplength)
			}
		irtn = 12
		return
	}

	if (i < 72 & il == ihw & iopcon(i-1:i+5) == 'weight*') {

		i=i+6
		call wjfren (i,x,il)
		if (il==ihx || il==ihe)  {
			call what(i)
			write (ttyout,*) 'ERROR weight'
			irtn = 1
			return
		}
		if (x > 0.000001) {

			dlweight(ifeat,imat) = x
		} else {
			call what(i)
			write (ttyout,*) 'ERROR weight out of range: too small:', x
			i=80
			irtn = 1
			return

		}
	}

	#### this should be the end of the linear continuum blocko
	go to 14000  # jump to history

14000	if (i < 72 & il == ihm & iopcon(i-1:i+2) == 'mode') {

		# future: add 50 modes, 20 characters each
		# note, mode should loop to end of line
		i = 80
	}

	if (i < 80) go to 313   # loop through all possible options

# write history
	chtmp1 = ' '   # initialize temp array
	ichtmp1 = 1
	ichtmplen = len(chtmp1)

	if (ictlimflg == 1) {
		write (chtmp1(ichtmp1:ichtmplen), 3097) 'ct',
					zcontmn(ifeat,imat)
3097		format (' ',a,f8.4)
		ichtmp1 = ichtmp1 + 11
	} else if (ictlimflg == 2) {
		write (chtmp1(ichtmp1:ichtmplen), 3098) 'ct',
					zcontmn(ifeat,imat),
					zcontmx(ifeat,imat)
3098		format (' ',a,f8.4,1x,f8.4)
		ichtmp1 = ichtmp1 + 20
	}
	if (ictrlimflg == 1) {
		write (chtmp1(ichtmp1:ichtmplen), 3097) 'rct',
					zcontrmn(ifeat,imat)
		ichtmp1 = ichtmp1 + 12
	} else if (ictrlimflg == 2) {
		write (chtmp1(ichtmp1:ichtmplen), 3098) 'rct',
					zcontrmn(ifeat,imat),
					zcontrmx(ifeat,imat)
		ichtmp1 = ichtmp1 + 21
	}
	if (ictllimflg == 1) {
		write (chtmp1(ichtmp1:ichtmplen), 3097) 'lct',
					zcontlmn(ifeat,imat)
		ichtmp1 = ichtmp1 + 12
	} else if (ictrlimflg == 1) {
		write (chtmp1(ichtmp1:ichtmplen), 3098) 'rct',
					zcontlmn(ifeat,imat),
					zcontlmx(ifeat,imat)
		ichtmp1 = ichtmp1 + 21
	}
	if (ictlgtrflg == 1) {
		write (chtmp1(ichtmp1:ichtmplen), 3098) 'lct/rct>',
			zcontlgtr(1,ifeat,imat), zcontlgtr(2,ifeat,imat)
		ichtmp1 = ichtmp1 + 18
	}
	if (ictrgtlflg == 1) {
		write (chtmp1(ichtmp1:ichtmplen), 3098) 'rct/lct>',
			zcontrgtl(1,ifeat,imat), zcontrgtl(2,ifeat,imat)
		ichtmp1 = ichtmp1 + 18
	}
        if (ircbblcgtflg == 1) {
		write (chtmp1(ichtmp1:ichtmplen), 3098) 'rcbblc> ',
			zrcbblcgt(1,ifeat,imat), zrcbblcgt(2,ifeat,imat)
		ichtmp1 = ichtmp1 + 18
	}
        if (ircbblcltflg == 1) {
		write (chtmp1(ichtmp1:ichtmplen), 3098) 'rcbblc< ',
			zrcbblclt(1,ifeat,imat), zrcbblclt(2,ifeat,imat)
		ichtmp1 = ichtmp1 + 18
	}
        if (ilcbbrcgtflg == 1) {
		write (chtmp1(ichtmp1:ichtmplen), 3098) 'lcbbrc> ',
			zlcbbrcgt(1,ifeat,imat), zlcbbrcgt(2,ifeat,imat)
		ichtmp1 = ichtmp1 + 18
	}
        if (ilcbbrcltflg == 1) {
		write (chtmp1(ichtmp1:ichtmplen), 3098) 'lcbbrc< ',
			zlcbbrclt(1,ifeat,imat), zlcbbrclt(2,ifeat,imat)
		ichtmp1 = ichtmp1 + 18
	}
	#
	# now compress blanks
	#
	chtmp2 = ' '
	chtmp2(1:1) = chtmp1(1:1)
	itmp2 = 1
	do itmp1 = 2, ichtmplen {

		if (chtmp1(itmp1:itmp1) != ' ') {
			chtmp2(itmp2:itmp2) = chtmp1(itmp1:itmp1)
			itmp2 = itmp2 + 1
		} else if (chtmp1(itmp1-1:itmp1-1) == ' ' & 
				chtmp1(itmp1:itmp1)  == ' ') {
				next
		} else {
			chtmp2(itmp2:itmp2) = chtmp1(itmp1:itmp1)
			itmp2 = itmp2 + 1
		}
	}

	if (icmode == 1) { # values in wavelengths
		write (lunhist, 3091) imch(featimprt(ifeat,imat)+1),
					wav1, wav2, wav3, wav4,
					chtmp2(1:itmp2), ihbcksl,
					ihbcksl, nch1, nch2, nch3, nch4
3091		format (a,'w',4(f8.4,1x),a,1x,a,
				'# continuum wavelengths',
				/,a,'#',31x,'(continuum channels=)',
				4(i4,' '))
	} else {
		write (lunhist, 3094) imch(featimprt(ifeat,imat)+1),
					nch1, nch2, nch3, nch4,
					chtmp2(1:itmp2),
					ihbcksl, imat, ifeat,
					ihbcksl, dataa(nch1), dataa(nch2),
					dataa(nch3), dataa(nch4)
3094		format (a,4(i5,' '),1x,a,2x,a,'# continuum chanls for Mat.:',
				i3,' Feat:',i3,/,
				a,'#',33x, a,'waves:',4(f7.4,' '))
	}


###########################################################
#     parameter description for bdm set call:
#        INPUT:
#           dataa = wavelengths
#           data  = reference library spectrum  R*4 (nch4 elements)
#           nch1   = continuum point begin on left side of band  I*4
#           nch2    = continuum point end on left side of band  I*4
#                      note: nch2 >= nch1  (checked)
#           nch3    = continuum point begin on right side of band  I*4
#                      note: nch3 > nch2 + 1 (checked)
#           nch4    = continuum point end on right side of band  I*4
#                      note: nch4 >= nch3 (checked)
#                      note: nch4 also determines the max array sizes
#        OUTPUT:
#           refcrm = reference library spectrum, continuum
#                                    removed  R*4 (nch4 elements)
#           minch  = minimum channel in the reference library spectrum.
#                    This is defined to be the band minimum.
#           maxch  = maximum channel in the reference library spectrum.
#                    This is defined to be the band maximum.
#           ifeattype = -1 is an emission feature
#                     =  1 is an absorption band
#           ier    = error detected in input:
#                       = 0 no error
#                       = 1 error
#   
#   For curved continuum with4 continuum intervals, 8 nch values,
#   use nch3, nch4, nch5, nch6 for the central group

	if ( icontype(ifeat,imat) == 0 ) {  # straight line continuum

		nch1left = nch1
		nch2left = nch2
		nch3right = nch3
		nch4right = nch4

	} else {   # curved continuum, so use central 4 channels
			# nch1, nch2, nch7, nch8 are the outer used to
			# define the curvature of the continuum.

		nch1left = nch3
		nch2left = nch4
		nch3right = nch5
		nch4right = nch6

	}

	do ijj = 1, nchans {
		rlbc(ijj,ifeat,imat) = 0.0
	}

	ier=12 # set mode for no error is out of bounds
	call bdmset (dataa,data,nch1left,nch2left,nch3right,nch4right,
                           refcrm,minch,maxch,ifeattype,ier)
	if (ier != 0) {
		ifeatenable(ifeat,imat) = 0   # disable
			dlweight(ifeat,imat)    = 0.0

			write (ttyout, 354) imat, ifeat, group(imat), tmptitle(1:tmplength)

			write (lunhist, 355) ifeat, imat, otitle(imat)

			if (featimprt(ifeat,imat) == 3) {  # Must be present, and is not, so disable this material in this group

				imatenable(imat) = 0  # disable
				write (ttyout,  353) imat, ifeat, group(imat), tmptitle(1:tmplength)
				write (lunhist, 353) imat, ifeat, group(imat), tmptitle(1:tmplength)
			}
		irtn = 12
		return
	}

#was	if (ier != 0) {
#		write (ttyout, 320)
#320		format ('ERROR in band mapping library setup',/,
#			'Press return to start over',///)
#		call what (-1)
#		#go to 210  # was 210 8/2014, changed to 69
#		go to 69
#	}

	do ijj = nch1left, nch4right {
		rlbc(ijj,ifeat,imat) = refcrm(ijj)
	}
	nchmin(ifeat,imat)= minch
	nchmax(ifeat,imat)= maxch
	nftype(ifeat,imat)= ifeattype
	if (ifeattype == -1) {
		if (cmdverbose(-1) <= 1) {
			write (ttyout,*) 'Emisssion feature ',
					'maximum channel=', maxch
		}
	} else if (ifeattype == 1) {
		if (cmdverbose(-1) <= 1) {
			write (ttyout,*) 'Absorption feature ',
					'maximum channel=', minch
		}
	} else {
		write (ttyout,*) 'ERROR: invalid feature type'
		write (ttyout,*) ' '
		call what (-1)
		irtn = 1
		return
	}
	nsp1 = nch2left +1  # first point in band not part of continuum
	nsp2 = nch3right -1  # last point in band not part of continuum
	area = 0.0
	do ijj = nsp1, nsp2 {
		if (refcrm(ijj) != -1.23e34) {
			area = area + abs(1.0 - refcrm(ijj)) # integrate absolute
							# area under contin
		}
	}
	if (  area != area )  {  # NaN check

		area = 0.0
		ifeatenable(ifeat,imat) = 0   # disable
			dlweight(ifeat,imat)    = 0.0
	}

	if (area <= 0.0) {
		write (ttyout, 322) area, ifeat, imat
		write (lunhist, 322) area, ifeat, imat
322		format (' ERROR: area of continuum removed',
			' absorption is < 0:',1pe10.4,' ifeat:',
			i4,' imat:',i4)
		ifeatenable(ifeat,imat) = 0   # disable
			dlweight(ifeat,imat)    = 0.0
	}
	if (featimprt(ifeat,imat) != 1) {  # do not count weak features
		dl(ifeat,imat) = area * dlweight(ifeat,imat)
		# do this in reflsetup 9/16/2015 # dlbar(imat) = dlbar(imat) + dl(ifeat,imat)
		# the above dlbar line gets executed multiple times for each continuation line
		#    so it is moved to reflsetup and executed after all features are fully defined.
		ndl = ndl + 1
	} else {
		dl(ifeat,imat) = 0.0  # weak feature: set area to zero
				      # even though it is finite.  This
				      # effectively takes it out of any
				      # calculations.
	}
	if ( dl(ifeat,imat) != dl(ifeat,imat) )  {     # NaN check

		dl(ifeat,imat) = 0.0
		ifeatenable(ifeat,imat) = 0   # disable
			dlweight(ifeat,imat)    = 0.0
	}


	go to 101   # continue scanning line

69	irtn = 1   # error
	return

	end

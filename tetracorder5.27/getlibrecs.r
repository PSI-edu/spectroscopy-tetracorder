	subroutine getlibrecs (imat, irtn)

	implicit integer*4 (i-n)

#ccc  name:         reflsetup
#ccc  version date: 
#ccc  author(s): Roger N. Clark
#ccc  language: ratfor
#ccc
#ccc  short description: sets up the reference library features
#ccc                     for tetracorder to map with.
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

#RED
	integer*4 ihchar     # function
	logical*4 fexist     # file exists: true, false if doesn't

	integer*4 cmdverbose   # function cmdverbose
	integer*4 fnb          # function fnb
	integer*4 lnb          # function lnb
	integer*4 ictlimflg, ictrlimflg, ictllimflg
	integer*4 ichtmp2, ichtmp1, ndl
	integer*4 itmpfr, nfr, irtn

	character*1 imch(5)
	character*20 chtest
	character*110 chtmp1, chtmp2
	character cht1*3, cht2*5, cht3*2
	integer*4 imat   # material number

	real*4 x

# define feature importance characters

	imch(1) = 'O'
	imch(2) = 'W'
	imch(3) = 'D'
	imch(4) = 'M'
	imch(5) = '?'

	irtn=0

	itmp =cmdverbose(-1)
######	write (ttyout,*) 'DEBUG: cmdverbose(-1) = ',itmp


###########################################################
#######   define library records
###########################################################

2811	if (cmdverbose(-1) <= 1) write (ttyout,2821)
2821	format (///,' type: define library records',/)
              #                      1111111111222
              #             1234567890123456789012
	call crtin
	if (cmdverbose(-1) <= 2) write (ttyout,*) iopcon
	i = 1
	call wjfren (i,x,il)
	if (i >= 80 & il == 0) go to 2811   # blank line, read again
	if (il==ihx || il==ihe)  {
		ic=il
		icrst = 1
		call rstart(icrst)
		call what (-1)
		return
	}
	# 
	i = i -1
	if (iopcon(i:i+21) != 'define library records') {

		call what(i)
		go to 2811
	}


###########################################################
#     ********************************************
#     * get library data file
#     ********************************************
# command block looks like this:
# define library records 
# 
#   a SMALL:  [splib06]  6216 d      \# file ID, rec no.    1 to 2171 channels
#   a MEDIUM: [splib06]  xxxx d      \# file ID, rec no. 2172 to 4852 channels
#   a LARGE:  [splib06]  xxxx d      \# file ID, rec no. larger_chans_future
#            \# need to have more than one reference spectrum per test, e.g.
#            \# use an ASD and a Nicolet convolved spectrum, and different
#            \# features in each.  label spectra: a, b, c  allow max 3 spectra
#   b SMALL:  [splib06]  6216 d      \# file ID, rec no.    1 to 2171 channels
#   b MEDIUM: [splib06]  xxxx d      \# file ID, rec no. 2172 to 4852 channels
#   b LARGE:  [splib06]  xxxx d      \# file ID, rec no. larger_chans_future
# endlibraryrecords

	inumalt = 0    # number of alternate library lines processed
	iflgalt = 0    # alternate library entry found
	chdltflg=' '   # default: no delete points
	icurlib=1      # which library is being loaded
	do iitmp = 1, maxlibs {   # Initialize

		ndevr(iitmp,imat) = 0     # specpr device letter ids, lib
		nrrec(iitmp,imat) = 0     # specpr rec nos for lib spec
		isplibs(iitmp,imat) = 0  # pointers to which rlb each spectrum came from
	}

69      if (cmdverbose(-1) <= 1) write(ttyout,70) imat
70      format (///,40('-'),/,
			' Enter:',/,
			' Alternate Library letetr (a, b, c, ...)  Name: ',
			'file id and record number for',
			' REFERENCE SPECTRUM',i4,/,
                        5x,'(then an optional  d  to delete points)',/,
			5x,'Example:   a SMALL: v 23 d',/,
			1x, 'Enter:  endlibraryrecords  to complete library record definitions',/,
			' or e or x to exit.'/)

	call crtin
	if (cmdverbose(-1) <= 2) write (ttyout,*) iopcon
	i = 1
	call wjfren (i,x,il)
	if (i >= 79 & il == 0) go to 69    # blank line, read again
	if ((il==ihx ) & (iopcon(i:i) == ' ')) {
		ic=il
		icrst = 1
		call rstart(icrst)
		return
	}

###########################################################
#######   endlibraryrecords
###########################################################

	if (i > 1 & i < 62) {   # keyword can be in range
	  if (iopcon(i-1:i+16) == 'endlibraryrecords') {

		if (iflgalt == 0) {
			call what(i)
			write (ttyout,*) 'ERROR: endlibraryrecords keyword found, but no library entries have been entered'
			irtn=1
			return
		}
		return   # everything id good
	  }
	}


###########################################################
#######   regular library entry processing
###########################################################

	jsplib = 0
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
		write (ttyout,33) il
33		format (' ERROR: library letter code out of range', a1)
		call what(i)
		go to 69
	}
	#write (ttyout,*) 'DEBUG: alternate index code = ', jsplib,' i=',i

	# get alternate library name (e.g. SMALL, MDEIUM, AVIRIS, ...)

	call wjfren (i,x,il)       # now get the alternmate library name to use
	if (i >= 79 && il == 0) {
		call what(i)
		write (ttyout,*) 'ERROR: no alternate libraye name found, wanted:', altuse
		go to 69
	}

	i = i -1
	ilast = index (iopcon,':')
	if (ilast == 0) {
		call what(i)
		write (ttyout,*) 'ERROR: no ":" after alternate keyword, wanted:', altuse,':'
		go to 69
	}
	chtest = ' '
	chtest = iopcon(i:ilast-1)
		#write (ttyout,*) 'DEBUG: iopcon, 1, ilast-1:', iopcon(i:ilast-1), i, ilast-1
	i = ilast +1
	do iitmp = 1, numaltlib {  # check if keyword is valid alt lib

		if (chtest == altlib(iitmp)) {

			iflgalt = 1
		}
			
	}
	if (iflgalt == 0) { # invalid keyword
		call what(-1)
		write (ttyout,*) 'ERROR: ',chtest,' is not recognised',
				' as an alternate library'
		go to 69
	}
	if (chtest != altuse) {          # not this entry, so go to next
		write (lunhist,*) iopcon
		inumalt = inumalt + 1
		if (inumalt < numaltlib) {

			if (cmdverbose(-1) <= 1) {
				itmp1=lnb(chtest)
				itmp2=lnb(altuse)
				write(ttyout,67) chtest(1:itmp1),
						altuse(1:itmp2)
67				format ('alternate library ',a,
					'is not the in use alternate library',
					a,' select another')
			}
			go to 69
		}
		itmp1=lnb(chtest)
		itmp2=lnb(altuse)
		if (cmdverbose(-1) <= 1) {
			write(ttyout,671) chtest(1:itmp1),
				altuse(1:itmp2)
671			format ('alternate library ',a,
			'is not the "in use alternate library" ',
					a,/,' going to next command line')
		}
		go to 69
	} else {
		if (cmdverbose(-1) <= 1) {
			itmp1=lnb(chtest)
			itmp2=lnb(altuse)
			write (ttyout,*) 'Using ',altuse(1:itmp2)
		}
	}

	inumalt = inumalt + 1

	call wjfren(i,x1,idevb)
	if (i >= 79 && idevb == 0) {
		call what(i)
		go to 69
	}

	call wjfren(i,xfilb,ic2)

	if (ic2 == ihd) {    # want to delete points
		if (cmdverbose(-1) <= 1) write (ttyout,*) 'DEBUG: deleted point flag set'
		chdltflg='d'
		ic2=0
	}

#        *** check for hard or soft exit ***
	if (ic2==ihx || ic2==ihe)  {
		ic=il
		icrst = 1
		call rstart(icrst)
		call what (-1)
		irtn=1
		return
	} else if (idevb==ihx || ic2==ihx) {
		ic=il
		icrst = 1
		call rstart(icrst)
		call what (-1)
		irtn=1
		return

#       *** check for invalid input ***
	} else if (x1!=0.0 || ic2!=0 || xfilb<=0.0) {
		call what(i)
		write(ttyout,66)
66		format ('invalid input, reenter')
		go to 69

#       *** looks ok so get record number ***
	} else {
		ifilb = xfilb
		call devok (4,idevb,ifilb,lun,ier)
		if (ier!=0) {
			write(ttyout,66)
			call what(i)
			go to 69
		}
		itmp = ifilb
		call redfil(itmp,lun,ier)
		if (ier != 0) {
			go to 69
		}
		iform = 1
		if (cmdverbose(-1) <= 2) {
			write (ttyout,68) imat, idevb, ifilb, ititl
		}
68		format ('Reference spectrum:',i4,'  ',a,i7,3x,a)
		if (cmdverbose(-1) <= 1) write (ttyout,*) ' '
		call namdev (idevb, inamr)

		ndevr(jsplib,imat) = idevb
		nrrec(jsplib,imat) = ifilb
		mtitle(jsplib,imat) = ititl
		isplibs(icurlib,imat) = jsplib

		do ijm = 1, nchans {

			rlb(ijm,jsplib,imat) = data(ijm)
		}

		icurlib = icurlib + 1          
	}
	call wjfren(i,x1,ic2)  # check for delete points indicator
	if (ic2 == ihd) {    # want to delete points
		if (cmdverbose(-1) <= 1) write (ttyout,*) 'DEBUG: deleted point flag set'
		chdltflg='d'
		ic2=0
	}

# write history
	itmp2=lnb(altuse)
	write (lunhist,3050) altuse(1:itmp2), idevb, ifilb,
				chdltflg, ihbcksl,
				 imat, ihbcksl, ititl
3050	format (a,3x,a1,i6,1x,a,15x,a,
		'# file ID, rec no. for material',i5,/,
		a,'#',11('=-'),' TITLE=',a)

	go to 69   # get more alt lib lines

	end

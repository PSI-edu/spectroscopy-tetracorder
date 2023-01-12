	subroutine reflsetup

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
	integer*4 imode12  # mode for cases where features defined but get disabled (imod12=12)
	integer*4 nfen

	real*4 x, xsum

# define feature importance characters

	imch(1) = 'O'
	imch(2) = 'W'
	imch(3) = 'D'
	imch(4) = 'M'
	imch(5) = '?'

	itmp =cmdverbose(-1)
######	write (ttyout,*) 'DEBUG: cmdverbose(-1) = ',itmp

###########################################################
120	if (cmdverbose(-1) <= 1) write (ttyout,125)
125	format (///,' Reference library specpr output:',/,
		'Enter:',/,
		10x,'liboutput none   or',/,
		10x,'liboutput new-file-name (NOT ENABLED YET)')
	call crtin
	if (cmdverbose(-1) <= 2) write (ttyout,*) iopcon
	i = 1
	call wjfren (i,x,il)
	if (i >= 80 & il == 0) go to 120    # blank line, read again
	if (il==ihx || il==ihe)  {
		ic=il
		icrst = 1
		call rstart(icrst)
		call what (-1)
		return
	}
	# put liboutput stuff here in FUTURE
	write (lunhist,126) 'liboutput none'
126	format (a)

###########################################################
130	if (cmdverbose(-1) <= 1) write (ttyout,135)
135	format (///,' Reference library specpr features output:',/,
		'Enter:',/,
		10x,'featoutput none   or',/,
		10x,'featoutput new-file-name (NOT ENABLED YET)')
	call crtin
	if (cmdverbose(-1) <= 2) write (ttyout,*) iopcon
	i = 1
	call wjfren (i,x,il)
	if (i >= 80 & il == 0) go to 130    # blank line, read again
	if (il==ihx || il==ihe)  {
		ic=il
		icrst = 1
		call rstart(icrst)
		call what (-1)
		return
	}
	# put featoutput stuff here in FUTURE
	write (lunhist,136) 'featoutput none'
136	format (a)

###########################################################
#     do in shell script driver
#1301	if (cmdverbose(-1) <= 1) write (ttyout,1351)
#1351	format (///,' Sound files directory path',/,
#		'Enter:',/,
#		10x,'soundpath "dir-path"')
#	call crtin
	if (cmdverbose(-1) <= 2) write (ttyout,*) iopcon
#	i = index(iopcon,'soundpath')
#	if ( i == 0) {
#		write (ttyout,*) 'ERROR: no soundpath keyword found'
#		go to 1301
#	}
#	i = i+9
#	call wjfren (i,x,il)
#	i = i - 1
#	isfirst = i
#	if (i > 78) {
#		write (ttyout,*)'ERROR: no sound file name'
#		go to 1301
#	}
#	do jj = i, 80 {
#		if (iopcon(jj:jj) == ' ') break # find next blank
#	}
#	islast = j - 1
#	pathsnd = iopcon(isfirst:islast)
#	#
#	write (lunhist,1361) islast
#1361	format ('soundpath ', a)
#
###########################################################
140	if (cmdverbose(-1) <= 1) write (ttyout,145) maxaltlib
145	format (' Enter the number of alternate ',
			'library reference data sets',/,
		10x,'range: 1 to',i4)
	call crtin
	if (cmdverbose(-1) <= 2) write (ttyout,*) iopcon
	i = 1
	call wjfren (i,x,il)
	if (i >= 80 & il == 0) go to 140    # blank line, read again
	if (il==ihx || il==ihe)  {
		ic=il
		icrst = 1
		call rstart(icrst)
		call what (-1)
		return
	}
	if (il != 0) {
		call what(i)
		go to 210
	}
	if (x < 1 || x > maxaltlib) {
		write (ttyout,148) maxaltlib
148		format ('ERROR: Number of alternates must be',
			' > 1 and <= ',i4,/,
                        18x,'(Note: this limit can be inceased by a ',
				'simple recompile',/,
			25x,'of the program.  See your software manager)')
		go to 140
	}
	numaltlib = x + 0.5
	write (lunhist,149) numaltlib, ihbcksl
149	format (i5, 20x,a1,'# munber of alternate libraries')

	do itmp = 1, numaltlib {
150		if (cmdverbose(-1) <= 1) write (ttyout,155) itmp
155		format (' Enter the alternate ',
			'library reference name',i4)
		call crtin
		if (cmdverbose(-1) <= 2) write (ttyout,*) iopcon
		i = 1
		call wjfren (i,x,il)
		ifirst=i-1
		call wjfren (i,x,il2)
		if ((il==ihx | il==ihe) & il2 == 0)  {
			ic=il
			icrst = 1
			call rstart(icrst)
			call what (-1)
			return
		}
		ialen = len(altlib(itmp))
		ilast = lnb(altlib(itmp))
		if (ilast - ifirst + 1 > ialen) {
			call what(ifirst+ialen)
			write (ttyout,*) 'ERROR: length too long, reenter'
			go to 150
		}
		altlib(itmp) = ' '
		altlib(itmp) = iopcon(ifirst:ilast)

#############FUTURE FIX: add : after name (watch blanks

		write (lunhist,156) altlib(itmp),ihbcksl
156		format (a, 10x,a1,'# alternate library name')

	}

###########################################################
# RED - 07/07/2008 - Removed extraneous comma at end of 158 statement
157	if (cmdverbose(-1) <= 1) write (ttyout,158)
158	format (' Enter:  use alternate-name ',/,
			20x,'where alternate-name is one of ',
				'the above',/,
			20x,'alternate library reference data sets')
	call crtin
	if (cmdverbose(-1) <= 2) write (ttyout,*) iopcon
	i = 1
	call wjfren (i,x,il)
	if (i >= 80 & il == 0) go to 157    # blank line, read again
	if (il==ihx || il==ihe)  {
		ic=il
		icrst = 1
		call rstart(icrst)
		call what (-1)
		return
	}
	ifirst = i - 1
	if (iopcon(ifirst:ifirst+2) != 'use') {
		call what(i)
		write (ttyout,*) 'ERROR: "use" keyword not found'
		go to 157
	}
	i = i+3
	call wjfren (i,x,il)
	i = i-1
	ilast = lnb(iopcon(i:80))
	if (ilast == 0) {
		call what(i)
		write (ttyout,*) 'ERROR: no alternate keyword found'
		go to 157
	}
	altuse = ' '
	altuse = iopcon(i:i+ilast)
	write (lunhist,159) altuse, ihbcksl
159	format ('use ',a,10x,a1,'# alternate library to use')

###########################################################
160	if (cmdverbose(-1) <= 1) write (ttyout,165) maxgrp, maxcse
165	format (' Enter the number of groups and ',
			'cases:',
		10x,'range: 1 to',i7,' groups (does not include group zero)',/,
		10x,'range: 0 to',i7,' cases  (first case  is one)')
	call crtin
	if (cmdverbose(-1) <= 2) write (ttyout,*) iopcon
	i = 1
	call wjfren (i,x,il)
	if (i >= 80 & il == 0) go to 160    # blank line, read again
	if (il==ihx || il==ihe)  {
		ic=il
		icrst = 1
		call rstart(icrst)
		call what (-1)
		return
	}
	if (il != 0) {
		call what(i)
		go to 160
	}
	if (x < 1 || x > maxgrp) {
		write (ttyout,167) maxgrp
167		format ('ERROR: Number of groups must be',
			' > 1 and <= ',i4,/,
                        18x,'(Note: this limit can be inceased by a ',
				'simple recompile',/,
			25x,'of the program.  See your software manager')
		go to 160
	}
	ngroups = x + 0.5
	call wjfren (i,x,il)
	if (il != 0) {
		call what(i)
		go to 160
	}
	if (x < 0 || x > maxcse) {
		write (ttyout,168) maxcse
168		format ('ERROR: Number of cases must be',
			' 0 to <= ',i4,/,
                        18x,'(Note: this limit can be inceased by a ',
				'simple recompile',/,
			25x,'of the program.  See your software manager')
		go to 160
	}
	ncases = x + 0.5
	write (lunhist,169) ngroups, ncases, ihbcksl
169	format (i5,3x,i5, 10x,a1,'# munber of groups and cases')

	do itmp = 1, ngroups {
170		if (cmdverbose(-1) <= 1) write (ttyout,175) itmp
175		format (' Enter the group ', i5,
			' directory path name',i4)
		call crtin
		if (cmdverbose(-1) <= 2) write (ttyout,*) iopcon
		i = 1
		call wjfren (i,x,il)
		if (i >= 80 & il == 0) go to 170    # blank line, read again
		ifirst=i-1
		call wjfren (i,x,il2)
		if ((il==ihx | il==ihe) & il2 == 0)  {
			ic=il
			icrst = 1
			call rstart(icrst)
			call what (-1)
			return
		}
		ialen = len(pathgrp(itmp))
		ilast = lnb(iopcon)
		if (ilast - ifirst + 1 > ialen) {
			call what(ifirst+ialen)
			write (ttyout,*) 'ERROR: length too long, reenter'
			go to 170
		}
		pathgrp(itmp) = ' '
		pathgrp(itmp) = iopcon(ifirst:ilast)
		lengdir(itmp) = ilast - ifirst + 1

		write (lunhist,176) pathgrp(itmp)(1:lengdir(itmp)),
					ihbcksl,itmp
176		format (a, 10x,a1,'# group',i5,' path name')

	}

	if (ncases > 0) {
		do itmp = 1, ncases {
1801			if (cmdverbose(-1) <= 1) write (ttyout,1851) itmp
1851			format (' Enter the case ', i5,
				' directory path name',i4)
			call crtin
			if (cmdverbose(-1) <= 2) write (ttyout,*) iopcon
			i = 1
			call wjfren (i,x,il)
			if (i >= 80 & il == 0) go to 1801  # blank line, read again
			ifirst=i-1
			call wjfren (i,x,il2)
			if ((il==ihx | il==ihe) & il2 == 0)  {
				ic=il
				icrst = 1
				call rstart(icrst)
				call what (-1)
				return
			}
			ialen = len(pathcase(itmp))
			ilast = lnb(iopcon)
			if (ilast - ifirst + 1 > ialen) {
				call what(ifirst+ialen)
				write (ttyout,*) 'ERROR: length too long, reenter'
				go to 1801
			}
			pathcase(itmp) = ' '
			pathcase(itmp) = iopcon(ifirst:ilast)
			lengcdir(itmp) = ilast - ifirst + 1

			write (lunhist,1861) pathcase(itmp)(1:lengcdir(itmp)),
						ihbcksl,itmp
1861			format (a, 10x,a1,'# case',i5,' path name')

		}
	}

#####################################################################
2190	if (cmdverbose(-1) <= 1) write (ttyout,2189)
2189	format (' Enter nogroup0: and which groups should not include',
		'group 0',/,'Example: nogroup0: 3 6 8')
	do itmp = 1, maxgrp {
		incgrp0(itmp) = 1      ##### default: include group 0
	}
	call crtin
	if (cmdverbose(-1) <= 2) write (ttyout,*) iopcon
	i = 1
	call wjfren (i,x,il)
	if (i >= 80 & il == 0) go to 2190    # blank line, read again
	if (il==ihx || il==ihe)  {
		ic=il
		icrst = 1
		call rstart(icrst)
		call what (-1)
		return
	}
	i = i-1
	if (iopcon(i:i+8) != 'nogroup0:') {
		call what(i)
		go to 2190
	}
	i = i + 9
2195	call wjfren (i,x,il)
	if (i > 79) go to 2197  # done scanning line
	if (il != 0) {
		write (ttyout,*) 'ERROR: invalid character'
		call what(i)
		go to 2190
	}
	itmp = x + 0.5
	if (itmp < 1 | itmp > maxgrp) {
		write (ttyout,*) 'ERROR: group out of range: ', itmp, '  maxgrp=', maxgrp, ' maxgrp is a compile time limit'
		call what(i)
		go to 2190
	} else {
		incgrp0(itmp) = 0
	}
	go to 2195

2197	write (lunhist,2193) iopcon(1:lnb(iopcon))
2193	format (a)
	
	imat = 0


##############################################################################

	call getgrpcasenam    # get short names for groups and cases
                              # used in single spectrum output results

##############################################################################
190	if (cmdverbose(-1) <= 1) write (ttyout,191)
191	format (' Enter BEGIN SETUP  to start setup')
	call crtin
	if (cmdverbose(-1) <= 2) write (ttyout,*) iopcon
	i = 1
	call wjfren (i,x,il)
	if (i >= 80 & il == 0) go to 190    # blank line, read again
	if (il==ihx || il==ihe)  {
		ic=il
		icrst = 1
		call rstart(icrst)
		call what (-1)
		return
	}
	if (iopcon(1:11) != 'BEGIN SETUP') {
		call what(-1)
		go to 190
	}
	write (lunhist,192) 'BEGIN SETUP'
192	format (a)
	
	imat = 0


#####################################################################
############# SETUP: group, case, or END SETUP ######################

210	if (cmdverbose(-1) <= 1) write (ttyout,215)
215	format (///,' Enter:',/,
		10x,'group #  (where # is the group number),',/,
		10x,'case #   (where # is the case  number),',/,
		10x,'END SETUP  to end tetracorder setup')
	call crtin
	if (cmdverbose(-1) <= 2) write (ttyout,*) iopcon
	i = 1
	call wjfren (i,x,il)
	if (i >= 80 & il == 0) go to 210    # blank line, read again
	if (il==ihx || il==ihe)  {
		ic=il
		icrst = 1
		call rstart(icrst)
		call what (-1)
		return
	}
	if (il == ihce) {   # possibly END SETUP
		if (iopcon(i:i+7) == 'ND SETUP') {
			nmats = imat
			write (lunhist,216) 'END SETUP'
216			format (a)

			# no do a check on groups and cases and disable if not populated


			return
		}
		call what (i)
		go to 210
	}

#       if not end setup, then it should be a case or group

2161	if (il == ihg) {    # possibly group
		if (iopcon(i:i+3) == 'roup') {
			i = i +4
			call wjfren (i,x,il)
			if (il != 0) {
				call what(i)
				go to 210
			}
			ii = x + 0.5
			if (ii < 0 | ii > ngroups) {
				call what (i)
				write (ttyout,*) 'ERROR: group out of range:', ii, '  ngroups=', ngroups, ' (defined in setup)'
				go to 210
			}
			imat = imat + 1
			if (imat > maxmat) go to 217  # imat too big
			group(imat) = ii
			group0(imat) = 0  # FUTURE: check for -0
			icase(imat)  = 0   # not a case

			write (lunhist,2171) 'group', group(imat)
2171			format (a,i3)

			go to 2191   # do setup for this material
			
		} else {
			call what(i)
			go to 210
		}
	} else if (il == ihc) {    # possibly case
		if (iopcon(i:i+2) == 'ase') {
			i = i +4
			call wjfren (i,x,il)
			if (il != 0) {
				call what(i)
				go to 210
			}
			ii = x + 0.5
			if (ii < 0 | ii > ncases) {
				call what (i)
				write (ttyout,*) 'ERROR: case out of range'
				go to 210
			}
			imat = imat + 1
			if (imat > maxmat) go to 217  # imat too big
			icase(imat) = ii
			group0(imat) = -1  # no group 0 in a case
			group(imat)  = -1  # not a group

			write (lunhist,2171) 'case', icase(imat)

			go to 2191   # do setup for this material
			
		} else {
			call what(i)
			go to 210
		}
	}

217	write (ttyout,218) maxmat
218	format ('ERROR: Number of material must be',
			' > 1 and <= ',i4,/,
                        18x,'(Note: this limit can be inceased by a ',
				'simple recompile',/,
			25x,'of the program.  See your software manager')
	go to 210



###########################################################
2191    if (cmdverbose(-1) <= 1) write (ttyout,2192)
2192	format (///,' Enter: use = yes or no to enable/disable this entry')

	call crtin
	if (cmdverbose(-1) <= 2) write (ttyout,*) iopcon
	i = 1
	call wjfren (i,x,il)
	if (i >= 80 & il == 0) go to 221    # blank line, read again
	if (il==ihx || il==ihe)  {
		ic=il
		icrst = 1
		call rstart(icrst)
		call what (-1)
		return
	}
	#  select whether or not to use this material
	imatenable(imat)=1  # default is enable
	if (iopcon(1:3) == 'use' ) {

		i = 4
		call wjfren (i,x,il)
	
		if ( i > 75 )  {
			write (ttyout,*) 'ERROR: nothing on line after use'
			go to 2191
		}
                if (iopcon(i-1:i-1) == '=' ) { # OK to keep scanning
			call wjfren (i,x,il)

			if ( i > 75 )  {
				write (ttyout,*) 'ERROR: nothing on line after ='
				go to 2191
			}
			if (iopcon(i-1:i+1) == 'yes' ) {
					imatenable(imat)=1           # enable material
					write (lunhist,*) 'use = yes'
			} else if ( iopcon(i-1:i) == 'no' ) {
					imatenable(imat)=0           # disable material
					write (lunhist,*) 'use = no'
			} else {
				call what (i)
				write (ttyout,*) 'ERROR: use must be yes or no'
				go to 2191
			}
		}

	} else {
		write (ttyout,*) 'ERRPR: expecting "use" keyword'
		go to 2191
	}

2194	if ( imatenable(imat)== 0 ) {  # cycle through until we get to an endaction or END SETUP statement

2196		call crtin
		if (cmdverbose(-1) <= 2) write (ttyout,*) iopcon
		#if (cmdverbose(-1) <= 1) write (ttyout,*) 'DEBUG: cycle to next material.  iopcon=', iopcon(1:lnb(iopcon))
		write (lunhist,216) iopcon(1:lnb(iopcon))
		if (iopcon(1:9) == 'endaction') {
			go to 210
		} else if (iopcon(1:9) == 'END SETUP') {
                        nmats = imat
                        return

		} else {
			go to 2196  # read another line
		}
	}
	

	


###########################################################
221	if (cmdverbose(-1) <= 1) write (ttyout,222)
222	format (///,' Enter data converstion: ',/,
		10x,'udata: raw         (no effect yet)',/,
		10x,'udata: radiance    (no effect yet)',/,
		10x,'udata: reflectance (no effect yet)',/,
		10x,'udata: 1-reflectance (no effect yet)',/,
		10x,'udata: emittance   (no effect yet)')
	call crtin
	if (cmdverbose(-1) <= 2) write (ttyout,*) iopcon
	i = 1
	call wjfren (i,x,il)
	if (i >= 80 & il == 0) go to 221    # blank line, read again
	if (il==ihx || il==ihe)  {
		ic=il
		icrst = 1
		call rstart(icrst)
		call what (-1)
		return
	}
	# put udata stuff here in FUTURE
	if (iopcon(1:10) == 'udata: raw') {
		udata(imat) = 0
		write (lunhist,223) 'udata: raw'
223	format (a)

	} else if (iopcon(1:15) == 'udata: radiance') {
		udata(imat) = 1
		write (lunhist,223)  'udata: radiance'

	} else if (iopcon(1:18) == 'udata: reflectance') {
		udata(imat) = 2
		write (lunhist,223)  'udata: reflectance'

	} else if (iopcon(1:16) == 'udata: emittance') {
		udata(imat) = 3
		write (lunhist,223)  'udata: emittance'
	} else {
		call what(i)
		go to 221
	}

###########################################################
231	if (cmdverbose(-1) <= 1) write (ttyout,232)
232	format (///,' Enter convolution options: ',/,
		4x,'convolve: no         (no effect yet)',/,
		4x,'convolve: alt-keyword gaussian wav= FILE-ID REC# res= FILE-ID REC#  (not allowed yet)')
	call crtin
	if (cmdverbose(-1) <= 2) write (ttyout,*) iopcon
	i = 1
	call wjfren (i,x,il)
	if (i >= 80 & il == 0) go to 231    # blank line, read again
	if (il==ihx || il==ihe)  {
		ic=il
		icrst = 1
		call rstart(icrst)
		call what (-1)
		return
	}
	# put convolve stuff here in FUTURE
	if (iopcon(1:12) == 'convolve: no') {
		write (lunhist,233) 'convolve: no'
233	format (a)

	} else {
		call what(i)
		go to 231
	}

###########################################################
241	if (cmdverbose(-1) <= 1) write (ttyout,242)
242	format (///,' Enter preratio options: ',/,
		4x,'preratio: none         (no effect yet)',/,
		4x,'preratio: URATIO FILE-ID REC#',
		4x,'preratio: RRATIO FILE-ID REC#',
		4x,'preratio: URATIO FILE-ID REC# RRATIO FILE-ID REC#  (not allowed yet)')

        # uratio(imaxch,maxmat)  # reflectance of spectrum to ratio 
	#				 # into unknown spectrum
        # rratio(imaxch,maxmat)  # reflectance of spectrum to ratio 
	#				 # into unknown spectrum
        # ndevurat(maxmat)       # specpr device letter ids for uratio
        # nrecurat(maxmat)       # specpr rec nos for uratio
        # ndevrrat(maxmat)       # specpr device letter ids for rratio
        # nrecrrat(maxmat)       # specpr rec nos for rratio
        # flguratio(maxmat)      # flag to indicate do uratio: no =0
        # flgrratio(maxmat)      # flag to indicate do rratio: no =0

	flguratio(imat) = 0
	flgrratio(imat) = 0

	call crtin
	if (cmdverbose(-1) <= 2) write (ttyout,*) iopcon
	i = 1
	call wjfren (i,x,il)
	if (i >= 80 & il == 0) go to 241    # blank line, read again
	if (il==ihx || il==ihe)  {
		ic=il
		icrst = 1
		call rstart(icrst)
		call what (-1)
		return
	}

	if (iopcon(1:9) == 'preratio:') {
		i = 10
		call wjfren (i,x,il)
		i = i - 1
		if (iopcon(i:i+3) == 'none') {
			write (lunhist,243) 'preratio: none'
243			format (a)
		} else if (iopcon(i:i+5) == 'URATIO') {

			i = i + 6
			call wjfren(i,x1,idevb)
			if (i >= 79 && idevb == 0) {
				call what(i)
				go to 241
			}

			call wjfren(i,xfilb,ic2)

			if (ic2 == ihd) {    # want to delete points
						# FUTURE
				chdltflg='d'
				ic2=0
			}

		#       *** check for invalid input ***
			if (x1!=0.0 || ic2!=0 || xfilb<=0.0) {
				call what(i)
				write(ttyout,66)
66				format ('invalid input, reenter')
				go to 241

		#       *** looks ok so get file b ***
			} else {
				ifilb = xfilb
				call devok (4,idevb,ifilb,lun,ier)
				if (ier!=0) {
					go to 241
				}
				itmp = ifilb
				call redfil(itmp,lun,ier)
				if (ier != 0) {
					go to 241
				}
				iform = 1
				if (cmdverbose(-1) <= 2) {
					write (ttyout,244) imat, idevb,
							ifilb, ititl
				}
244				format ('URATIO spectrum for mat:',i4,
							2x,a,i7,3x,a)
				if (cmdverbose(-1) <= 1) write (ttyout,*) ' '
				call namdev (idevb, inamr)

				ndevurat(imat) = idevb
				nrecurat(imat) = ifilb
			}
			do i = 1, nchans {
				uratio(i,imat) = data(i)
			}
			flguratio(imat) = 1
			write (lunhist,245) 'preratio: URATIO', inamr,
							ifilb, ihbcksl,ititl
245			format (a,1x,a,i7,9x,a,'# ',a)
			

		} else if (iopcon(i:i+5) == 'RRATIO') {    # FUTURE

			i = i + 6
			call wjfren(i,x1,idevb)
			if (i >= 79 && idevb == 0) {
				call what(i)
				go to 241
			}

			call wjfren(i,xfilb,ic2)

			if (ic2 == ihd) {    # want to delete points
						# FUTURE
				chdltflg='d'
				ic2=0
			}

		#       *** check for invalid input ***
			if (x1!=0.0 || ic2!=0 || xfilb<=0.0) {
				call what(i)
				write(ttyout,66)
				go to 241

		#       *** looks ok so get file b ***
			} else {
				ifilb = xfilb
				call devok (4,idevb,ifilb,lun,ier)
				if (ier!=0) {
					go to 241
				}
				itmp = ifilb
				call redfil(itmp,lun,ier)
				if (ier != 0) {
					go to 241
				}
				iform = 1
				if (cmdverbose(-1) <= 2) {   
					write (ttyout,1244) imat, idevb,
							ifilb, ititl
				}
1244				format ('RRATIO spectrum for mat:',i4,
							2x,a,i7,3x,a)
				if (cmdverbose(-1) <= 1) write (ttyout,*) ' '
				call namdev (idevb, inamr)

				ndevrrat(imat) = idevb
				nrecrrat(imat) = ifilb
			}
			do i = 1, nchans {
				rratio(i,imat) = data(i)
			}
			flgrratio(imat) = 1
			write (lunhist,245) 'preratio: RRATIO', inamr,
							ifilb, ihbcksl,ititl

		} else {

			write (ttyout,*) 'ERROR: expecting URATIO or RRATIO keyword'
			call what(i)
			go to 241
		}

	} else {
		write (ttyout,*) 'ERROR: expecting preratio: keyword'
		call what(i)
		go to 241
	}

###########################################################
261	if (cmdverbose(-1) <= 1) write (ttyout,262)
262	format (///,' Enter preprocess options: ',/,
		4x,'preprocess: none         (no effect yet)',/,
		4x,'preprocess: algorithm case # (not allowed yet)')
	call crtin
	if (cmdverbose(-1) <= 2) write (ttyout,*) iopcon
	i = 1
	call wjfren (i,x,il)
	if (i >= 80 & il == 0) go to 261    # blank line, read again
	if (il==ihx || il==ihe)  {
		ic=il
		icrst = 1
		call rstart(icrst)
		call what (-1)
		return
	}
	# put preprocess stuff here in FUTURE
	if (iopcon(1:16) == 'preprocess: none') {
		write (lunhist,263) 'preprocess: none'
263		format (a)

	} else {
		call what(i)
		go to 231
	}

###########################################################
271	if (cmdverbose(-1) <= 1) write (ttyout,272)
272	format (///,' Enter algorithm options: ',/,
		4x,'algorithm: tricorder-primary',/,
		4x,'algorithm: nvres             (veg red edge position)',/,
		4x,'algorithm: other algorithm          (not allowed yet)')

	ialgorithm(imat) = 0 # default = tricorder-primary
	call crtin
	if (cmdverbose(-1) <= 2) write (ttyout,*) iopcon
	i = 1
	call wjfren (i,x,il)
	if (i >= 80 & il == 0) go to 271    # blank line, read again
	if (il==ihx || il==ihe)  {
		ic=il
		icrst = 1
		call rstart(icrst)
		call what (-1)
		return
	}
	# put algorithm stuff here in FUTURE
	i = i -1
	if (iopcon(i:i+9) == 'algorithm:') {
		i = i +10
		call wjfren (i,x,il)
		i = i -1
		if (i < 80 - 17) {
			if (iopcon(i:i+16) == 'tricorder-primary') {
				ialgorithm(imat) = 0
				write (lunhist,273) 'tricorder-primary'
273				format ('algorithm: ',a)
			} else if (iopcon(i:i+16) == 'nvres') {
				ialgorithm(imat) = 1
				write (lunhist,273) 'nvres'
			} else {
				call what(i)
				go to 231
			}
		} else {

			call what(i)
			go to 231
		}

	} else {
		call what(i)
		go to 231
	}


###########################################################
#######   Matterial ID
###########################################################
2711	if (cmdverbose(-1) <= 1) write (ttyout,2721)
2721	format (///,' ID= and up to 20 characters for the material ID',/)
	matid(imat) = '                    '  # default = tricorder-primary
        #                       11111111112
        #              12345678901234567890
	call crtin
	if (cmdverbose(-1) <= 2) write (ttyout,*) iopcon
	i = 1
	call wjfren (i,x,il)
	if (i >= 80 & il == 0) go to 2711    # blank line, read again
	if (il==ihx || il==ihe)  {
		ic=il
		icrst = 1
		call rstart(icrst)
		call what (-1)
		return
	}
	# 
	i = i -1
	if (iopcon(i:i+2) == 'ID=') {
		i = i +3
		call wjfren (i,x,il)
		i = i -1
		if (i < 80 - 39) {
			matid(imat)=iopcon(i:i+39)
		} else {

			call what(i)
			go to 2711
		}

	} else {
		call what(i)
		go to 2711
	}

###########################################################
#######   define library records
###########################################################

2811	call getlibrecs (imat, irtn)
	if (irtn != 0) {
                write (ttyout,*) 'ERROR: NO Alternate Libraries, TRY AGAIN'
                call what(i)
                go to 2811
	}



###########################################################
###########################################################
# delete points
###########################################################

3051	if (chdltflg == 'd') {
		if (cmdverbose(-1) <= 1) {
			write (ttyout,*) 'Enter channels to delete, then a c to continue'
		}
		call crtin
		if (cmdverbose(-1) <= 2) write (ttyout,*) iopcon
		i=1
		call dltpts(i,jdlt,idlt,nchans,ic2)
		if (ic2==ihx)  {
			icrst = 1
			call rstart(icrst)
			call what (-1)
			return
		}
		if (ic2!=ihe & jdlt > 1) {
			mxstrdel=240   # 3 lines of 80 characters
			call delhist(jdlt,idlt,mhist,mxstrdel)

			write (lunhist,3055) mhist(1:lnb(mhist(1:80)))
3055			format (a)
			if (mhist(81:81) != ' ') {
				write (lunhist,3055) mhist(81:80+lnb(mhist(81:160)))
			}
			if (mhist(161:161) != ' ') {
				write (lunhist,3055) mhist(161:160+lnb(mhist(161:240)))
			}
			do jj = 1, jdlt {               # delete the channels
				data(idlt(jj))=-1.23e34
			}
		} else {
			write (lunhist,3055) 'c'
		}

	} else {
		if (cmdverbose(-1) <= 1) write (ttyout,*) 'DEBUG: NOTE: no deleted point flag'
	}

###########################################################
# Title for material "imat"

161	if (cmdverbose(-1) <= 1) write (ttyout,162)
162	format (' Type in an output title',/,
		'-------------------------------|')
	call crtin
	if (cmdverbose(-1) <= 2) write (ttyout,*) iopcon
	i = 1
	call wjfren (i,x,il)
	if (i >= 80 & il == 0) go to 161    # blank line, read again
	if (i > 1) i = i-1
	jend=lnb(iopcon)
	length = jend - i + 1
	if (length > 32) length = 32
	jend = i + length - 1
	otitle(imat)(1:length) = iopcon(i:jend)
	do ij = 1, 40 {
		if(otitle(imat)(ij:ij) == char(0)) {
			otitle(imat)(ij:ij) = ' '
		}
	}

# write history
	write (lunhist, 3060) otitle(imat)(1:32),ihbcksl
3060	format (a, 2x,a,'# output title')


###########################################################
#######   define features
###########################################################
3811	if (cmdverbose(-1) <= 1) write (ttyout,3821)
3821	format (///,' type: define features',/)
              #                      1111111111222
              #             1234567890123456789012
	call crtin
	if (cmdverbose(-1) <= 2) write (ttyout,*) iopcon
	i = 1
	call wjfren (i,x,il)
	if (i >= 80 & il == 0) go to 3811   # blank line, read again
	if (il==ihx || il==ihe)  {
		ic=il
		icrst = 1
		call rstart(icrst)
		call what (-1)
		return
	}
	# 
	i = i -1
	if (iopcon(i:i+14) != 'define features') {

		call what(i)
		go to 3811
	}


###########################################################
####   define features   up to   endfeatures 
###########################################################
220	nfeat(imat) = 0    # number features to analyze in each material, start at 0

	numnotfeat(imat) = 0  # initially set to 0

	do itmpfeat = 1,  maxfeat {

		ifeatenable(itmpfeat,imat) = 0  # disable all features
	}	

	do itmpfeat = 1, maxnotfeat {

		notfeatenable(itmpfeat,imat) = 0  # disable all not features
	}

#	continuum points

297	ndl = 0
	imod12=0  # no features are disabled yet

	dlbar(imat) = 0.0

###########################################################

298	if (cmdverbose(-1) <= 1) write (ttyout,300) 
300	format (1x, 'Enter feature type  number  LEFT and RIGHT CONTINUUM INTERVALS ',
			'and constraints:',//,
		'Enter  f  for feature, n for not feature, followed by feature number:',/,
		'          followed by spectrum library letter (a, b, c, ...)',/,
		'          NOTE: at presetn, only library letter  a  is valid ',/,
		10x,'D  =  Diagnostic feature (must be present)',/,
		10x,'M  =  Must have diagnostic unconditinionally',/,
		10x,'O  =  Optionally present feature',/,
		10x,'W  =  Weak feature',/,
		10x,'      (There is a speed advantage to listing',/,
		10x,'       diagnostic features first.)',/,
		'NEXT Enter two channel numbers to describe the continuum',
			' interval on the ',/,5x,
			'LEFT side of the absorption, and',//,
		'NEXT Enter two channel numbers to describe ',
			'the continuum interval on the ',/,5x,
			'RIGHT side of the absorption ',//,
		' (enter imortance and 4 channel numbers total),',
					 ' then OPTIONS:',/)

	if (cmdverbose(-1) <= 1) write (ttyout,301)
301	format (' Options:',/,
	'         ct n m  where  ct means continuum threshold',/,
	'        lct n m  where lct means left  continuum threshold',/,
	'        rct n m  where rct means right continuum threshold',/,
        '        lcbbrc = (lc-bb)/(rc-bb), bb= band bottom',/,
        '        rcbblc = (rc-bb)/(lc-bb), bb= band bottom',/,
        '        rcbblc, lcbbrc = left contin, band bottom, ',
				'right continuum shape',/,
	17x, 'n is lower, and (optional) m is upper.',/,
	17x, 'Values exceeding this range will reject that material',/,
	17x, '       default = 0.1e-6 to 0.2e+20',/,
	17x, ' mode = a_mode_string',/,
	17x, ' moce_left  direction amount (direction = left or right, 2c = 2 channels',/,
	17x, ' moce_right direction amount (direction = left or right, 2c = 2 channels',/,
	17x, 'ignore w  leftlimit  rightlimit ',/)

	if (cmdverbose(-1) <= 1) write (ttyout,304)
304	format (/,
	'        lct/rct> n1 n2 where n1 n2 are positive real numbers',/,
	'        rct/lct> n1 n2 where n1 n2 are positive real numbers',/,
	'        rcbblc> n1 n2  where n1 n2 are positive real numbers',/,
	'        rcbblc< n1 n2  where n1 n2 are positive real numbers',/,
	'        lcbbrc> n1 n2  where n1 n2 are positive real numbers',/,
	'        lcbbrc< n1 n2  where n1 n2 are positive real numbers',/,
	'        r*bd>   n1 n2  where n1 n2 are positive real numbers',/,
	'                       note: bd is abs(bd) for positive features',/,
	17x, 'Values < (or >) these ranges will reject that material',//,
	17x, '       default = 0 (no lct/rct of rct/lct checking',/)

305     if (cmdverbose(-1) <= 1) write (ttyout,303)
303     format (//,'NOT Features:',//,
                '  Enter n feature# a|r NOT followed by the File ID, Rec. No., ',
                                'feature #, threshold',
                                        ' depth[a,r#] and threshold fit',/,
                '         where a  = absolute depth',/,
                '               r# = relative depth, so that',/,
                '                    NOT feat depth / r# depth is tested',/,
                '                    where # = a feature from the current',/,
                '                    material',/,
                '  Examples:',/,
                '   n1a  NOT [sprln01] 12 2 0.05a  0.3 \# absolute depth .05, fit .3',/,
                '   n1r  NOT [sprln01] 12 2 0.05r2 0.3 \# relative depth .05, fit .3',//,
                '  File ID and Rec No. must be an previosly entered',
                        ' spectrum')

	call crtin  # read new line, expect blank, f, n, or endfeatures
	if (cmdverbose(-1) <= 2) write (ttyout,*) iopcon
26	i=1

	call wjfren (i,x,il)        # look for feature importance
	if (i >= 80 & il == 0) go to 298    # blank line, read again
	if (il==ihe && iopcon(i-1:i+9) == 'endfeatures') { # endfeatures

		if (nfeat(imat) == 0 &  numnotfeat(imat) == 0) {
			call what(i)
			write (ttyout,*) 'ERROR: endfeatures found, but no features entered'
			go to 298
		}
		go to 3090  # features complete
	}
	if (il==ihx || il==ihe) {
		ic=il
		icrst = 1
		call rstart(icrst)
		call what (-1)
		return
	}
	if (il!=ihf && il!=ihn) {  # should be f for feature or n for notfeature

		#write (ttyout,*) 'DEBUG test 1, we are here'
		write (ttyout,3041) iopcon
		call what (i)
		icrst = 1
		return
3041		format (' ERROR: expecting f for feature or n for notfeature',/,
			' input line=',/, a)
	}

	if (il==ihf) {

		call getifeat (i, ifeat, imat, ndl, irtn)   # get feature and feature constraints
		if (irtn != 0 & irtn != 2 & irtn != 12) {

			write (ttyout,*) 'ERROR ',irtn,' on return from getifeat, i=', i
			write (ttyout,*) 'input line:'
			write (ttyout,*) iopcon
			write (ttyout,*) '         11111111112222222222333333333344444444445555555555666666666677777777778'
			write (ttyout,*) '12345678901234567890123456789012345678901234567890123456789012345678901234567890'
			go to 298    # error, read next line
		}
		if (irtn == 0) {
			#write (ttyout,*) 'DEBUG in reflsetup: getifeat irtn == 0 thus found endfeatures'
			go to 3090
		}
		if (irtn ==12) {  # feature was out of range, disabled, read next line
			imod12=12
			go to 298 # read next line
		}
		if (il==ihe & iopcon(i-1:i+9) == 'endfeatures') { # endfeatures
			#write (ttyout,*) 'DEBUG in reflsetup: found endfeatures'
			go to 3090 
		}
		if (irtn != 2) {
			go to 26   # look for more input
		}   # else we drop down and do not featue, because if irtn=2, il=ihn

		if (irtn == 2) {  # found a not feature, so drop down to the next if and call getnotfeat
			il=ihn
		}
	}
	if (il==ihn) {                      # go to not feature definition

		call getnotfeat (i, ifeat, imat, irtn)      # get not feature and feature constraints
		if (irtn != 0) go to 298    # error
		if (irtn == 0) {
			#write (ttyout,*) 'DEBUG in reflsetup: getnotfeat irtn == 0 thus found endfeatures'
			go to 3090
		}
		if (il==ihe && iopcon(i-1:i+9) == 'endfeatures') { # endfeatures
			go to 3090 
		}
		go to 26   # look for more input
	}


3090     continue   # jump to here from endfeatures statement

  ndl = 0 # recompute ndl, the number of non-weak features
  nfen = 0 # number of features enabled of any kind
  dlbar(imat) = 0.0

  #####   added 10/2019
  do ifeat = 1, nfeat(imat) {
	if ( ifeatenable(ifeat,imat) > 0) {

		nfen = nfen +1   # we have a feature enabled
	}
  }
  if ( nfen == 0 ) {         # no features, so disable material
	imatenable(imat) = 0
	write (ttyout, 3045) imat, otitle(imat)
	write (lunhist, 3045) imat, otitle(imat)
  }
  #####

  do ifeat = 1, nfeat(imat) {
	
	if (dl(ifeat,imat) != dl(ifeat,imat) ) {        # NaN check
			dl(ifeat,imat) = 0.0            # no weight
			ifeatenable(ifeat,imat) = 0     # disable feature
			write (ttyout, *)  'DEBUG: reflsetup NaN detected for dl(',ifeat,',',imat,')'
			write (lunhist, *) 'DEBUG: reflsetup NaN detected for dl(',ifeat,',',imat,')'
	}

	if (featimprt(ifeat,imat) != 1 & ifeatenable(ifeat,imat) > 0 ) {  # do not count weak features
                dlbar(imat) = dlbar(imat) + dl(ifeat,imat)
                ndl = ndl + 1
	}
  }

  if (ndl < 1) {
	if (imod12 == 12) {   # no features but some were disabled, so disable material

		imatenable(imat) = 0  # disable feature	
		write (ttyout, 3045) imat, otitle(imat)
3045		format (9x,'WARNING: material', i6,' is DISABLED due to no features, title=',a,//)

		goto 2194 # cycle through until we get to an endaction or END SETUP statement

	} else {   # no features found and none were disabled,so this is an error
		write (ttyout,*) 'ERROR: no diagnostic or optional features ',
			'declared, you must have at least one'
		i =1
		call what(i)
		go to 297
	}
  }
  dlsum(imat) = dlbar(imat)             # this is the summed ref lib band depth areas
  dlbar(imat) = dlbar(imat)/ float(ndl) # this is the normalized summed areas

###########################################################
  xsum=0.0 # test summation of normalozed  weights
  do ifeat = 1, nfeat(imat) {   # compute normalized lib band depths

	dln(ifeat,imat) = dl(ifeat,imat)/dlsum(imat)
	xsum=xsum + dln(ifeat,imat)    # sum of normalized weights

#       history notes:
	if (ifeat == 1) write (lunhist,3101) ihbcksl
	write (lunhist, 3100) ihbcksl, ifeat, dln(ifeat,imat)
3100	format (a,'#',9x,'Feat:',i3,
		'  has a weight of',f6.3)
3101	format (a,'# Notes:')
  }
        write (lunhist, 3103) ihbcksl, xsum
3103    format (a,'#',9x,'Check: normalized sum of weights=:',f9.5,' (should be 1.0)')
        write (lunhist, 3103) 'DEBUG: ', xsum

 

###########################################################
#       now check for fit, dpeth, fd thresholds

7	call getconstraints (imat, irtn)
	if (irtn != 474) {
		write (ttyout,*) 'ERROR: getconstraints irtn = ', irtn
		call what(i)
		go to 7
	}


###########################################################
## files to output if imaging

474	if (cmdverbose(-1) <= 1) write (ttyout,475)
475	format(///, 'Enter: define output to begin the output block',/,
		'   then', /,
		'Enter: output= and the images to output ',
					'(when in image mode',/,
		10x,'output  options: fit depth fd none',/,
		10x,'example: output= fit depth fd')
	call crtin
	if (cmdverbose(-1) <= 2) write (ttyout,*) iopcon
	i = 1
	call wjfren (i,x,il)
	if (i >= 80 & il == 0) go to 474    # blank line, read again
	if (il==ihx || il==ihe)  {
		ic=il
		icrst = 1
		call rstart(icrst)
		call what (-1)
		return
	}
	
	if (iopcon(i-1:i+11) != 'define output') {   # define output block
		call what(i)
		write (ttyout,*) 'ERROR: define output expected, try again'
		if (cmdverbose(-1) <= 1) write (ttyout,*) 'DEBUG: iopcon(i-1:i+11)=',iopcon(i-1:+11),' i=',i
		go to 474
	}
	if (cmdverbose(-1) <= 1) write (ttyout,*) 'DEBUG: define output statement found.  starting output definitions block'

477	call crtin
	if (cmdverbose(-1) <= 2) write (ttyout,*) iopcon
	i=1
	call wjfren (i,x,il)
	if (i >= 80 & il == 0) go to 474    # blank line, read again
	if (il==ihx || il==ihe)  {
		ic=il
		icrst = 1
		call rstart(icrst)
		call what (-1)
		return
	}
	if (index(iopcon,'output=') == 0) {
		call what (i)
		write (ttyout,*) 'ERROR: output= keyword not found'
		go to 474
	}
	oenblfit(imat) = 0
	oenbldepth(imat) = 0
	oenblfd(imat) = 0
	cht1 = '   '
	cht2 = '     '
	cht3 = '  '
	if (index(iopcon,'fit') > 0) {
		oenblfit(imat) = 1
		cht1 = 'fit'
	}
	if (index(iopcon,'depth') > 0) {
		oenbldepth(imat) = 1
		cht2='depth'
	}
	if (index(iopcon,'fd') > 0) {
		oenblfd(imat) = 1
		cht3='fd'
	}
	if (index(iopcon,'none') > 0) {
		if (cmdverbose(-1) <= 1) write (ttyout,*) 'WARNING: no image output'
	}

	write (lunhist, 476) cht1, cht2, cht3
476	format ('output= ',a,' ',a,' ',a)


###########################################################
# Base file name for material "imat"

180	if (cmdverbose(-1) <= 1) write (ttyout,182)
182	format (///,' Output files for cube analysis: ',/,
		' Type in the output BASE file name',/,
		'      The output files will be called:',/,
		'               filename.depth',/,
		'               filename.fit',/,
		'               filename.fd',/,
		'-------------------------------------------|')
	call crtin
	if (cmdverbose(-1) <= 2) write (ttyout,*) iopcon
	i = 1
	call wjfren (i,x,il)
	if (i >= 80 & il == 0) go to 180    # blank line, read again
	if (i >= 79) {
		call what(i)
		go to 180
	}
	if (i > 1) i = i-1
	jfirst=fnb(iopcon)
	if (jfirst < 1) jfirst = 1
	jend  =lnb(iopcon)
	if (jend > 80) jend = 80
	if (jend < 1) jend = 1
	length = jend - jfirst + 1
	###write (6,*) 'DEBUG: jfirst jend, length',jfirst, jend, length
	if (length > mfilelen) {
		write (ttyout,*) 'WARNING: truncating file name'
		length = mfilelen
	}
	jend = jfirst + length - 1                 #truncate
	###write (6,*) 'DEBUG: jfirst jend, length',jfirst, jend, length
	mfile(imat)(1:length) = iopcon(jfirst:jend)
	itmp = index(mfile(imat)(1:length),' ')    # check for blanks
	if (itmp != 0) {
		call what(jfirst+itmp)
		write (ttyout,184)
184		format (' ERROR: blanks are not allowed in file names',/)
		go to 180
	}
	itmp = index(mfile(imat)(1:length),char(9))  # check for tabs
	if (itmp != 0) {
		call what(i+itmp)
		write (ttyout,185)
185		format (' ERROR: tabs are not allowed in file names',/)
		go to 180
	}
	do ij = 1, mfilelen {
		if (mfile(imat)(ij:ij) == char(0)) mfile(imat)(ij:ij)=' '
	}

	lenfile(imat)=length

	dfile = mfile(imat)(1:length) // '.depth'
	if (oenblfit(imat) == 1) {
		inquire (file=dfile(1:length+6),exist=fexist, iostat=ier)
		
		#write (ttyout,*) 'DEBUG: dfile ier=',ier, '  fexist=',fexist
		if (fexist | ier != 0) {
			write (ttyout,187) ier, 'depth', dfile(1:length+6)
187			format (' ERROR',i5,' on ',a,' file:',/,a,/,
				' FILE EXISTS, but it should not')
			call what (-1)
			go to 180
		}
	}

	ffile = mfile(imat)(1:length) // '.fit'
	if (oenblfit(imat) == 1) {
		inquire (file=ffile(1:length+4), exist=fexist, iostat=ier)

		#write (ttyout,*) 'DEBUG: ffile ier=',ier, '  fexist=',fexist
		if (fexist | ier != 0) {
			write (ttyout,187) ier, 'fit', ffile(1:length+4)
			call what (-1)
			go to 180
		}
	}

	fdfile = mfile(imat)(1:length) // '.fd'
	if (oenblfd(imat) == 1) {
		inquire (file=fdfile(1:length+3), exist=fexist, iostat=ier)

		#write (ttyout,*) 'DEBUG: fdfile ier=',ier, '  fexist=',fexist
		if (fexist | ier != 0) {
			write (ttyout,187) ier, 'fit*depth', fdfile(1:length+3)
			call what (-1)
			go to 180
		}
	}

	if (cmdverbose(-1) <= 1) {
		write (ttyout,*) ' '
		write (ttyout,*) ' '
	}

# write history
	write (lunhist,3070) mfile(imat)(1:mfilelen), ihbcksl
3070	format (a,3x,a,'# Output base file name')

###########################################################
     # Prompt the user for scaling factors for output
	bdscal(imat) = 1.0
	qfscal(imat) = 1.0  # note this is hard set to 1.0

464	if (cmdverbose(-1) <= 1) write (ttyout,465) imat, otitle(imat)
465	format(///,' Enter the output and scale for the band depth for',//,
		' Material',i3,': ',a,/,
		' Enter output bits/pixel DN dn# = real# option',//,
		5x,'bits/pixel = 8 or 16',/,
		5x,'dn# is an image data number to scale real# to',/,
		5x,'Example:  8     DN 255   = 0.5',/,
		5x,'means 8 bits/pixel and scale 0.5 to a DN of 255')

	call crtin
	if (cmdverbose(-1) <= 2) write (ttyout,*) iopcon
	i=1
	call wjfren (i,x,il)
	if (i >= 80 & il == 0) go to 464    # blank line, read again

	if (il == ihe | il == ihx) {
		icrst = 1
		call rstart (icrst)
		write (ttyout,*)'Exiting.'
		call what (-1)
		return
	} else if (il != 0) {
		call what(i)
		go to 464
	} else {
		itmp = x + 0.5
		if (itmp != 8 & itmp != 16) {
			call what(i)
			write (ttyout,*)'ERROR: bits must be 8 or 16'
			go to 464
		}
		obits(imat) = itmp
	}

	call wjfren (i,x,il)
	if (il == ihcd & iopcon(i:i) == 'N') {    # DN found

		i = i+1
	} else {
		call what(i)
		write (ttyout,*) 'ERROR: DN not found'
		go to 464
	}
	noeql = 0
	call wjfren (i,x,il)
	if (il == 0 | il == ihchar('=')) {

		idn = x + 0.5
		if (obits(imat) == 8 & (idn < 1 | idn > 255)) {
			call what(i-1)
			write (ttyout,*) 'ERROR: dn value out of ',
							'8-bit range'
			go to 464
		} else if (obits(imat) == 16 & 
				(idn < -32767 | idn > 32767 |
							idn == 0)) {
			call what(i-1)
			write (ttyout,*) 'ERROR: dn value out of ',
							'16-bit range'
			go to 464
		}
		if (il == ihchar('=')) noeql = 1
	}
	if (noeql == 0) {           # looking for =
		call wjfren (i,x,il)
		if (il != ihchar('=')) {
			call what(i)
			write (ttyout,*) 'ERROR: = expected'
			go to 464
		}
	}
	call wjfren (i,x,il)
	if (il != 0) {
		call what(i)
		go to 464
	}
	if (abs(x) < 0.001 | abs(x) > 1000.0) {
		
		call what(i-1)
		write (ttyout,*) 'ERROR: value out of range'
		write (ttyout,*) 'range = 0.01 to 1000.'
		go to 464
	}
	
	if (obits(imat) == 8) {
		bdscal(imat) = float(idn) / x
		qfscal(imat) = 255.0
	} else {

		bdscal(imat) = float(idn) / x
		qfscal(imat) = 32767
	}
	
			
	if (cmdverbose(-1) <= 1) {
		write (6,*)'band depth values will be multiplied by ',
				bdscal(imat)
		write (6,*)'quality of fit values will be ',
				'multiplied by ',qfscal(imat)
	}

	write (lunhist,463) obits(imat), idn, x, ihbcksl
463	format (i3, 3x, 'DN ',i6,' = ',f10.4, 9x, a,'# output bits, scale  ')

###########################################################
## compression of output if imaging

484	if (cmdverbose(-1) <= 1) write (ttyout,485)
485	format(///,'Enter: compress= none   or:',/,
		   '       compress= zip')
	call crtin
	if (cmdverbose(-1) <= 2) write (ttyout,*) iopcon
	i = 1
	call wjfren (i,x,il)
	if (i >= 80 & il == 0) go to 484    # blank line, read again
	if (il==ihx || il==ihe)  {
		ic=il
		icrst = 1
		call rstart(icrst)
		call what (-1)
		return
	}
	if (index(iopcon,'compress=') == 0) {
		call what (-1)
		write (ttyout,*) 'ERROR: compress= keyword not found'
		go to 484
	}
	i = index(iopcon,'compress=')+9
	call wjfren (i,x,il)
	if (iopcon(i-1:i+1) == 'zip' ) {
		ocompress(imat) = 1
		write (lunhist,486) 'compress= zip'
	} else if (iopcon(i-1:i+2) == 'none' ) {
		ocompress(imat) = 0
		write (lunhist,486) 'compress= none'
	} else {
		write (ttyout,*) 'ERROR: wrong compression keyword'
		call what(i)
		go to 484
	}

486	format (a)

################# now expect endoutput

492	if (cmdverbose(-1) <= 1) write (ttyout,*) 'Enter endoutput to finish the define ouput section'
	call crtin
	if (cmdverbose(-1) <= 2) write (ttyout,*) iopcon
	i = 1
	call wjfren (i,x,il)
	if (i >= 80 & il == 0) go to 492    # blank line, read again
	if (il==ihx )  {
		ic=il
		icrst = 1
		call rstart(icrst)
		call what (-1)
		return
	}
	
	if (iopcon(i-1:i+7) != 'endoutput') {   # end the output block
		write (ttyout,*) 'ERROR: endoutput expected, found: ', iopcon
		write (ttyout,*) ' try again'
		go to 492
	} else {
		write (lunhist, *) 'endoutput'
	}

###########################################################
## action

504	call getaction (imat, irtn)

	if (irtn != 0) return   # e or x exit setup


########} # end of do imat loop

go to 210

#DEBUG:
#	if (cmdverbose(-1) <= 1) {
#		write (ttyout,*) 'nmats=',nmats
#		write (ttyout,*) 'nfeats:',(nfeat(i), i= 1, nmats)
#	}

	end

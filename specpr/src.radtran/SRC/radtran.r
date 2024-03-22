#HPUX program radtran (ach1, ach2, ach3)
#
#     RATFOR
#
#     this program is an interface to radiative transfer models:
#          mreflectv3: do reflectance computations
#          optconst: compute optical constants from reflectance spectra
#
#**************************************************************************
# This software was written by Roger N. Clark and colleagues and falls under the following license:
# 
# Copyright (c) 1975 - 2020, Roger N. Clark,
#                            Planetary Science Institute, PSI
#                            rclark@psi.edu, and colleagues named in the code.
# 
# All rights reserved.
# 
# GNU General Public License https://www.gnu.org/licenses/gpl.html
# 
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
#   Redistributions of the program must retain the above copyright notice,
#   this list of conditions and the following disclaimer.
#
#   Neither Roger N. Clark, Planetary Science Institute, nor the names of
#   contributors to this software may be used to endorse or promote products
#   derived from this software without specific prior written permission.
#
#   Translation/recoding into other languages: the translation and source code
#   must be made available for free.
#
#   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS "AS IS"
#   AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
#   IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
#   ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS
#   BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY,
#   OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
#   SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
#   INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
#   CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
#   ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF
#   THE POSSIBILITY OF SUCH DAMAGE.


	implicit integer*4 (i-n)

	include "../../src.specpr/common/spmaxes"

	include "defs.h"
	include "lmrefl.h"
	include "convh.h"

        include "../../src.specpr/common/blank"
        include "../../src.specpr/common/lbl7"
        include "../../src.specpr/common/lbl8"
        include "../../src.specpr/common/lbl6"
        include "../../src.specpr/common/lbl4"
        include "../../src.specpr/common/label1"
        include "../../src.specpr/common/labl2"
        include "../../src.specpr/common/lbl3"
        include "../../src.specpr/common/label3"
        include "../../src.specpr/common/labelf"
        include "../../src.specpr/common/lblg"
        include "../../src.specpr/common/info"
        include "../../src.specpr/common/lblvol"
        include "../../src.specpr/common/lblprt"
        include "../../src.specpr/common/cmd"
        include "../../src.specpr/common/cmdarg"
        include "../../src.specpr/common/lundefs"
        include "../../src.specpr/common/alphabet"
        include "../../src.specpr/common/filenames"
        include "../../src.specpr/common/lblwav"
        include "../../src.specpr/common/sitelogo"
        include "../../src.specpr/common/hptrm"
        include "../../src.specpr/common/pipes"
        include "../../src.specpr/common/iocontrol"
        include "../../src.specpr/common/overlys"
        include "../../src.specpr/common/sp3pfeat"
        include "../../src.specpr/common/tetfeat"
        include "../../src.specpr/common/wavemarks"
        include "../../src.specpr/common/deletep"
        include "../../src.specpr/common/inputhistory"

#
	character*1536 dummy
 	equivalence (dummy,ititl)

#HPUX	character*80 ach1, ach2, ach3

	integer*4 fsize
	integer*4 recnum
	integer*4 idummy, filsiz, icrst
	integer*2 chkbit,ibit,bitnum
	real*4 xxn(NMINL),xxk(NMINL)   # temporary holding arrays for
				#   passing info to mrefl sub.

	real*4 param(18),pturb,fitcri,stndev,min,num
	character*8 file1,namwav
	character*1 iform
	integer*4 iargc, nn, ilen, idlt(SPMAXCHAN)

        character*7     ovcolor
	integer*4 iov  # overlay working variable
	integer*4 i2   # temp holder of line scane index

	data ihzx,ihxz/2hzx,2hxz/

#
#
#HPUX	charg1 = ach1
#HPUX	charg2 = ach2
#HPUX	charg3 = ach3

#
#	ttyout = screen output
#	ttyin = keyboard input

#
#       set  common variables in spblockdata  KEL  06/09
#
        call spblockdata

# OLD:
#	maxrec = 999999
#	maxchn = MAXCHNS
#	maxtxt = maxtext   # was 19860

	maxrec = SPMAXREC
	MAXCHNS = SPMAXCHAN
	maxchn = MAXCHNS
	maxtext = SPMAXTEXT
	maxtxt = maxtext   # was 19860

        delpt    = -1.23e34  # deleted point value
        delptlow = -1.231e34
        delptup  = -1.229e34

	iline=0
	helppg =   'specprhelp                              '

	ihistch=0

####### Choose one for default, comment out others 
#                     (this is the default, can be changed by user at run time)
 	iflgse=0      # Se calculation default: from Hapke 1981 graph
#       iflgse=1981   # Se calculation default: from Hapke 1981 graph
#	iflgse=59     # Se calculation Use Hapke 2012 book  page 59 equations
#	iflgse=60     # Se calculation Use Hapke 2012 book  page 60 equations

	call getcmdargs

#############################################################
#       set command counter to beginning of file            #
#############################################################
	icoman = 1
	redire = .false.
	cndx = 0
	icopy = 0

	wmflgmenu = 0
        ovflgmenu = 0

        for (i=1; i<=6; i=i+1) {

                ovrflg(i) = -1   # overlays not defined
                ovrflgb(i) = -1   # overlays not defined
                ovrchn(i) = 0    # number of channels in overlay
        }

# initialize band depth data

        bdflgmenu = 0  # turn of menu info in plot
        do i = 1, imax3pt {

                sleftwave(1,i)  = 0.0
                sleftwave(2,i)  = 0.0
                sctrwave(1,i)   = 0.0
                sctrwave(2,i)   = 0.0
                srightwave(1,i) = 0.0
                srightwave(2,i) = 0.0

                sleftchan(1,i)  = 0
                sleftchan(2,i)  = 0
                sctrchan(1,i)   = 0
                sctrchan(2,i)   = 0
                srightchan(1,i) = 0
                srightchan(2,i) = 0

                sbdepth(i) = 0.0

                sfeatname(i) = '            '
                sfmode(i)    = 0
                sonoff(i)    = 0
                spcbox(i)    = 0

        }

# initialize tetracorder feature data

        tetflgmenu = 0   # turn of menu info in plot
        do i = 1, imaxtet {

                tleftwave(1,i)  = 0.0
                tleftwave(2,i)  = 0.0
                tctrwave(1,i)   = 0.0
                tctrwave(2,i)   = 0.0
                trightwave(1,i) = 0.0
                trightwave(2,i) = 0.0

                tleftchan(1,i)  = 0
                tleftchan(2,i)  = 0
                tctrchan(1,i)   = 0
                tctrchan(2,i)   = 0
                trightchan(1,i) = 0
                trightchan(2,i) = 0

                tbdepth(i) = 0.0

                tfeatname(i) = '            '
                tfmode(i)    = 0
                tetonoff(i)  = 0

                tetfna(i) = '    '
                tetfnc(i) = '    '

        }

	tlyr(1) = 1.0e25 # initialiize, effectively infinite thick layer


##########################################################
#       call initialization routine                      #
##########################################################
        if ((ncmdarg >= 1) & (charg1(1:2) == '-g')) {
                write (ttyout,101)
101             format ('You must specify a restart file before a ',
                       'graphics mode',/)
                stop
        }
	icrst=2
	call rstart(icrst)
#############################################################
#       set command counter to beginning of file            #
#############################################################
	icoman = 1
	redire = .true.
	cndx = 0
	icopy = 0

# initialize imask array

	do i=1, MAXCHNS {
		imask(i) =0
	}

# more specpr initialization

	call taprw
	call prochk

# check for graphics settings

if ((ncmdarg >= 2) & (charg2(1:2) == '-g')) {
	igrmod = 0

	write (ttyout,*)"DEBUG: ", charg2(3:8)
	if (charg2(3:5) == 'xhp') {
		igrmod = 50
		call initt(igrmod)

						# added graphics window size 8/15/2011 - RNC
	} else if (charg2(3:8) == 'xterm3') {   # triple size xterm graphics window
		igrmod = 53
		write (ttyout,*)"DEBUG: igrmod=", igrmod
		call initt(igrmod)

	} else if (charg2(3:8) == 'xterm2') {   # double size xterm graphics window
		igrmod = 52
		write (ttyout,*)"DEBUG: igrmod=", igrmod
		call initt(igrmod)

	} else if (charg2(3:8) == 'xterm1') {   # standard size xterm graphics window
		igrmod = 51
		call initt(igrmod)

	} else if (charg2(3:7) == 'xterm' ) {
		igrmod = 51
		call initt(igrmod)

	} else if (charg2(3:5) == 'xdt') {
# dt for dtterm vs hpterm - result in no io via eralph to clear memory
		igrmod = 60
		call initt(igrmod)

	} else if (charg2(3:3) >= '0' & charg2(3:3) <= '9') {
		iopcon = charg2
		i = 3
		call wjfren(i,x,il)
		if (il != 0) {
			write (ttyout,*) iopcon
			call what(i)
			write (ttyout,*) 'Graphics mode', x, ' unknown'
			write (ttyout,*) 'Graphics mode ignored'
		} else {
			igrmod = x
			call initt(igrmod)
		}
	}
}

399	call eralph
	call whedr

6     write (ttyout,5)
5     format (/, 20x, 'PROGRAM RADTRAN:', /,
       ' This routine interfaces to radiative transfer routines',/)

 	write (ttyout,7)
7	format (/,' Enter  r  to compute reflectance ',
			'spectra of intimate mixtures',
                /,' Enter  R  to compute reflectance spectra of particulate',/,
                  '           intimate + molecular + areal mixtures,',/,
		  '           particles in air or vacuum',
                /,' Enter  L2 to compute reflectance spectra of particulate',/,
                  '           intimate + molecular + areal mixtures,',/,
                  '           in 2 layers, ',/,
		  '           particles in air or vacuum',
                /,' Enter  M  to compute reflectance spectra of ',/,
		  '           particulates in another medium with',/,
                  '           embedded intimate + molecular + areal mixtures',
		/,'           including rocks, oil emulsions',
                /,' Enter  a  to derive absorption ',
				'coefficients from reflectance spectra',
                /,' Enter  u  to unmix (derive absorption ',
				'coefficients for an unknown in a ',/,
		  '           ',
			'reflectance spectrum containing ',
				'one or more knowns)', /,/,
		' Enter  EX  to exit',/)

	nlayers = 0   # no layered media by default

	write (ttyout,8)
8	format ('Follow your selection with which model Se and Si to use:',/,
		'      = 0      Use the original Hapke formulation, ',
					'derived from 1981 graph (radtran pre 2014)',/,
                '      = 1981   Use the original Hapke formulation, ',
					'derived from 1981 graph (radtran pre 2014)',/,
                '      = 59     Use Hapke 2012 book  page 59 equations',/,
                '      = 60     Use Hapke 2012 book  page 60 equations',/,
		'     DEFAULT = 0 = 1981')


#### these will be added in the future
#/,' Enter  i  to compute the reflectance of intimate-areal mixtures',
#/,' Enter  n  to derive k and n from reflectance spectra',//,

# show defined overlays
        for (iov=1; iov<=6; iov=iov+1) {
                if (ovrflg(iov) > 0) {

                        if (iov == 1) ovcolor='red    '
                        if (iov == 2) ovcolor='blue   '
                        if (iov == 3) ovcolor='green  '
                        if (iov == 4) ovcolor='orange '
                        if (iov == 5) ovcolor='cyan   '
                        if (iov == 6) ovcolor='magenta'
                        write (ttyout,1234) iov,
                                ovfil(iov), ovrec(iov),
                                ovwfil(iov), ovwrec(iov),
                                ovops(iov), ovcolor,
                                ovtitle(iov), ovrchn(iov)
                }
        }
1234    format(1x,'ov', i1,'=',a1,i6,' ',a1,i6,' ',a,' ',a,' ',a,i7)

	call crtin
	i = 1
	call wjfren (i,x,il)
#RED
# Unable to locate varialbe id defined anywhere.  Given the statement
# that immediately follows, assume that this statement should have been
# either deleted or commented out at some point - now commented out
#	if (id == ihe || id == ihx) go to 399
	if (il == ihe || il == ihx) go to 399

	if (il ==  ihce) {                  # look for capital E
		call wjfren (i,x,il)
                if (il == ihcx) {  # now if capital X , exit
                        #            # if graphics = X windows,
                        #                #       reset to HP mode
                        #if (igrmod >= 50 & igrmod <= 59) igrmod = 4

			go to 10000
                }
	}


#       check if overlay defined, form= ov1= v23 V22 color
#       added 12/22/2009

        #write (*,*) "DEBUG: overlay point 1"
        if (i < 83 && il == iho && iopcon(i:i)=='v') {
                iero = 0
                if(iopcon(i-1:i+2) == "ov1=" ) {
                        #write (*,*) "DEBUG: overlay point 2 ov1="
                        i=i+3
                        itmpo=1
                        call getoverly(i,itmpo,iero)
                        #call crtin   # DEBUG
                        go to 399   # go to beginning
                }
                if(iopcon(i-1:i+2) == "ov2=" ) {
                        #write (*,*) "DEBUG: overlay point 2 ov2="
                        i=i+3
                        itmpo=2
                        call getoverly(i,itmpo,iero)
                        go to 399   # go to beginning
                }
                if(iopcon(i-1:i+2) == "ov3=" ) {
                        i=i+3
                        itmpo=3
                        call getoverly(i,itmpo,iero)
                        go to 399   # go to beginning
                }
                if(iopcon(i-1:i+2) == "ov4=" ) {
                        i=i+3
                        itmpo=4
                        call getoverly(i,itmpo,iero)
                        go to 399   # go to beginning
                }
                if(iopcon(i-1:i+2) == "ov5=" ) {
                        i=i+3
                        itmpo=5
                        call getoverly(i,itmpo,iero)
                        go to 399   # go to beginning
                }
                if(iopcon(i-1:i+2) == "ov6=" ) {
                        i=i+3
                        itmpo=6
                        call getoverly(i,itmpo,iero)
                        go to 399   # go to beginning
                }
                if (iero > 0) {
                        go to 399
                }

        }


	if (il == ihr)  call refcom(idummy)
	if (il == ihcr) {  # intimate particulate mixtures in air/vacuum but with embedded materials

		i2=i    # save position of i
		call wjfren (i,x,il)       # if the next value = 1981, 59 or 60,
					   #  set the iflgse to say which Se subroutine to use
		if (il == ihe || il == ihx) go to 399   # exit
		if (il != 0) {                          # error
			call what(i)
			call crtin
			go to 399
		}
		matrixptr=0  # default: no mattric material
		iflgse = 0 # default
		if ( abs(x-59.) < 0.001 )   iflgse = 59  # use Hapke 2012 page 59 equations
		if ( abs(x-60.) < 0.001 )   iflgse = 60  # use Hapke 2012 page 60 equations
		if ( abs(x-1981.) < 0.001 ) iflgse = 0   # use Hapke 1981 equations (original radtran, pre 2014)
		if (i < 80) i2=i   # last position afert input data

		# begin recording input lines to the history array for later output.
		# Note: once set up here, crtin does the rest.

		inhsenbl = 1              # enable history recording
		tinphist(1:i2-1) = iopcon(1:i2-1)
		tinphist(i2:i2) = char(10)  # line feed
		inhistch=i2+1              # next position in the history


		call refmix(idummy)

		inhsenbl = 0  # disable input history recording
	}
	if (il == ihcl) {  # intimate particulate mixtures in LAYERS in air/vacuum but with embedded materials

		call wjfren (i,x,il)       # expect next is the number of layers.
		
		nlayers = x
		if (nlayers < 2 || nlayers > MAXLAYERS) {
			call what(i)
			write (ttyout,*) 'ERROR: layers out of range (2 to', MAXLAYERS
			write (ttyout,*) 'temp: limit is 2 layers as 3 or more needs programming'
			call crtin
			go to 399
		}

		i2=i    # save position of i
		call wjfren (i,x,il)       # if the next value = 1981, 59 or 60,
					   #  set the iflgse to say which Se subroutine to use
		if (il == ihe || il == ihx) go to 399   # exit
		if (il != 0) {                          # error
			call what(i)
			call crtin
			go to 399
		}
		matrixptr=0  # default: no mattric material
		iflgse = 0 # default
		if ( abs(x-59.) < 0.001 )   iflgse = 59  # use Hapke 2012 page 59 equations
		if ( abs(x-60.) < 0.001 )   iflgse = 60  # use Hapke 2012 page 60 equations
		if ( abs(x-1981.) < 0.001 ) iflgse = 0   # use Hapke 1981 equations (original radtran, pre 2014)
		if (i < 80) i2=i   # last position afert input data

		# begin recording input lines to the history array for later output.
		# Note: once set up here, crtin does the rest.

		inhsenbl = 1              # enable history recording
		tinphist(1:i2-1) = iopcon(1:i2-1)
		tinphist(i2:i2) = char(10)  # line feed
		inhistch=i2+1              # next position in the history

		call reflyrmix(idummy)     # layered media reflectance mixtures

		inhsenbl = 0  # disable input history recording
	}
	if (il == ihcm) {   # embedded mixtures, including rocks, oil emulsions

		i2=i    # save position of i
		call wjfren (i,x,il)       # if the next value = 1981, 59 or 60,
					   #  set the iflgse to say which Se subroutine to use
		if (il == ihe || il == ihx) go to 399   # exit
		if (il != 0) {                          # error
			call what(i)
			call crtin
			go to 399
		}
		matrixptr=0  # default: no mattric material
		iflgse = 0 # default
		if ( abs(x-59.) < 0.001 )   iflgse = 59  # use Hapke 2012 page 59 equations
		if ( abs(x-60.) < 0.001 )   iflgse = 60  # use Hapke 2012 page 60 equations
		if ( abs(x-1981.) < 0.001 ) iflgse = 0   # use Hapke 1981 equations (original radtran, pre 2014)
		if (i < 80) i2=i   # last position afert input data

		# begin recording input lines to the history array for later output.
		# Note: once set up here, crtin does the rest.

		inhsenbl = 1              # enable history recording
		tinphist(1:i2-1) = iopcon(1:i2-1)
		tinphist(i2:i2) = char(10)  # line feed
		inhistch=i2+1              # next position in the history

		

		call embedmix(idummy)

		inhsenbl = 0  # disable input history recording
	}
	if (il == iha)  {

		i2=i    # save position of i
		call wjfren (i,x,il)       # if the next value = 1981, 59 or 60,
					   #  set the iflgse to say which Se subroutine to use
		if (il == ihe || il == ihx) go to 399   # exit
		if (il != 0) {                          # error
			call what(i)
			call crtin
			go to 399
		}
		matrixptr=0  # default: no mattric material
		iflgse = 0 # default
		if ( abs(x-59.) < 0.001 )   iflgse = 59  # use Hapke 2012 page 59 equations
		if ( abs(x-60.) < 0.001 )   iflgse = 60  # use Hapke 2012 page 60 equations
		if ( abs(x-1981.) < 0.001 ) iflgse = 0   # use Hapke 1981 equations (original radtran, pre 2014)
		if (i < 80) i2=i   # last position afert input data

		# begin recording input lines to the history array for later output.
		# Note: once set up here, crtin does the rest.

		inhsenbl = 1              # enable history recording
		tinphist(1:i2-1) = iopcon(1:i2-1)
		tinphist(i2:i2) = char(10)  # line feed
		inhistch=i2+1              # next position in the history

		call abscf(idummy)

		inhsenbl = 0  # disable input history recording
	}

#######	if (il == ihn)  call optic(idummy) # will be in the future

	if (il == ihu)  {

		i2=i    # save position of i
		call wjfren (i,x,il)       # if the next value = 1981, 59 or 60,
					   #  set the iflgse to say which Se subroutine to use
		if (il == ihe || il == ihx) go to 399   # exit
		if (il != 0) {                          # error
			call what(i)
			call crtin
			go to 399
		}
		matrixptr=0  # default: no mattric material
		iflgse = 0 # default
		if ( abs(x-59.) < 0.001 )   iflgse = 59  # use Hapke 2012 page 59 equations
		if ( abs(x-60.) < 0.001 )   iflgse = 60  # use Hapke 2012 page 60 equations
		if ( abs(x-1981.) < 0.001 ) iflgse = 0   # use Hapke 1981 equations (original radtran, pre 2014)
		if (i < 80) i2=i   # last position afert input data

		# begin recording input lines to the history array for later output.
		# Note: once set up here, crtin does the rest.

		inhsenbl = 1              # enable history recording
		tinphist(1:i2-1) = iopcon(1:i2-1)
		tinphist(i2:i2) = char(10)  # line feed
		inhistch=i2+1              # next position in the history

		call unmix(idummy)

		inhsenbl = 0  # disable input history recording
	}

#######	if (il == ihi)  call intarl(idummy) # will be in the future

	go to 399

10000	icrst=1
	call rstart(icrst)
	call closef
	stop
	end

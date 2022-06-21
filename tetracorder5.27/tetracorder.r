#HPUX	program tetracorder (ach1, ach2, ach3)
#IA64HPUX   program tetracorder

	implicit integer*4 (i-n)

# This software was written by Roger N. Clark and colleagues and falls under the following license:
# 
# Copyright (c) 1975 - 2022, Roger N. Clark,
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


#ccc  name:         tetracorder
#ccc  version date: 
#ccc  author(s): Roger N. Clark
#ccc  language: ratfor
#ccc
#ccc  short description: map multiple features from spectra of
#ccc                     multiple minerals in an imaging spectrometer data set.
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
#ccc            from a multi-spectral data cube.  The input
#ccc            reference (library) spectrum, wavelengths, and
#ccc            3d data file must all have been previously as-
#ccc            signed using specpr so that the restart file info
#ccc            can be used to open the appropriate files.  The
#ccc            format of the 3d data file must be band inter-
#ccc            leaved by line (BIL), 2-byte integers.  3d data
#ccc            files not of this format cause the program to
#ccc            terminate.  The output of this program is a
#ccc            2-plane image in BIL, 1-byte integers, scaled from
#ccc            values ranging from 0.0 to 1.0 to values ranging
#ccc            from 0 to 255.  In addition to this data file a
#ccc            REMAPP header file and a specpr-like history file
#ccc            is also generated.  These files are written using
#ccc            FORTRAN file i/o.  The output data file is opened
#ccc            and written using C i/o via calls to cdiskio3.
#ccc            Command line arguments ach1,ach2,ach3 are used to
#ccc            provide arguments to the restart module rstbgn in
#ccc            the form:  tetracorder r1 -      where tetracorder is the pro-
#ccc            gram (executable) is called tetracorder, r1 is the name
#ccc            of the restart file and - indicates the non-verbose
#ccc            option.
#ccc---------------------------------------------------------------
#cccVARIABLES:
#ccc            ach1,2,3     - Command line args used in the form:
#ccc                         tetracorder r1 -
#ccc            i4buff       - Integer*4 array used in 3d data cube
#ccc                         reading routines and contains band
#ccc                         depth values on output (equiv'd to
#ccc                         chbuff)
#ccc            i4buf2       - Integer*4 array used in 3d data cube
#ccc                         reading routines and contains quality
#ccc                         of fit values for output (equiv'd to
#ccc                         chbuf2)
#ccc            fcb          - File control block array (I*4) used
#ccc                         in generating remapp headers (equi-
#ccc                         valenced to cfcb)
#ccc            work2        - Working array (equivalenced to
#ccc                         chdata) passed to cdiskio3.  Returns
#ccc                         header file name.
#ccc            i2sht        - Integer*2 array for holding 3d data
#ccc                         sheets (equivalenced to chidata)
#ccc            fname        - File name (char) variable returned
#ccc                         from namdev (dev -> name) routine
#ccc            bdhdr        - Band depth header file name (char)
#ccc            bdfile       - Band depth file name (char)
#ccc            reflib       - Reference spectrum from specpr lib,
#ccc                         equivalenced to datsc1 (R*4)
#ccc            refcrm       - Reference spectrum with continuum
#ccc                         removed, equiv'd to datsc2 (R*4)
#ccc            wavlen       - Wavelengths array, equiv'd to wdata
#ccc                         (R*4)
#ccc            obscrm       - Observed (from 3d data file) con-
#ccc                         tinuum removed, eqv'd to datab (R*4)
#ccc            bdarr        - Band depth array, equiv'd to datsc3
#ccc                         (R*4)
#ccc            rfid         - Reflectance file id (u,v,w or y)
#ccc                         (I*4)
#ccc            rrecno       - Reflectance file record number (I*4)
#ccc            rflun        - Reflectance file unit number (I*4)
#ccc            wfid         - Wavelength file id (use upper case)
#ccc                         (I*4)
#ccc            wrecno       - Wavelength file record number (I*4)
#ccc            wflun        - Wavelength file unit number (I*4)
#ccc            f3did        - 3d data file id (I*4)
#ccc            qlun         - 3d data file unit number (I*4)
#ccc            minch        - Channel (of band) with minimum value
#ccc                         (I*4)
#ccc            dx,y,z       - Dimensions of 3d data file (I*4)
#ccc            dx2          - x-dimension divided by 2 (indicates
#ccc                         the x coordinate of pixel to print
#ccc                         diagnostic info on) (I*4)
#ccc            flag         - Indicates extraction direction (not
#ccc                         relevant in this application) (I*4)
#ccc            ier/ioerr    - Error flag variables (I*4)
#ccc            bdrecl       - Band depth file record length (I*4)
#ccc            icrst        - Update restart file routine code
#ccc                         (I*4)
#ccc            bndstb,e     - Band start ranges (continuum begin.)
#ccc                         (I*4)
#ccc            bndenb,e     - Band end ranges (continuum end)
#ccc                         (I*4)
#ccc            emtogl       - Error message toggle (bandmp arg)
#ccc                         (I*4)
#ccc            imgflg       - Image file flag for bandmp (i*4)
#ccc                           = 1 do not include the continuum channels
#ccc                               in the analysis (for speed)
#ccc                           = 0 DO include continuum channels.
#ccc                               Gregg Swayze 2/10/95 found in the S/N tests
#ccc                               that including the continuum channles can
#ccc                               help results significantly!
#ccc                               All tetracorder previous to 3.1 (2/10/95)
#ccc                                   had imgflg = 1, but as of tetracorder 3.1,
#ccc                               imgflg = 0.
#ccc                                          - Roger N. Clark
#ccc
#ccc            reclen       - 3d data file record length (I*4)
#ccc            rechdr       - 3d data file record header length
#ccc            nrecshdr     - 3d data file header length (in records) (I*4)
#ccc            diaflg       - Diagnostics print flag (I*4)
#ccc            nth          - Diagnostics printed every nth line
#ccc                         (I*4)
#ccc            filorg       - File organization (BIL, BSQ, BIP)
#ccc                         (I*4)
#ccc            i2pack       - Packing routine (written in C)
#ccc                         (I*4)
#ccc            bdnlen       - Band depth file name length (bytes)
#ccc                         (I*4)
#ccc            paksiz       - Number of bits to pack number to
#ccc                         (I*4)
#ccc            outfil       - Band depth output file unit number
#ccc                         (I*4)
#ccc            key          - Record number of 3d data file (obs)
#ccc                         (I*4)
#ccc            idnoff       - DN offset (y=mx+b, where b=idnoff)
#ccc                         (I*4)
#ccc            itmp*        - Temporary variables (I*4)
#ccc            iptdrop      - Point drop value (I*4)
#ccc            oper         - Operation value for cdiskio3 (I*4)
#ccc            indx         - Index for determining # of chars
#ccc                         in a string (I*4)
#ccc            ictlimflg    - continuum threshold set (I*4)
#ccc
#ccc---------------------------------------------------------------
#
# tetracorder 3.3: make not features absolute (as before), or relative
#                to a feature in current material.
#                8/17/95  Roger N. Clark
#
# tetracorder 3.4: add rct, lct continuum thresholding
#                add rct/lct> and lct/rct> checking
#                8/28/95 Roger N. Clark
#
# tetracorder 3.41: add code to not write out image lines with all
#                 zeros; but wait to do it when there is some
#                 non-zero output.  Effectively, this buffers
#                 output so less I/O limits are hit.

# tetracorder 3.4: add sound on output

# tetracorder 3.6: add weak features and output file compression.
#                  weak features must be present but do not
#                  count in the weighted fit or depth calculations.
#                  - Roger N. Clark 1/19/2000
                     
# tetracorder 3.7: fuzzy logic added
#                  added hooks for physical constraints like pressure, temp
#                  - Roger N. Clark 5/11/2001

# tetracorder 4.0: added feature asymmetry shape comparison 
#                                         before continuum removal
#                  added logging and authentication
#                  prep for hooks for spatial conext
#                  prep for DEM tracking
#                  - Roger N. Clark 9/10/2001

# tetracorder 4.1: make r*bd r*abs(bd) so track positive features too.
#                  - Roger N. Clark 8/29/2003

# tetracorder 4.2: restart converted to ASCII; source implimented on LINUX, WINXP
#			- Roger N. Clark Sept. 2009

# Tetracorder 5+: new expert system format with new features and constraints.

# the following block is pre 9/14/2009

#	include 	"../specpr/src.specpr/common/label1"
#	include 	"../specpr/src.specpr/common/lbl3"
#	include 	"../specpr/src.specpr/common/lbl4"
#	include 	"../specpr/src.specpr/common/lbl7"
#	include 	"../specpr/src.specpr/common/lundefs"
#	include 	"../specpr/src.specpr/common/alphabet"
#	include 	"../specpr/src.specpr/common/cmd"
#	include 	"../specpr/src.specpr/common/lblg"
#	include 	"../specpr/src.specpr/common/lblwav"
#	include 	"../specpr/src.specpr/common/cmdarg"
#	include 	"../specpr/src.specpr/common/dscrch"
#	include 	"../specpr/src.specpr/common/ioftyp"
#	include 	"../specpr/src.specpr/common/blank"
#	include		"../specpr/src.specpr/common/lblvol"
#	include		"../specpr/src.specpr/common/iocontrol"

# the following is everything that is in specpr and in the same order

	include "../specpr/src.specpr/common/spmaxes"   # max parameters, must be first

        include "../specpr/src.specpr/common/blank"
        include "../specpr/src.specpr/common/lbl7"
        include "../specpr/src.specpr/common/lbl8"
        include "../specpr/src.specpr/common/lbl6"
        include "../specpr/src.specpr/common/lbl4"
        include "../specpr/src.specpr/common/label1"
        include "../specpr/src.specpr/common/labl2"
        include "../specpr/src.specpr/common/lbl3"
        include "../specpr/src.specpr/common/label3"
        include "../specpr/src.specpr/common/labelf"
        include "../specpr/src.specpr/common/lblg"
        include "../specpr/src.specpr/common/info"
        include "../specpr/src.specpr/common/lblvol"
        include "../specpr/src.specpr/common/lblprt"
        include "../specpr/src.specpr/common/cmd"
        include "../specpr/src.specpr/common/cmdarg"
        include "../specpr/src.specpr/common/lundefs"
        include "../specpr/src.specpr/common/alphabet"
        include "../specpr/src.specpr/common/filenames"
        include "../specpr/src.specpr/common/lblwav"
        include "../specpr/src.specpr/common/sitelogo"
        include "../specpr/src.specpr/common/hptrm"
        include "../specpr/src.specpr/common/pipes"
        include "../specpr/src.specpr/common/iocontrol"
	include "../specpr/src.specpr/common/ioftyp"
	include "../specpr/src.specpr/common/dscrch"
	include "../specpr/src.specpr/common/tetfeat"


#HPUX	character*80 	ach1, ach2, ach3

# arrays for multiple materials

	include "multmap.h"

	include "tricube.h"

# basic tetracorder parameters

	include "tri1.h"

# arrays for buffering output

	include "obuffers.h"

	integer*4 cmdverbose   # function cmdverbose
	integer*4 i, il
	real*4    x

	integer system
	character*10 chtmpa

# on HP-UX ignore underflows

#HPUX	ON REAL UNDERFLOW IGNORE
#IA64HPUX ON REAL UNDERFLOW IGNORE

# Initialize variables
#HPUX	charg1 = ach1
#HPUX	charg2 = ach2
#HPUX	charg3 = ach3

#
#       set  common variables in spblockdata  KEL  06/09
#
        call spblockdata
        iops = '                                        '

#### specpr initialization

        maxrec = SPMAXREC
        maxchn = SPMAXCHAN
        maxtxt = SPMAXTEXT


        delpt    = -1.23e34  # deleted point value
        delptlow = -1.231e34
        delptup  = -1.229e34

        iline=0
        helppg =   'specprhelp                              '

        spcolor=0

# default: do not follow growing files

        yfollow =0
        dfollow =0
        vfollow =0
        wfollow =0

#############################################################
#       set command counter to beginning of file            #
#############################################################
        icoman = 1
        redire = .false.
        cndx = 0
        icopy = 0
        ioutverbose = 0
	pipelv=1
################ end specpr initialization

# initialize tetracorder feature data

# 10/2018:  the following are defined in ../specpr/src.specpr/common/tetfeat
#           imaxtet is defined in ../specpr/src.specpr/common/spmaxes

        tetflgmenu = 0   # turn off menu info in plot
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

        do i = 1, maxgrp {

		groupenable(i) = 0
		groupname(i) = '            '
                             #           111
                             #  123456789012
	}
        do i = 1, maxcse {

		caseenable(i) = 0
		casename(i) = '            '
	}

	iversion = 5.27      # program version

	outfile = 50        # output lun for images
        ihbcksl = char(92)  # this is the backslash character
	lunhist = 61
	lunresult = 62
	qlun = 63
	nrec = 0
	imgflg = 0    # use all channels in analysis to help best
	maxrc=32767.0
	minrc=0.0
	noflush = 1  # flush flushseqfile buffers and restart
	rferfl(1)=1  # issue error messages on specpr
			# read when continuation bit flag is set

	soundenable = 0  # enable sound = 1, not =0

	bdepth = 0.0
	fit    = 0.0

	lsetup = 0  # reference lib setup has not yet been done.

	chfirst= 99999  # first spectral channel used
	chlast =     0  # last  spectral channel used

	dtemperature(1) =    0.0        # data set minimum temperature in Kelvin
	dtemperature(2) = 9999999000.0  # data set maximum temperature in Kelvin

	dpressure(1)    =    0.0        # data set minimum pressure in bars
	dpressure(2)    =  9999999000.0 # data set maximum pressure in bars

	call getcmdargs

# Invoke restart initialization module
	call rstbgn

	i = 0

	write (ttyout,'("tetracorder version =", f5.2)') iversion

#  Comment out to bypass version check
#	write (chtmpa,'(f5.1)') iversion
#	i=system('/usr/local/securebin/authtetracorder' // chtmpa // char(0))
#	i=system('/usr/local/bin/authtetracorder' // chtmpa // char(0))
#  end of version check

	if ( i > 0 ) {
		write (*,*) 'exit ', i
		stop
	}

################################################################
# get ready to start.  This loop until a return allows a command
#                      file to contain lines of "c"s to change
#                      protections on out-of-date restart files.

1	write (ttyout,*) 'Press return to begin tetracorder'
	call crtin
	i=1
	call wjfren(i,x,il)
	if (i < 80) {
			#write (ttyout,*) 'debug: blank line'
			go to 1  # this will happen if there are any
				# characters on the line
	}

# changed 'call er'to 'call eralph'
# segmentation-fault error when display set to X-Windows
#    due to a screen clear 'XWIN call xclear'
#    in 'src.specpr/hpgraph/respag.r'
#	call er
	call eralph

	call whedr

# program start, history file, wavelength set.

	call tri1setup

# now set up reference spectral library features to map on.

	call reflsetup
	if (nmats > 0) {
		itotlf = 0
		do i = 1, nmats {
			itotlf = itotlf + nfeat(i)
		}
	}

##########################################################################

        # ok, we now have temperature and pressure ranges for the incoming
        # data and the library with temperature and pressure constrainbts,
	# so now apply constraints that may disable some features.

        call applygtpconstraints

	call gcsetup  # build cross reference lists for groups, cases
	write (lunresult,*) 'Number of materials being mapped=',nmats
	write (lunresult,*) 'Total spectral features =',itotlf
	call wrtgcsetup (1)

# now let user choose analysis options.

# changed 'call er'to 'call eralph'
# segmentation-fault error when display set to X-Windows
#    due to a screen clear 'XWIN call xclear'
#    in 'src.specpr/hpgraph/respag.r'
#45	call er
45	call eralph

	call whedr


# check authorization file to see if user is qualified to run the program

	
50	if (cmdverbose(-1) <= 4) write (ttyout,51)
51	format (////,' Tetracorder Analysis Options:',//,
		10x, 'Type  l  to setup new reference library spectra',/,
		10x, 'Type  c  to analyze imaging spec BIL data cube',/,
		10x, 'Type  s  to analyze a single spectrum',//,
		10x, 'Type  bn to turn off flushing of buffers',//,
		10x, 'Type  bf to turn on  flushing of buffers',//,
		10x, 'Type  e  or  x  to exit program.',/)

	if (cmdverbose(-1) <= 4) {
		write (ttyout,*) '     Number of materials being mapped=',nmats
		write (ttyout,*) '     Total spectral features =',itotlf
		call wrtgcsetup (0)
	}

	call crtin
	i = 1
	call wjfren (i,x,il)
	if (il==ihx || il==ihe)  {              # e or x exit
		ic=il
		icrst = 1
		call rstart(icrst)
		call closef
		stop
	}
	if (il==ihce & iopcon(i:i) == 'X')  {    # EX exit
		ic=il
		icrst = 1
		call rstart(icrst)
		call closef
		stop
	}
	if ( il == ihb) {
		call wjfren (i,x,il)
		if (il == ihn) {
			noflush = 0
			write (ttyout,*) 'buffer flushing disabled'
			go to 50
		}
		if (il == ihf) {
			noflush = 1
			write (ttyout,*) 'buffer flushing enabled'
			go to 50
		}
	}
	if ( il == ihl) {
		# write history
		write (lunhist,3020) ihbcksl
3020		format ('l',17x,a,'# call reference library setup')
		call reflsetup
		write (lunresult,*) 'NEW reference library setup:'
		if (nmats > 0) {
			itotlf = 0
			do i = 1, nmats {
				itotlf = itotlf + nfeat(i)
			}
		}
		call gcsetup  # build cross reference lists for groups, cases
		write (lunresult,*) 'Number of materials being mapped=',nmats
		write (lunresult,*) 'Total spectral features =',itotlf
		call wrtgcsetup (1)
		go to 50
	}
	if ( il == ihc) {
		# write history
		write (lunhist,3030) ihbcksl
3030		format ('c',17x,a,'# analyze an imaging spectrometer cube')
		if (nmats > 0) {
			do i = 1, nmats {
				fitmean(i) = 0.0
			}
		}
		call cubecorder
		go to 50
	}
	if ( il == ihs) {
		# write history
		write (lunhist,3040) ihbcksl
3040		format ('s',17x,a,'# analyze a spectrum')
		call specorder
		go to 50
	}

	go to 50
	end

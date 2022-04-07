#HPUX	program specpr (ach1, ach2, ach3)
#IA64HPUX       program specpr

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

	include "../common/spmaxes"   # max parameters, must be first

	include "../common/blank"
	include "../common/lbl7"
	include "../common/lbl8"
	include "../common/lbl6"
	include "../common/lbl4"
	include "../common/label1"
	include "../common/labl2"
	include "../common/lbl3"
	include "../common/label3"
	include "../common/labelf"
	include "../common/lblg"
	include "../common/info"
	include "../common/lblvol"
	include "../common/lblprt"
	include "../common/cmd"
	include "../common/cmdarg"
	include "../common/lundefs"
	include "../common/alphabet"
	include "../common/filenames"
	include "../common/lblwav"
	include "../common/sitelogo"
	include "../common/hptrm"
	include "../common/pipes"
	include "../common/iocontrol"
	include "../common/overlys"
	include "../common/sp3pfeat"
	include "../common/tetfeat"
	include "../common/wavemarks"
	include "../common/deletep"
	include "../common/inputhistory"

	character*80 ach1,ach2,ach3

	integer*4 iargc, ier

	data ihzx,ihxz/2hzx,2hxz/
#
#
#
#***********************************************************************
#                                                                      *
#        program written and developed by Roger Nelson Clark           *
#                                                                      *
#***********************************************************************
#                                                                      *
#     LSI 11/23 UNIX version developed 1981,1982                       *
#     converted to ratfor by Jeff Hoover ~1982
#
#     Eunice conversion 1984 by Roger N. Clark
#     HP-UX  conversion 1984 by Roger N. Clark
#                                                                      *
#***********************************************************************
#
#
#     explanation of the file structure :
#       the program uses 6 main files for storage of data.  the user
#       accesses each of the main data records (get a spectrum and its
#       header info) by a letter code signifying one of the files and
#       the record number of the desired spectrum.  the letter id's are
#       : v,w,d,u,y,s ( see user's manual for more details ).  valid
#       record numbers are between 1 and 2000.  note that the program asks
#       the user for the file id and the file number ( technically a record
#       number ) because in the original mit version, each spectrum was a
#       file and not a record as in the current version.
#
#       all i/o is performed by transferring the /label1/ common block to
#       or from disk or tape for the files v,w,d,u, and y.  file 's' is
#       the starpack file which contains the necessary extinction para-
#       meters ( three data arrays - slope, intercept, quality of fit ) so
#       unlabeled common is used.  starpack records 1 to 50 are valid.
#       starpack records can be transferred to the regular data records  for
#       saving ( to w,v,u,d, or y ).  it takes 3 standard specpr data records
#       to hold 1 starpack record.  file d is always a disk file, whereas v,
#       w,u, and y can be either a disk or a tape file.  in order to save
#       disk space, all four of these cannot be assigned to disk or tape only.
#       there are 2 disk and 2 tape files available, thus a special relation-
#       ship is set up between u and v, and w and y.  when v is assigned to
#       disk, u can only be assigned to tape and vice versa.  similarily for
#       w and y.  the histories of operations refer to tape name and file
#       numbers (actually record numbers), thus there are some restrictions
#       for tape to disk and disk to tape transfers.  (eg: to corresponding
#       record numbers only ).  see users manual for more details.
#
#
# TI 980 B (~1982)
# TI 980 B (~1982)
#       transfer : disk to tape takes 40 seconds per 100 files transferred
#       transfer : disk to disk takes 40 seconds per 100 files transferred
#       transfer : tape to disk takes 60 seconds per 100 files transferred
#
#
#
#
#
#
######################################################################

	include "../logo/set.logo"
#HPUX   ON REAL UNDERFLOW IGNORE
#HPUX   ON REAL ILLEGAL CALL e1trap
#IA64HPUX ON REAL UNDERFLOW IGNORE
#IA64HPUX ON REAL ILLEGAL CALL e1trap

#
#       set  common variables in spblockdata  KEL  06/09
#
        call spblockdata

#
#  check for nulls in site logo and set to blanks.
#
	do i = 1, 80 {
		if (logo(i:i) == char(0)) logo(i:i) = ' '
	}

######## was pre 10/25/2015:
#	maxrec = 999999
#	maxchn = 4852
#	maxtxt = 19860

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

# initialize overlay and wavelength marker variables

	wmflgmenu = 0
	ovflgmenu = 0

	do i = 1, 6 {

		wmnum(i) =0
		wmrflg(i)=0
		ovrflg(i)=0
		ovops(i) = '    '
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

#############################################################
#       set command counter to beginning of file            #
#############################################################
	icoman = 1
	redire = .false.
	cndx = 0
	icopy = 0
	ioutverbose = 0
	pipelv=1
##########################################################
#       call initialization routine                      #
##########################################################
#
# first get command line agruments, if any; store in /cmdarg/ common block

	charg1 = ach1
	charg2 = ach2
	charg3 = ach3

	call getcmdargs

	for (i=1; i<=6; i=i+1) {

		ovrflg(i) = -1   # overlays not defined
		ovrflgb(i) = -1   # overlays not defined
		ovrchn(i) = 0    # number of channels in overlay
	}

#DEBUG:	write (ttyout,7790) ncmdarg,charg1
#DEBUG:	7790	format(' ncmdarg=',i9,20x,'charg1=',a)

if (ncmdarg < 1) {
	open(cmdlun,file=COMMAND,access='direct',form='unformatted',
             iostat=ier,recl=80)
	if (ier != 0) {
		write (ttyout,151) ier
		stop
	}
	call wedgea
} else {
	if ((ncmdarg >= 1) & (charg1(1:2) == '-g')) {
		write (ttyout,101)
101		format ('You must specify a restart file before a ',
			'graphics mode',/)
		stop
	}
	call rstart(2)
#############################################################
#       set command counter to beginning of file            #
#############################################################
	icoman = 1
	redire = .false.
	cndx = 0
	icopy = 0
	call taprw
}

# check for graphics settings

if ((ncmdarg >= 2) & (charg2(1:2) == '-g')) {
	igrmod = 0

	#write (ttyout,*)"DEBUG: ", charg2(3:8)
	if (charg2(3:5) == 'xhp') {
		igrmod = 50
		call initt(igrmod)

						# added graphics window size 8/15/2011 - RNC
	} else if (charg2(3:8) == 'xterm3') {   # triple size xterm graphics window
		igrmod = 53
		#write (ttyout,*)"DEBUG: igrmod=", igrmod
		call initt(igrmod)

	} else if (charg2(3:8) == 'xterm2') {   # double size xterm graphics window
		igrmod = 52
		#write (ttyout,*)"DEBUG: igrmod=", igrmod
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

call prochk
111   call eralph
      call whedr
#
# Initialize random seed (used much later)
#
	call initrand
#
#     write control instructions on crt
#
      call mninst (infopr,id,idvx,idv2,inext,icon,ixit,idisu,idad,
		   ictrl,idx,x,xx)
#
#     call requested routines
#

	if (id ==  ihi) {
		if (idx==ihn) infopr=0
		else infopr=1

	} else if (id ==  ihg) {
		write (ttyout,1000)
		call gfit

	} else if (id == iht | id == ihl | id == ihm) {
		if (id!=ihl) idx=id
		write (ttyout,1001)
		call reddis(idx)

	} else if (id ==  ihb) {
		ititle(1:2) = 'zx'
		call wedgea

	} else if (id ==  ihf) {
		write (ttyout,1002)
		call rstdmp

	} else if (id ==  ihp) {
		write (ttyout,1003)
		call gpplot

	} else if (id ==  ihs) {
		write (ttyout,1004)
		call extnct

	} else if (id ==  ihr) {
		ititle(1:2)='xz'
		call wedgea

	} else if (id ==  ihce) {                  # look for capital E
		if (idx == ihcx) {  # now if capital X , exit
                        #            # if graphics = X windows, 
			#		#	reset to HP mode
			# do not do this anymore: we are not on HP terminals
			#if (igrmod >= 50 & igrmod <= 59) igrmod = 4

			call closef
			call rstart(1)
			stop
		}

	} else {

		xjunk=0
	}
	go to 111
151	format(' open error', i6, ' on command file')
1000    format(' Transferring to GFIT')
1001    format(' Transferring to display & math routine')
1002    format(' Transferring to restart dump routine')
1003    format(' Transferring to Gould plot routine')
1004    format(' Transferring to extinction routine')
	end


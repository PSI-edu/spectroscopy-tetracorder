#HPUX program oceanopticstospecpr(ach1,ach2,ach3)
#     Ratfor

# R. Clark, January 2020.

#***************  oceanopticstospecpr  *****************************
#
#     this program reads data from the an Oceanoptics spectrometer
#          output and puts it into SPECPR format.
#
#     Exampel data:
#            Data from water-h2o-liq-X-mm-trans2_Reflection_02.txt Node
#            Date: Sun Aug 18 13:57:21 MDT 2019
#            User: rclark
#            Spectrometer: FLMS02693
#            Trigger mode: 0
#            Integration Time (sec): 2.000000E0
#            Scans to average: 10
#            Electric dark correction enabled: true
#            Nonlinearity correction enabled: false
#            Boxcar width: 0
#            XAxis mode: Wavelengths
#            Number of Pixels in Spectrum: 2048
#            >>>>>Begin Spectral Data<<<<<
#            189.497	271.43
#            189.881	271.43
#
#     Usage:
#           oceanopticstospecpr  specprfilename  oceanopticsfilename [-w]
#           oceanopticstospecpr  specprfilename  oceanopticsfilename [-p wavpointer respointer]
#
#********************************************************************
#
	implicit integer*4 (i-n)

	include "../src.specpr/common/spmaxes"   # max parameters, must be first

	include "../src.specpr/common/label1"
	include "../src.specpr/common/lbl4"
	include "../src.specpr/common/cmdarg"
	include "../src.specpr/common/alphabet"
#
#
#HPUX	character*80 ach1, ach2, ach3
	real*4 datsav(SPMAXCHAN)

        character*80 title
        character*40 datatyp, xunit, yunit
	character*4 tmonth
	character*256 spfilnam, oceanfile

	integer*4 lnbspf, lnbocf  # last non blanks for spfilnam, oceanfile
	integer*4 ocflun          # oceanoptics file lun
	integer*4 spflun          # specpr file lun

        integer*4 xfactor, ilastc
        real*4 npoints, deltax, yfactor, firstx, lastx, firsty

	real*4 yscale   # scale factor for the data
#

	character*1 ichil
	character*40 itlsav
	character*40 btitle
	character*1536 dummy
	equivalence (dummy,ititl)

	integer*4 recnum, fsize, arglen
	integer*4 idummy, filsiz
	integer*4 ier, ihplus, lun
	integer*2 chkbit, itmp2
	real*4 exem, sec
	integer*4 ihour, imin

	integer*4 wavptr, resptr # wavelength, resolution pointers

	integer*4 jmhist, jhist   # pointers to last chanracter in the array
#
	common /dta1/ wave(SPMAXCHAN),reflec(SPMAXCHAN)
	common /dta1/ sig(SPMAXCHAN),sam(SPMAXCHAN)
	common /dta1/ ref(SPMAXCHAN),dark(SPMAXCHAN),del(SPMAXCHAN)
#

###old	data ihplus/'+'/

	equivalence (itimch,exem)
#
#HPUX	charg1 = ach1
#HPUX	charg2 = ach2
#HPUX	charg3 = ach3

#
#
#       set  common variables in spblockdata
#
        call spblockdata

        maxrec = SPMAXREC
        maxchn = SPMAXCHAN
        maxtxt = SPMAXTEXT   # was 19860
        iline=0
        ihistch=0

	luncrt = 6
	lunin = 5

	spflun = 86   # specr file lun
	ocflun = 88   # oceanfile lun

	ihplus = ihchar('+')

	wavptr = 0   # wavelength pointer
	resptr = 0   # resolution pointer

	yscale = 1.0   # scale factor

#
#      get number of arguments from command line after program name.
#
	call getcmdargs

	stopflag = 0
	if (ncmdarg < 2) {
		stopflag = 1
	}
#
11	if (stopflag == 1) {
		write (luncrt,1)
1      		format (1x, 'input not fully specified after program name.',
		        1x, 'proper use is:',/,
			1x,' oceanopticstospecpr specprfilename ',
			   ' oceanopticsfilename  [-w]',/,
			1x,' oceanopticstospecpr specprfilename ',
			   ' oceanopticsfilename  [-p wavepointer resolpointer] [-scale yscale]',/)
		close (spflun,iostat=ier)
		stop
	}

	do j= 1,40 {
		btitle(j:j) = ' '   # set title to blanks
	}

	spfilnam  = charg1  # specpr output file name
	oceanfile = charg2  # oceanoptics input file name
#
#       open the output specpr file
#
10	lnbspf = lnb(spfilnam)  # last non blank character
 	open (spflun,file=spfilnam(1:lnbspf), iostat=ier,status='old',
             access='direct',recl=1536, form='unformatted')
#
       if (ier != 0) {
		write (luncrt,15) ier, spfilnam(1:lnbspf)
15		format (1x, 'cant open file, error',1x, i6,'   file=',a,/)
			write (luncrt,*) "stopping"
		stop
       }
#
#       Open the input oceanoptics file
#
	lnbocf = lnb(oceanfile)
	open (ocflun, file = oceanfile(1:lnbocf), access = 'sequential',
                        form = 'formatted', status = 'unknown',
                        iostat = ier)
       if (ier != 0) {
		write (luncrt,15) ier, oceanfile(1:lnbocf)
		stop
       }

	write (luncrt,*) oceanfile(1:lnbocf)

	# title
	i2 = lnbocf
	if (i2 > 4) {
		if (oceanfile(i2-3:i2) == '.txt') {
			i2 =  i2 - 4   # do not include .txt
		}
	}
	if (i2 > 40) i2 =40
	btitle(1:i2) = oceanfile(1:i2)
	do j= 1,40 {
		if (btitle(j:j) == char(0)) {
			btitle(j:j)=' '
		}
		if (btitle(j:j) == '_') {   # change underbars, _, to blanks
			btitle(j:j)=' '
		}
	}

	# wavelengths
	igetwaves = 0
	if (ncmdarg >= 3) {
		if ( charg3(1:2) == "-w" ) {

			igetwaves = 1
			write (luncrt,*) "Only getting Wavelengths"
		}
		if ( charg3(1:3) == "-wr" ) {

			igetwaves = 2
			write (luncrt,*) "Only getting Wavelengths, and adding resolution placeholder"
		}
		if ( charg3(1:2) == "-p" ) {   # get wavelength, resolution pointers
			if (ncmdarg >= 5) {

				iopcon(1:50) = charg4(1:50)
				i=1
				call wjfren(i,x,il)
				if (il != 0) {
					write(luncrt,*) "Error getting wavelength pointer"
					write(luncrt,*) "Stopping"
					stop
				}
				wavptr = int(x)

				iopcon(1:50) = charg5(1:50)
				i=1
				call wjfren(i,x,il)
				if (il != 0) {
					write(luncrt,*) "Error getting resolution pointer"
					write(luncrt,*) "Stopping"
					stop
				}
				resptr = int(x)

				if (wavptr < 1) wavptr = 0
				if (resptr < 1) resptr = 0

				if ( charg6(1:6) == "-scale" ) {   # get wavelength, resolution pointers
					#write(luncrt,*) "debug: ncmdarg= ", ncmdarg
					#write(luncrt,*) "debug: charg6 = ", charg6
					#write(luncrt,*) "debug: charg7 = ", charg7
					if (ncmdarg >= 7) {
						iopcon(1:50) = charg7(1:50)
						i=1
						call wjfren(i,x,il)
						if (il != 0) {
							write(luncrt,*) "Error getting scale factor, not found", charg7
							write(luncrt,*) "Stopping"
							stop
						}
						yscale = x
						if (yscale < 0.1e-20) {
							write (luncrt,*) "Error: scale factor too small: ", yscale
							write (luncrt,*) " "
							stopflag = 1
							go to 11   # stopping
						}
						write (luncrt,*) "scale factor= ", yscale
					} else {
						write (luncrt,*) "Error: expecting scale factor, not found"
						write (luncrt,*) " "
						stopflag = 1
						go to 11   # stopping
					}
				}

			} else {
				write (luncrt,*) "Error: expecting wavelength, resolution pointers"
				write (luncrt,*) " "
				stopflag = 1
				go to 11   # stopping
			}
		}
		if ( charg3(1:6) == "-scale" ) {   # get wavelength, resolution pointers
			if (ncmdarg >= 4) {
				iopcon(1:50) = charg4(1:50)
				i=1
				call wjfren(i,x,il)
				if (il != 0) {
					write(luncrt,*) "Error getting scale factor, not found", charg4
					write(luncrt,*) "Stopping"
					stop
				}
				yscale = x
				if (yscale < 0.1e-20) {
					write (luncrt,*) "Error: scale factor too small: ", yscale
					write (luncrt,*) " "
					stopflag = 1
					go to 11   # stopping
				}
				write (luncrt,*) "scale factor= ", yscale
			} else {
				write (luncrt,*) "Error: expecting scale factor, not found"
				write (luncrt,*) " "
				stopflag = 1
				go to 11   # stopping
			}
		}
	}
#
# get current length of file, test if consistent with specpr file
# and set initial output record number to the end of the data
#     already in the file.
#
	filsiz = fsize(spfilnam)
#
	if (mod(filsiz,1536).ne.0) {
		write (luncrt,30) spfilnam, filsiz
30		format (1x, a, " does not appear to be a specpr file",
			/, "length of", i9, 
			" is not a multiple of 1536",/)
			write (luncrt,*) "stopping"
		stop
	}
#
	if (filsiz > 0) {
		recnum = (filsiz-1536)/1536
	}
	if (filsiz < 0) recnum = 0
#
# open cmd file for crtin  (this is the .cmd file used by specpr, not needed here)
#
	open (16, file='/dev/null', access='direct',recl=80,
		iostat=ier,form='unformatted')
	if (ier != 0) {
		write (luncrt, 1623) ier
1623		format (1x, 'cant open /dev/null, error=', 1x, i6)
			write (luncrt,*) "stopping"
		stop
	}

#
# record in which to write the data is the next one after the end.
#

#
# initialize arrays
#
50	do i = 1, 3 {
		sta(i)(1:2)='  '
		stb(i)(1:2)='  '
		ira(i)=0
		idec(i)=0
	}
#
	isgn = 1
	istb=0
	isra=0
	isdec=0
	irmas = 0
	revs = 1
	iband(1)=1
	iband(2)=1
	irwav=0
	irespt=0
	itpntr=0
	nruns=1
	siangl=0
	seangl=2000000000
	sphase=2000000000
	ieros =0
	iwtrns=1
	xnrm=1.0
	tempd= 293.0
	nchant = 0
	nchans = 0
	iscta = 0
	isctb = 0
	jdatea = 0
	jdateb = 0
	minchn = 4097
	itchan = 0
	firstx = 0
	lastx  = 0
	maxchn = 4852
	maxrec = 99999

	ichan = 0   # spectral channel number

#
#	set flags
#
	iwvflg=0
	idkflg = 0
	irfflg = 0
	ismflg = 0
	irflfg = 0
	isgflg = 0
	idlflg = 0
	iemxfl = 0

	do j = 1, 60 {
		ihist(j:j) = ' '
	}
	do j = 1, 296 {
		mhist(j:j) = ' '
	}
#
# set initial values for channel to deleted point
#
	do j = 1, maxchn {
		wave(j)  = -1.23e34
		reflec(j)= -1.23e34
		sig(j)   = -1.23e34
		del(j)   = -1.23e34
		sam(j)   = -1.23e34
		dark(j)  = -1.23e34
		data(j)  = -1.23E34
	}
#
# clear all bits in the bit flags
#
	do j = 1, 32 {
		itmp2 = j
		call clrbit (icflag,itmp2)
	}

# title is the (modified) file name

	ititl = btitle

	do j= 1,40 {
		if (ititl(j:j) == char(0)) {
			ititl(j:j)=' '
		}
	}

# history is the oceanoptics file name

	ihist(1:5) = 'From '
	jhist = 6   # position counter
	i2 = jhist + lnbocf
	if (lnbocf > 55) i2 = 55     # note: this could truncat the file name
	i3 = 60
	if (i2 < 55) i3 = i2+5
	ihist(6:i3) = oceanfile(1:i2)
	
	do j= 1,60 {
		if (ihist(j:j) == char(0)) {
			ihist(j:j)=' '
		}
	}

	jmhist = 0     # position counter for mhist
#
# read header info from standard input and put into specpr labeled
#      common
#
60	read (ocflun, 100, end=6000) iopcon
100    format (80a)
#
# Data from water-h2o-liq-X-mm-trans2_Reflection_02.txt Node
# Date: Sun Aug 18 13:57:21 MDT 2019
# User: rclark
# Spectrometer: FLMS02693
# Trigger mode: 0
# Integration Time (sec): 2.000000E0
# Scans to average: 10
# Electric dark correction enabled: true
# Nonlinearity correction enabled: false
# Boxcar width: 0
# XAxis mode: Wavelengths
# Number of Pixels in Spectrum: 2048
# >>>>>Begin Spectral Data<<<<<
#
        if (iopcon (1:24) == '>>>>>Begin Spectral Data') go to 598 # now read the data
#
	if (iopcon(1:9) == 'Data from') {    # file name, also works as title
		mhist(1:60)=iopcon(10:69)
		do j= 1,60 {
			if (ihist(j:j) == char(0)) {
				ihist(j:j)=' '
			}
		}
		jmhist = lnb(mhist)
		go to 60    # read next line
	}
#
# Date: Sun Aug 18 13:57:21 MDT 2019
#          11111111112222222222333333
# 12345678901234567890123456789012345

	if (iopcon(1:5) == 'Date:') {

		write (luncrt,*)  iopcon(1:lnb(iopcon))   # date line
		tmonth= iopcon(11:14)
		imon=0
		if        ( tmonth == 'Jan ' ) {
			imon = 1
		} else if ( tmonth == 'Feb ' ) {
			imon = 2
		} else if ( tmonth == 'Mar ' ) {
			imon = 3
		} else if ( tmonth == 'Apr ' ) {
			imon = 4
		} else if ( tmonth == 'may ' ) {
			imon = 5
		} else if ( tmonth == 'Jun ' ) {
			imon = 6
		} else if ( tmonth == 'Jul ' ) {
			imon = 7
		} else if ( tmonth == 'Aug ' ) {
			imon = 8
		} else if ( tmonth == 'Sep ' ) {
			imon = 9
		} else if ( tmonth == 'Oct ' ) {
			imon = 10
		} else if ( tmonth == 'Nov ' ) {
			imon = 11
		} else if ( tmonth == 'Dec ' ) {
			imon = 12
		} else {
			imon = -1
		}
		if (imon < 1) {
			write (luncrt,*) "Error decoding month, got ", tmonth
		}
		i=15
		call wjfren(i,x,il)
		if (il != 0 ) {
			write (luncrt,120) il, iopcon
120			format (1x, 'error getting date day: ', a1,
				' encountered',/, a)
			call what (i-1)
			write (luncrt,*) "stopping"
			stop
		}
		iday = x

		# Date: Sun Aug 18 13:57:21 MDT 2019

		# now get time 13:57:21

		call wjfren(i,x,il)
		if (il != 0 & il != ihchar(':')) {
			write (luncrt,121) il, iopcon
121			format (1x, 'error getting time: ', a1,
				' encountered',/, a)
			call what (i-1)
			write (luncrt,*) "stopping"
			stop
		}
		ihour = x
		#write (*,*) "debug: hour:", ihour

		call wjfren(i,x,il)
		if (il != 0 & il != ihchar(':')) {
			write (luncrt,121) il, iopcon
			call what (i-1)
		}
		imin = x
		#write (*,*) "debug: minutes:", imin

		call wjfren(i,x,il)
		if (il != 0 & il != ihchar(':')) {
			write (luncrt,121) il, iopcon
			call what (i-1)
		}
		sec = x
		#write (*,*) "debug: seconds:", sec
		call frdms(itime,24000,ihour,imin,sec,isgn)

		iscta = itime
		isctb = itime

		# now finish date
		# Date: Sun Aug 18 13:57:21 MDT 2019

		i = 30
		call wjfren(i,x,il)
		if (il != 0 ) {
			write (luncrt,120) il
			write (luncrt,*) "stopping"
			stop
		}
		iy = x

		call  tojuld (iy,imon,iday,jday)
		jdatea = jday
		jdateb = jday

		go to 60  # read next line
	}

# Trigger mode: 0

	if (iopcon (1:13)== 'Trigger mode:') {
		# not needed now, so skip
		#  write(luncrt,*) "debug: ", iopcon(1:lnb(iopcon)),' jmhist=', jmhist
		i2 = lnb(iopcon)
		jmhist = jmhist +2
		if (jmhist + i2 < 296) {
			mhist(jmhist:jmhist+i2) = iopcon(1:i2)
		}
		jmhist = lnb(mhist)
		go to 60  # read next line
	}

# Electric dark correction enabled: true

	if (iopcon (1:13)== 'Electric dark') {
		# not needed now, so skip
		# write(luncrt,*) "debug: ", iopcon(1:lnb(iopcon)),' jmhist=', jmhist
		i2 = lnb(iopcon)
		jmhist = jmhist +2
		if (jmhist + i2 < 296) {
			mhist(jmhist:jmhist+i2) = iopcon(1:i2)
		}
		jmhist = lnb(mhist)
		go to 60  # read next line
	}

# Nonlinearity correction enabled: false

	if (iopcon (1:23)== 'Nonlinearity correction') {
		# not needed now, so skip
		#  write(luncrt,*) "debug: skip: ", iopcon(1:lnb(iopcon)),' jmhist=', jmhist
		i2 = lnb(iopcon)
		jmhist = jmhist +2
		if (jmhist + i2 < 296) {
			mhist(jmhist:jmhist+i2) = iopcon(1:i2)
		}
		jmhist = lnb(mhist)
		go to 60  # read next line
	}

# Boxcar width: 0

	if (iopcon (1:13)== 'Boxcar width:') {
		# not needed now, so skip
		#write(luncrt,*) "debug: skip: ", iopcon
		go to 60  # read next line
	}

# XAxis mode: Wavelengths

	if (iopcon (1:11)== 'XAxis mode:') {
		# not needed now, so skip
		#write(luncrt,*) "debug: skip: ", iopcon
		go to 60  # read next line
	}

# Number of Pixels in Spectrum: 2048

	if (iopcon (1:9) == 'Number of') {
		i=30
		call wjfren (i,x,il)
		nchans=x 
		npoints=nchans
		if (nchans > maxchn) {
			write (luncrt,510) maxchn, nchans
510			format (1x, 'nchans exceeds maximum channels (',
					i6,'):',i6)
			write (luncrt,*) "stopping"
			stop
		}
		write (luncrt,*) "Number of channels= ", nchans
		go to 60  # read next line
	}
#
# User: rclark
#

        if (iopcon (1:5) == 'User:') {
		usernm(1:8)=iopcon(7:14)
 		do j=1,8 {
			if (usernm(j:j) == char(0)) {
				usernm(j:j) = ' '
			}
		}
		go to 60  # read next line
	}
#
# Spectrometer: FLMS02693
#
	if (iopcon (1:13) == 'Spectrometer:') {
		i2 = lnb(iopcon)
		jmhist = jmhist +2
		if (jmhist + i2 < 296) {
			mhist(jmhist:jmhist+i2) = iopcon(1:i2)
		}
		jmhist = lnb(mhist)
		go to 60  # read next line
	}
#
# Integration Time (sec): 2.000000E0
#
#
	if (iopcon(1:16) == 'Integration Time') {
		i=24
		call rlchng (i,x,il)  # get real number, including e format
 		scatim = x            # 1-spectrum integration time
		if (scatim < 0.1e-30) {
			write (luncrt,520) x
520			format (1x,'scatim out of range:', f14.7)	
			write (luncrt,*) "stopping"
			stop
		}
		go to 60  # read next line
	}
#
	if (iopcon (1:9) == '##XUNITS=') {
		xunit(1:40)=iopcon(10:50)
		do j=1,40 {
			if (xunit(j:j) == char(0)) xunit(j:j)=' '
		}
		go to 60  # read next line
	}
#
# Scans to average: 10
#
	if (iopcon (1:7) == 'Scans to average:') {
		i=18
		call wjfren (i,x,il)
		nruns = int(x)
			if (nruns < 1) {
				write (luncrt,530) 
530				format (1x,'nruns value less than 1')
				write (luncrt,*) "stopping"
				stop
			}
		go to 60  # read next line
	}
#
# >>>>>Begin Spectral Data<<<<<
#
	if (iopcon (1:24) == '>>>>>Begin Spectral Data') {

		go to 598  # read  the data
	}
#
	go to 60  # read next line
#
# -----now read data---------------------------------
#
# compute number of channels, and how to load data, longest wavelength
# at top of array.

598     if (nchans < 1) {
		write (luncrt,*) "ERROR: number of channels < 1, exiting"
		#go to 5000      # exit
			write (luncrt,*) "stopping"
		stop
	}

	timint = scatim * float(nruns)   # total integrating time

#    Now expect 2 values: wavelength <tab> y-value <control-m>

	ihtab1 = ihchar('	')   # that is a tab character
	ihrtrn = ihchar('')        # carriage return

600	read (ocflun, 100, end=2000) iopcon

	i=1
	call wjfren (i,x,il)
	if (il !=0 && il != ihtab1) {
		write (luncrt,610) il, iopcon
610               format (1x, 'Expecting wavelength, strange character ', a,
			' encountered in the line:',
			/, a, /, 1x, 'Data are CORUPTED: DO NOT USE',/)
		#go to 600  # read next line
			write (luncrt,*) "stopping"
		stop
	}

	wv1 = x /1000.0    # wavelength at beginning of line, convert nm to microns

	# i = i+1 not needed

	call rlchng (i,x,il)
	if (il !=0 && il != ihrtrn) {
		write (luncrt,612) il, iopcon
612               format (1x, 'Expecting DATA, strange character ', a,
			' encountered in the line:',
			/, a, /, 1x, 'Data are CORUPTED: DO NOT USE',/)
		#go to 600  # read next line
			write (luncrt,*) "stopping"
		stop
	}

	ichan = ichan +1           # channel number
	y1 = x  * yscale           # data value

	wave(ichan) = wv1             # wavelenths
	data(ichan) = y1              # data

	if (ichan == 1 || ichan == nchans) {
		 write (luncrt,*) ichan, wv1, y1
	}
#DEBUG:
		# write (luncrt,*) ichan, wv1, y1

	go to 600
#
#  now write the data to the specpr data file
#

2000    itlsav(1:40)=ititl(1:40)
2001	format('itlsav =  ',a)

#
# if time is not set, get dates ant times from system
#
	if (iscta == 0) {

		call sptimb  # set date and time of acquisition and processing

	} else {
		call sptime  # set date and time of acquisition
	}

	
	if ( ichan != nchans) {

		write (luncrt,2003) nchans, ichan
2003		format ("Error: expecting",i7," channels, found",i7)
	}

	write (luncrt,2005)
2005	format (1x, 'data acquired, now send to specpr file')
#
	itchan=ichan

# the following only works for bit 6 = 0 (no fluorescence data)

	if (igetwaves >= 1) {
		ititl(33:40)=' waves  '
		ititl(1:32)= 'wavelengths to following data   '

		do ii = 1, itchan {
			datsav(ii) = data(ii)
			data(ii) = wave(ii)
		}
		recnum = recnum + 1
		write (luncrt, 2010) recnum, ititl, itchan
2010		format (1x,i6,3x, a, 3x, 'channels=', i5)
		call wrtspr(recnum,spflun,ier)

		if (ier != 0) {
			write (luncrt,*) "WRITE ERROR: ", ier
			write (luncrt,*) "exit"
			stop
		}

		# ititl(1:40)=itlsav(1:40)
		# do ii = 1, itchan {
		# 	data(ii) = datsav(ii)
		# }

		if (igetwaves == 2) {
			ititl(33:40)=' resol  '
			ititl(1:32)= 'resolution (FWHM) placeholder   '

			do ii = 1, itchan {
				data(ii) = 0.0
			}

			filsiz = fsize(spfilnam)
			recnum = (filsiz-1536)/1536
			recnum = recnum + 1

			write (luncrt, 2010) recnum, ititl, itchan
			call wrtspr(recnum,spflun,ier)

			if (ier != 0) {
				write (luncrt,*) "WRITE ERROR: ", ier
				write (luncrt,*) "exit"
				stop
			}
		}

		# -w, -wr now means only do wavelengths, so stop here

		close (spflun,iostat=ier)
		stop
	}
#
	irwav  = wavptr
	irespt = resptr

	recnum = recnum + 1
	write (luncrt, 2010) recnum, ititl, itchan
	call wrtspr(recnum,spflun,ier)

		if (ier != 0) {
			write (luncrt,*) "WRITE ERROR: ", ier
			write (luncrt,*) "exit"
			stop
		}

	ititl(1:40)=itlsav(1:40)
	write (luncrt, *) ' '
#
#
# completed now go back to beginning and wait for the next spectrum
#
#
# done
#
5000    close (spflun,iostat=ier)
	stop

6000    close (spflun,iostat=ier)
	write(luncrt,*) "Error: premature EOF on oceanoptics input file"
	write(luncrt,*) "Stopping"
	stop
#
	end

#HPUX	program cgastosp (ach1, ach2, ach3)
#IA64HPUX	program cgastosp
#     Ratfor
#********************* cgastosp ***********************************
#
#     this program reads data from the IRTF CGAS translated data and
#          puts it into specpr format.  
#
# A Sample input sequence:
#
# SIMPLE  =                    F
# TELESCOP= 'NASA-IRTF         '
# INSTRUME= 'CGAS (32 chan)    '
# OBSERVER= 'UNKNOWN           '
# SYSID   = '02/07/6'
# OBJECT  = '                  '
# COMMENT = '                                                  '
# DATE-OBS= '27/08/88    102453' / dd/mm/yy hhmmss U.T. 
# OBSNUM  =                   35
# RA      =             10220.56
# DEC     =              74938.3
# EPOCH   =              UNKNOWN
# HA      =            -23552.78
# RA-OFF  =                  0.2
# DEC-OFF =                  0.0
# FILTER  =                    6
# INT-TIME=               10.000
# AMASS-B =                1.298 /Airmass at beginning
# AMASS-E =                1.291 /Airmass at end
# CHAN'S  =                   32
# INTEG'S =                    1
# POSIT'S =                    1
# STEPSIZE=                    0
# GSTEP-B =                 6977
# SPACING =                  306 / Channel spacing in grating steps
# OCTETS  =                    1 / No. of beamswitch cycles
# MFLAG   =                    0 / MFLAG= 0: Octets, MFLAG= 1: Scans
# COMMENT   Special Interim Format for CGAS (vers6/7) data
# COLUMNS =                    7
# ROWS    =                  128
# COLUMN1 = 'Grating Step #    '
# FORMAT1 = 'i10               '
# COLUMN2 = 'Channel #         '
# FORMAT2 = 'i5                '
# COLUMN3 = 'A value           '
# FORMAT3 = 'e13.4             '
# COLUMN4 = 'B value           '
# FORMAT4 = 'e13.4             '
# COLUMN5 = 'Avg A-B           '
# FORMAT5 = 'e13.4             '
# COLUMN6 = 'Std-Dev of Avg    '
# FORMAT6 = 'e13.4             '
# COLUMN7 = 'Signal-to-noise ratio'
# FORMAT7 = 'e13.4             '
# END
# OCTET  1: 1
#    11873    1  -1.9501e-01  -3.8452e-02  -1.5656e-01   0.0000e+00   0.0000e+00
#    11567    2   4.3823e-01  -2.1362e-02   4.5959e-01   0.0000e+00   0.0000e+00
#        .
#        .
#        .
#     2693   31   1.1639e+00   7.8735e-02   1.1221e+00   3.6926e-02   3.0389e+01
#     2387   32   1.1185e+00   8.8806e-02   1.0658e+00   3.6164e-02   2.9472e+01
# SUMMARY
# 35   UT27/08/88 102453                                                     
# 10.000     1   1   0   6   1 306  10220.56   74938.3 1.298 1.291   6977  32
#    1  -4.3335e-02   5.2861e-02
#    2   4.2877e-01   1.4326e-02
#    .
#    .
#    .
#   31   1.1192e+00   1.6643e-02
#   32   1.0641e+00   1.6056e-02
#
#  A keyword has been added:
#  Gratwav = num1 num2 num3
#  where num1 num2 and num3 are the constants to compute the
#  wavelengths from the grating step positions.  If you change the order
#  of the grating, put in a new Gratwav.  Note Gratwav is not all uppercase.
#  Samples (for now no e-format is allowed):
#
#  Gratwav = 43823.2  0.48474 -0.000000105088  / Mars grating order 1
#  Gratwav = 21911.6  0.24237 -0.000000052544  / Mars grating order 2
#  Gratwav = 14607.7  0.16158 -0.0000000350293 / Mars grating order 3
#
#  the computation of wavelength is in microns:
#      wav = (num1 + num2 * step + num3 * step**2)/10000.0
#
#  Gratwav entries will set the wavelength flag and generate a wavelength
#  set and grating position set.  The wavelength pointer of all subsequent
#  data sets will point to the new wavelength set!
#
#####################################
#  Added new keywords (10/30/90):
#
#  Wavptr n G# r#
#               where n  is an array pointer to store the following
#                        numbers (1 to 20)
#               and   G# is a grating step number
#               and   r# is the wavelength record pointer
#
#  Example: 
#
#  Wavptr 1 G 6977 r 12    # wavelength set for G6977 is in record 12
#  Wavptr 2 G 2000 r 13    # wavelength set for G2000 is in record 13
#
#  when a G# is encountered that has a pointer predefined, that
#  pointer is used.  If it is not predefined, a new wavelength set is
#  computed.  The G# is stored in array gptr, and r# is stored in
#  array wavptr
#
######
#  Only gratwav for order 1 is needed now.
#  The order is determined by the filters in use, so the array
#
#  Forder o1 o2 o3 o4 o5 o6
#
#  can be defined where the filter number is one to 6, setting the
#  order number for each filter.  These values are stored in array "order"
#
#  Example:
#
#  Forder 1 1 1 1 1 2
#
#         sets the order to 1 for filters 1 to 5 and to order 2 for filter 6
#
#********************************************************************
#
	implicit integer*4 (i-n)

	include "../../src.specpr/common/spmaxes"
	include "../../src.specpr/common/label1"
	include "../../src.specpr/common/lbl4"
	include "../../src.specpr/common/cmdarg"
#
#HPUX	character*80 ach1,ach2,ach3

	character*1 ichil
	character*40 itlsav
	character*80 filnam
	character*1536 dummy
	equivalence (dummy,ititl)

	integer*4 recnum, fsize, arglen, iscale
	integer*4 idummy, filsiz
	integer*4 ier, maxgptr, wavptr(20), gptr(20), iwgptr
	integer*2 chkbit, itmp2
	real*4 exem, sec
	real*4 order(6)
#
	common /dta1/ wave(4096),reflec(4096),sig(4096),gstep(4096)
#
	equivalence (itimch,exem)
#
	luncrt = 6
	lunin = 5
#
#      the spectrum number translated is in the following
#
	specnum = 0          # spectrum number
	gstep17 = -9999999   # grating step position given by GSTEP-B keyword
	gsteplast = -9999999  # last grating position
	iline = 0    # line number of input data
	inrmlz = 1   # normalize octet data (set to 0 for no normalization)
#
#      get number of arguments from command line after program.
#
#HPUX	charg1 = ach1
#HPUX	charg2 = ach2
#HPUX	charg3 = ach3

	call getcmdargs

#DEBUG:	write (ttyout,7790) ncmdarg,charg1
#DEBUG:	7790	format(' ncmdarg=',i9,20x,'charg1=',a)
	n = ncmdarg
	filnam = charg1
	if (n.ge.1) go to 10
#
	write (luncrt,1)
1      format (1x, 'input file not fully specified after program name',
		/, 1x, 'proper use is cgastosp (specpr)filname',
                        ' <sourcefilename',/)
#
	go to 5000
#
10	lun = 10
 	open (lun,file=filnam,iostat=idummy,status='old',
		access='direct',recl=1536,
		form='unformatted')
       if (idummy.ne.0) {
		write (luncrt,15)idummy
15		format (1x, 'cant open file, error',1x, i6,/)
		stop
       }
#
# get current length of file, test if consistent with specpr file
#     and set initial output record number to the end of the data
#     already in the file.
#
	filsiz = fsize(filnam)
#
	if (mod(filsiz,1536).ne.0) {
		write (luncrt,30) filnam, filsiz
30		format (1x, a, " does not appear to be a specpr file",
			/, "length of", i9, 
			" is not a multiple of 1536",/)
		stop
	}
#
	if (filsiz.gt.0) {
		recnum = (filsiz-1536)/1536
	}
	if (filsiz.le.0) recnum = 0
#
# open cmd file for crtin
#
	open (16, file='/dev/null', access='direct',recl=80,
		iostat=ier,form='unformatted')
#
# initialize wavelength pointers and filter order numbers
#
	do i = 1, 6 {
		order(i) = 1.0
	}
	do i = 1, 20 {
		wavptr(i) = 0
		gptr(i)   = 0
	}
	maxgptr = 0  # number of grating and wavelength pointers
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
#	write (luncrt,"(' DEBUG: starting data set decode')")
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
	seangl=0
	sphase=0
	ieros =0
	iwtrns=1
	xnrm=1.0
	tempd= 293.0
	nchant = 0
	nchans = 0
	minchn = 4097
	itchan = 0
	maxchn = 4852
	maxrec = 999999

	do i = 1, 60 {
		ihist(i:i) = ' '
	}
	do i = 1, 296 {
		mhist(i:i) = ' '
	}

#
#	set flags
#
	iwvflg=0
	iwvflg2=0
	iwgptr = 0
	idkflg = 0
	irfflg = 0
	ismflg = 0
	irflfg = 0
	isgflg = 0
	idlflg = 0
	iemxfl = 0
	irecptr = 0
	mflag = -1
#
# set initial values for channel to deleted point
#
	do j = 1, 4096 {
		wave(j)=-1.23e34
		reflec(j)=-1.23e34
		sig(j)=-1.23e34
		gstep(j)=-1.23e34
	}
#
# Set data array not filled by data to zero
#
	do j = 4096,maxchn{
		data(j) = 0.0
	}
#
# clear all bits in the bit flags
#
	do j = 1, 32 {
		itmp2 = j
		call clrbit (icflag,itmp2)
	}
#
# times are in UT, so set bits 4, 5
#
	itmp2 = 4
	call setbit(icflag,itmp2)
	itmp2 = 5
	call setbit(icflag,itmp2)
#
# read header info from standard input and put into specpr labeled
#      common
#
60	read (lunin,100,end=5000)iopcon
	iline = iline + 1
100    format (a)

#
	if (iline == 1) {
		if (iopcon(1:9) != 'Gratwav =') {
			write (luncrt, 101)
101			format (' ERROR: first line of input must be a ',
				'"Gratwav =" keyword in order to compute',
				' wavelengths.',/)
			stop
		}
	}
	if (iopcon(1:6) == 'Forder') {
		i = 7
		do iiz = 1, 6 {
			call wjfren (i,x,il)
			if (il != 0) {
				write (luncrt, 103) iiz
103				format (' "Forder" ERROR in finding ',
					'order number for filter',i3)
				stop
			}
			if (x < 1 | x > 5) {
				write (luncrt,104) iix
104				format (' "Forder" ERROR:  the order',
					' must be > 0 and < 6',/,9x,
					' Order number for filter',i3)
				stop
			}
			order(iiz) = int(x+0.5)
			write (luncrt,*) 'Forder: filter ',iiz,
					' = order ',order(iiz)
		}
	}
#
# Wavelength pointers
#
	if (iopcon(1:6) == 'Wavptr') {
		i = 7
		call wjfren (i,x,il)
		if (il != 0) {
			write (luncrt, 106) iline
106			format (' "Wavptr" ERROR: strange ',
				'character encountered',/,9x,
				' Line number',i8)
			stop
		}
		if (x < 1 | x > 20) {
			write (luncrt,107) x
107			format (' "Wavptr" ERROR:  the array',
				' pointer must be > 0 and < 21',/,9x,
				' Array pointer is',f7.0)
			stop
		}
		ngptr = int(x +0.5)

		call wjfren(i,x,il)
		if (il != ihchar('G')) {
			write (luncrt, *) 'Wavptr ERROR: expected a G'
			stop
		}

		call wjfren(i,x,il)
		if (il != 0) {
			write (luncrt, 106) iline
			stop
		}
		gptr(ngptr) = nint(x)

		call wjfren(i,x,il)
		if (il != ihchar('r')) {
			write (luncrt, *) 'Wavptr ERROR: expected an r'
			stop
		}

		call wjfren(i,x,il)
		if (il != 0) {
			write (luncrt, 106) iline
			stop
		}
		if (x < 1 | x > recnum) {
			write (luncrt,108) recnum+1, x
108			format (' "Wavptr" ERROR:  the wavelength',
				' pointer must be > 0 and <',i5,/,9x,
				' Array pointer is',f7.0)
			stop
		}
		wavptr(ngptr) = int(x+0.5)

		if (maxgptr < ngptr) maxgptr = ngptr

		write (luncrt,*) 'Grating step ',gptr(ngptr),
				' assigned to wavelength pointer ',
				wavptr(ngptr)
	}

	if (iopcon(1:9).eq.'SIMPLE  =') go to 60
	specnum = specnum + 1
#
	if (iopcon(1:10).eq.'OBJECT  = ') {
		ititl(1:18)=iopcon(12:29)
		ititl(19:29) = " IRTF CGAS "
		ititl(30:40) = " "
		do j= 1,40 {
			if (ititl(j:j).eq.char(0)) ititl(j:j)=' '
		}
	}
#
	if (iopcon(1:10).eq.'OBSERVER= ') {
		usernm(1:8)=iopcon(12:19)
		do j= 1,8 {
			if (usernm(j:j).eq.char(0)) usernm(j:j)=' '
		}
	}
##
	if (iopcon(1:9).eq.'DATE-OBS=') {
		do j= 1,80 {
			if(iopcon(j:j)=='-'){
				iopcon(j:j)='/'
			}
		}
		i=12
		call wjfren(i,x,il)
		if (il != 0 & il != ihchar('/')) {
			write (luncrt,120) il
120			format (1x, 'error getting jdatea: ', a1,
				' encountered',/)
		}
		iday = x + 0.5
		call wjfren(i,x,il)
		if (il != 0 & il != ihchar('/')) {
			write (luncrt,120) il
		}
		imon = x + 0.5

		call wjfren(i,x,il)
		if (il != 0 & il != ihchar('/')) {
			write (luncrt,120) il
		}
		iy = x + 1900.5

		call  tojuld(iy,imon,iday,jday)
		jdatea = jday
		jdateb = jday
#
#               time is scrunched together, so extract it and put in :
#
		iopcon(1:2) = iopcon(24:25)
		iopcon(3:3) = ':'
		iopcon(4:5) = iopcon(26:27)
		iopcon(6:6) = ':'
		iopcon(7:8) = iopcon(28:29)
		iopcon(9:9) = ' '

		i =1
		call wjfren(i,x,il)
		if (il != 0 & il != ihchar(':')) {
			write (luncrt,130)il 
130			format (1x, 'error getting observation time: ', a1,
				' encountered',/)
		}
		id = x + 0.5

		call wjfren(i,x,il)
		if (il != 0 & il != ihchar(':')) {
			write (luncrt,130)il 
		}
		im = x + 0.5

		call wjfren(i,x,il)
		if (il != 0 & il != ihchar(':')) {
			write (luncrt,130)il 
		}
		sec = x
		isgn = 1
#DEBUG:
#		write (6,*)'id=',id,'im=',im,'sec=',sec,'isgn=',isgn

		iscale = 24000
		call frdms(itime,iscale,id,im,sec,isgn)

		iscta = itime
		isctb = itime
	}
#
	if (iopcon(1:9).eq.'RA      =') { 
		isgn = 1
#
#               RA is scrunched together, so extract it and put in :
#
		iopcon(1:2) = iopcon(22:23)
		iopcon(3:3) = ':'
		iopcon(4:5) = iopcon(24:25)
		iopcon(6:6) = ':'
		iopcon(7:11) = iopcon(26:30)
		iopcon(12:12) = ' '
		i =1
		call wjfren(i,x,il)
		if (il != 0 & il != ihchar(':')) {
			write (luncrt,131)il 
131			format (1x, 'error getting RA: ', a1,
				' encountered',/)
		}
		id = x + 0.5

		call wjfren(i,x,il)
		if (il != 0 & il != ihchar(':')) {
			write (luncrt,131)il 
		}
		im = x + 0.5

		call wjfren(i,x,il)
		if (il != 0 & il != ihchar(':')) {
			write (luncrt,131)il 
		}
		sec = x
		iscale = 1000
		call frdms(isra,iscale,id,im,sec,isgn)

	}
#
	if (iopcon(1:9).eq.'DEC     =') { 
#
#               DEC is scrunched together, so extract it and put in :
#
		isgn = 1
		do ii = 21, 28 {
			if (iopcon(ii:ii) == '-') {
				iopcon(ii:ii) = ' '
				isgn = -1
			}
		}
		iopcon(1:3) = iopcon(22:24)
		iopcon(4:4) = ':'
		iopcon(5:6) = iopcon(25:26)
		iopcon(7:7) = ':'
		iopcon(8:11) = iopcon(27:30)
		iopcon(12:12) = ' '
		i =1
		call wjfren(i,x,il)
		if (il != 0 & il != ihchar(':')) {
			write (luncrt,132)il 
132			format (1x, 'error getting DEC: ', a1,
				' encountered',/)
		}
		id = abs(x) + 0.5

		call wjfren(i,x,il)
		if (il != 0 & il != ihchar(':')) {
			write (luncrt,132)il 
		}
		im = x + 0.5

		call wjfren(i,x,il)
		if (il != 0 & il != ihchar(':')) {
			write (luncrt,132)il 
		}
		sec = x
		iscale = 1000
		call frdms(isdec,iscale,id,im,sec,isgn)

	}
#
	if (iopcon(1:9).eq.'HA      =') { 
		isgn = 1
		do ii = 20, 28 {
			if (iopcon(ii:ii) == '-') {
				iopcon(ii:ii) = ' '
				isgn = -1
			}
		}
#
#               HA is scrunched together, so extract it and put in :
#
		iopcon(1:2) = iopcon(22:23)
		iopcon(3:3) = ':'
		iopcon(4:5) = iopcon(24:25)
		iopcon(6:6) = ':'
		iopcon(7:11) = iopcon(26:30)
		iopcon(12:12) = ' '
		i =1
		call wjfren(i,x,il)
		if (il != 0 & il != ihchar(':')) {
			write (luncrt,133)il 
133			format (1x, 'error getting HA: ', a1,
				' encountered',/)
		}
		id = x + 0.5

		call wjfren(i,x,il)
		if (il != 0 & il != ihchar(':')) {
			write (luncrt,133)il 
		}
		im = x + 0.5

		call wjfren(i,x,il)
		if (il != 0 & il != ihchar(':')) {
			write (luncrt,133)il 
		}
		sec = x
		iscale = 24000
		call frdms(isha,iscale,id,im,sec,isgn)
#
#               now ST = RA + HA
#
		istb = isra*24 + isha
#
		isecperday = 2073600000  # this = seconds per day * 24000
		if (istb > isecperday) {
			istb = istb - isecperday
		}
		if (istb < 0) {
			istb = istb + isecperday
		}

	}
#
	if (iopcon(1:10).eq.'INTEG''S = ') {
		i=12
		call wjfren(i,x,il)
		if (il.ne.0) { 
			ichil = char(il)
			write (luncrt,150) ichil
150			format (1x, 'error getting revs: ', 1a,
				' encountered',/)
		}
		revs=x
	}
#
	if (iopcon(1:10).eq.'GSTEP-B = ') {
		i=12
		do i = i, 80 {
			if (iopcon(i:i) != ' ') break
		}
		ititl(30:40) = 'G=' // iopcon(i:i+8)
		do j= 1,40 {
			if (ititl(j:j).eq.char(0)) ititl(j:j)=' '
		}
		call wjfren(i,x,il)
		if (il.ne.0) { 
			ichil = char(il)
			write (luncrt,152) il
152			format (1x, 'error getting GSTEP-B: ', 1a,
				' encountered',/)
		}
		gsteplast = gstep17
		gstep17=x
		iwgptr = 0                    # unset flag that says wavelength
						# pointer are preset by
						# Wavptr keyword
		if (maxgptr > 0) {
			do iiz = 1, maxgptr {
				if (gptr(iiz) == gstep17) iwgptr = wavptr(iiz)
			}
		}
		if (iwgptr == 0) {
			if (gstep17 != gsteplast) {
				iwvflg2 = 1        # Compute new wavelength set.
			} else {
				iwvflg2 = 0
			}
		}
		write (ihist(46:60),"('Gr Pos=',i8)") gstep17
#		write (luncrt,"(' DEBUG: ihist Gr=',a)") ihist
	}
#
	if (iopcon(1:10).eq.'MFLAG   = ') {
		mflag = -1
		i=12
		call wjfren(i,x,il)
		if (il.ne.0) { 
			ichil = char(il)
			write (luncrt,153) il
153			format (1x, 'error getting MFLAG: ', 1a,
				' encountered',/)
		}
		mflag=x+0.5
		ihist(1:16) = 'NASA IRTF CGAS: '
		if (mflag == 0) {                  # Octets
			ihist(17:23) = 'Octets '
		} else if (mflag == 1) {           # Scans
			ihist(17:23) = 'Scans  '
		} else {
			ihist(17:23) = '?????? '   # Don't know what it is!
		}
	}
#
	if (iopcon(1:10).eq.'FILTER  = ') {
		i=12
		call wjfren(i,x,il)
		if (il.ne.0) { 
			ichil = char(il)
			write (luncrt,154) il
154			format (1x, 'error getting FILTER: ', 1a,
				' encountered',/)
		}
		ifiltr = x + 0.5
		write (ihist(24:33),"('Filter=',i2,' ')") ifiltr
#		write (luncrt,"(' DEBUG: ihist Fl=',a)") ihist
	}
#
	if (iopcon(1:10).eq.'OBSNUM  = ') {
		i=12
		call wjfren(i,x,il)
		if (il.ne.0) { 
			ichil = char(il)
			write (luncrt,155) il
155			format (1x, 'error getting OBSNUM: ', 1a,
				' encountered',/)
		}
		iobsnum = x + 0.5
		write (ihist(34:45),"('Obsnum=',i4,' ')") iobsnum
#		write (luncrt,"(' DEBUG: Obsnum=',i4)") iobsnum
#		write (luncrt,"(' DEBUG: ihist Ob=',a)") ihist
	}
#
	if (iopcon(1:10).eq.'RA-OFF  = ') {
		i=12
		call wjfren(i,x,il)
		if (il.ne.0) { 
			ichil = char(il)
			write (luncrt,156) il
156			format (1x, 'error getting RA-OFF: ', 1a,
				' encountered',/)
		}
		raoffset = x
		mhist(1:16) = 'CGAS: RA offset='
		write (mhist(17:24),"(f8.1)") raoffset
	}
#
	if (iopcon(1:10).eq.'DEC-OFF = ') {
		i=12
		call wjfren(i,x,il)
		if (il.ne.0) { 
			ichil = char(il)
			write (luncrt,158) il
158			format (1x, 'error getting DEC-OFF: ', 1a,
				' encountered',/)
		}
		decoffset = x
		mhist(25:37) = ', DEC offset='
		write (mhist(38:45),"(f8.1)") decoffset
	}
#
	if (iopcon(1:10).eq.'POSIT''S = ') {
		i=12
		call wjfren(i,x,il)
		if (il.ne.0) { 
			ichil = char(il)
			write (luncrt,159) il
159			format (1x, 'error getting POSIT''S: ', 1a,
				' encountered',/)
		}
		iposits = x + 0.5
		mhist(46:57) = ', Positions='
		write (mhist(58:59),"(i2)") iposits
		mhist(60:74) = '               '
		if (iposits > 1) {
			write (luncrt,15901) iposits
15901			format (' WARNING: this program does not yet',
				' handle multiple positions, so the data',/,
				'        for this observation will not',
				' be decoded properly.',/,
				'        positions=',i6)
		}
	}
#
	if (iopcon(1:10).eq.'STEPSIZE= ') {
		i=12
		call wjfren(i,x,il)
		if (il.ne.0) { 
			ichil = char(il)
			write (luncrt,161) il
161			format (1x, 'error getting STEPSIZE: ', 1a,
				' encountered',/)
		}
		istepsize = x
		mhist(75:89) = 'CGAS: Stepsize='
		write (mhist(90:94),"(i5)") istepsize
	}
#
	if (iopcon(1:10).eq.'SPACING = ') {
		i=12
		call wjfren(i,x,il)
		if (il.ne.0) { 
			ichil = char(il)
			write (luncrt,162) il
162			format (1x, 'error getting SPACING ', 1a,
				' encountered',/)
		}
		ispacing = x
		mhist(95:129) = ', Detector spacing (grating steps)='
		write (mhist(130:134),"(i5)") ispacing
	}
#
	if (iopcon(1:10).eq.'OCTETS  = ') {
		i=12
		call wjfren(i,x,il)
		if (il.ne.0) { 
			ichil = char(il)
			write (luncrt,163) il
163			format (1x, 'error getting OCTETS: ', 1a,
				' encountered',/)
		}
		ioctets = x
		mhist(131:137) = 'Octets='
		write (mhist(138:142),"(i5)") ioctets
		mhist (143:144) = ' '
	}
#
	if (iopcon(1:11).eq.'hdr:itimch=') {
		i=15
		call wjfren(i,x,il)
		if (il.ne.0) {
			ichil = char(il)
			write (luncrt,160) il
160			format (1x, 'error getting itimch: ', 1a,
				' encountered',/)
		}
		itimch=x + 0.5
	}
#
	if (iopcon(1:11).eq.'hdr:history') {
		ihist(1:60)=iopcon(15:74)
		do j = 1, 60 {
			if (ihist(j:j).eq.char(0)) ihist(j:j) =' '
		}
	}
#
	if (iopcon(1:5).eq.'hdr:1') {
		mhist(1:74) = iopcon(6:79)
		do j = 1, 74 {
			if (mhist(j:j).eq.char(0)) mhist(j:j) = ' '
		}
	}
#
	if (iopcon(1:5).eq.'hdr:2') {
		mhist(75:148) = iopcon(6:79)
		do j = 75, 148 {
			if (mhist(j:j).eq.char(0)) mhist(j:j) = ' '
		}
	}
#
	if (iopcon(1:5).eq.'hdr:3') {
		mhist(149:222) = iopcon(6:79)
		do j = 149, 222 {
			if (mhist(j:j).eq.char(0)) mhist(j:j) = ' '
		}
	}
#
	if (iopcon(1:5).eq.'hdr:4') {
		mhist(223:296) = iopcon(6:79)
		do j = 223, 296 {
			if (mhist(j:j).eq.char(0)) mhist(j:j) = ' '
		}
	}
#
	if (iopcon(1:10).eq.'INT-TIME= ') {
		i=11
		call wjfren(i,x,il)
		if (il.ne.0) {
			ichil = char(il)
			write (luncrt,190) ichil
190			format (1x, 'error getting INT-TIME: ', 1a,
				' encountered',/)
		}
		itimch= x*1000.0
	}
#
	if (iopcon(1:10).eq.'AMASS-B = ') {
		i=11
		call wjfren(i,x,il)
		if (il.ne.0) {
			ichil = char(il)
			write (luncrt,200) il
200			format (1x, 'error getting AMASS-B: ', 1a,
				' encountered',/)
		}
		amassb=x
#
#               read next line: it should be AMASS-E
#
		read (lunin,100,end=5000)iopcon
		iline = iline + 1

		if (iopcon(1:10).eq.'AMASS-E = ') {
			i=11
			call wjfren(i,x,il)
			if (il.ne.0) {
				ichil = char(il)
				write (luncrt,201) il
201				format (1x, 'error getting AMASS-E: ', 1a,
					' encountered',/)
			}
			amasse=x
		} else {
			write (luncrt,203)
203			format (' ERROR: AMASS-E not found after AMASS-B',/,
				' Stopping!')
			stop
		}
		amass = (amassb + amasse)/2.0
		irmas = amass * 1000.0
	}
#
	if (iopcon(1:10).eq.'CHAN''S  = ') {
		i=11
		call wjfren(i,x,il)
		if (il.ne.0) {
			ichil = char(il)
			write (luncrt,210) ichil
210			format (1x, 'error getting nchans: ', 1a,
				' encountered',/)
		}
		nchant = x + 0.5
		itchan = x + 0.5
	}
#
	if (iopcon(1:9).eq.'Gratwav =') {
		i=10
		call wjfren(i,x,il)
		if (il.ne.0) {
			ichil = char(il)
			write (luncrt,220) char(il)
220			format (1x, 'error getting Gratwav: ', 1a,
				' encountered',/)
		}
		gn1=x

		call wjfren(i,x,il)
		if (il.ne.0) {
			ichil = char(il)
			write (luncrt,220) char(il)
		}
		gn2=x
		iwvflg = 1

		call wjfren(i,x,il)
		if (il.ne.0) {
			ichil = char(il)
			write (luncrt,220) char(il)
		}
		gn3=x
		iwvflg = 1

		if (gn1 == 0 | gn2 == 0 | gn3 == 0) {
			write (luncrt,222) gn1, gn2, gn3
222			format (' WARNING: a grating constant is zero:',/,
				' gn1=',f14.5, ' gn2=',f14.7, ' gn3=',f15.9)
		}
	}
#
#
#
# the following are not in the CGAS, left them here for future.
#The dd mm ss.ssss of iangl,eangl,and phase are separated by blank
#spaces. Iangl and eangl are valid in the ranges +/- 90 and
# phase is valid for +/- 180.
#
	if (iopcon(1:9).eq.'hdr:iangl') { 
		do j= 10,25{
			if(iopcon(j:j) == '-')isgn= -1
		}
		i =15
		call wjfren(i,x,il)
		if (il != 0 ) {
			write (luncrt,260)il 
260			format (1x, 'error getting iangl: ', a1,
				' encountered',/)
		}
		id = x + 0.5

		call wjfren(i,x,il)
		if (il != 0 ) {
			write (luncrt,260)il 
		}
		im = x + 0.5

		call wjfren(i,x,il)
		if (il != 0 ) {
			write (luncrt,260)il 
		}
		sec = x
		iscale = 6000
		call frdms(itime,iscale,id,im,sec,isgn)

		siangl = itime
	}
#
#
#
	if (iopcon(1:9).eq.'hdr:eangl') { 
		do j= 10,25{
			if(iopcon(j:j) == '-')isgn= -1
		}
		i =15
		call wjfren(i,x,il)
		if (il != 0 ) {
			write (luncrt,270)il 
270			format (1x, 'error getting eangl: ', a1,
				' encountered',/)
		}
		id = x + 0.5

		call wjfren(i,x,il)
		if (il != 0 ) {
			write (luncrt,270)il 
		}
		im = x + 0.5

		call wjfren(i,x,il)
		if (il != 0 ) {
			write (luncrt,270)il 
		}
		sec = x
		iscale = 6000
		call frdms(itime,iscale,id,im,sec,isgn)

		seangl = itime
	}
#
#
	if (iopcon(1:9).eq.'hdr:phase') { 
		do j= 10,25{
			if(iopcon(j:j) == '-')isgn= -1
		}
		i =15
		call wjfren(i,x,il)
		if (il != 0 ) {
			write (luncrt,280)il 
280			format (1x, 'error getting phase: ', a1,
				' encountered',/)
		}
		id = x + 0.5

		call wjfren(i,x,il)
		if (il != 0 ) {
			write (luncrt,280)il 
		}
		im = x + 0.5

		call wjfren(i,x,il)
		if (il != 0 ) {
			write (luncrt,280)il 
		}
		sec = x
		iscale = 6000
		call frdms(itime,iscale,id,im,sec,isgn)

		sphase = itime
	}
#
#
	if (iopcon(1:9).eq.'hdr:temp') { 
		i=15
		call wjfren (i,x,il)
		xsup=0.0
		if (il.ne.0) {
			if (il != ihchar('E')) {
				write (luncrt,290)il 
290				format (1x, 'error getting temp: ', a1,
					' encountered',/)
			}
			if (il == ihchar('E')) {
				xtmp=x
				call wjfren (i,x,il)
				if (il == ihchar('+')){
                        		call wjfren (i,x,il)
				}
				if (il.ne.0) {
					write (luncrt,290) il, iopcon
				}
				xsup = x
				if (xsup.gt.37.0) xsup=37.0
				if (xsup.lt.-37.) xsup=-37.0
				xsup = int(xsup)
				x=xtmp
			}
		}
#
		tempd=x*10.0**xsup
	}
#
#
#--- now read data --------------------------
#
	if (iopcon(1:3).eq.'END') {
		igtrip = 0
		if (iwvflg2 == 1) iwvflg = 1
#
1000		read (lunin,100,end=5000)iopcon
		iline = iline + 1
#
#		write (luncrt,32000) iline, iopcon
#32000		format (' DEBUG: line=',i6,',  input data line:',a,/)
#
		if (iopcon(1:9) == 'SIMPLE  =') {
			write (luncrt,1004) iline, iopcon
1004			format (' ERROR: "SIMPLE" keyword encountered ',
				'before end of data set',/,
				' Line:', i6,':',/,a,//,
				' Going to decode new data set;',
				' this data set not written fo file',/)
			go to 50
		}

		if (iopcon(1:9) == 'Gratwav =') {
			write (luncrt,1005) iline, iopcon
1005			format (' ERROR: "Gratwav" keyword encountered ',
				'before end of data set',/,
				' Line:', i6,':',/,a,//,
				' CANOT recover from this error, STOPING')
			stop
		}
		if (iopcon(1:7) == 'SUMMARY') {
			read (lunin,100,end=5000)iopcon  # summary line 1
			iline = iline + 1
			read (lunin,100,end=5000)iopcon  # summary line 2
			iline = iline + 1
			go to 1008  # ******** Read and then write the data!***
		}
		if (((iopcon(1:6) == 'OCTET ' & mflag == 0) ||
				(mflag == 1)) &
				(igtrip == 0)) {   # read grating positions
						# and save, but only
						# do once (igtrip = 0).
			if (iwvflg == 0) go to 1000

			do ii = 1, itchan {  # read and decode step numbers
				if (mflag == 0 && ii == 1) { # this is an Octet,
								# so didn't 
								# read first 
								# line above
					read (lunin,100,end=5000)iopcon
					iline = iline + 1
				}
				if (ii > 1) {  # read all lines after first one
					read (lunin,100,end=5000)iopcon
					iline = iline + 1
				}
				if (iopcon(1:6) == 'OCTET ') {
					write (luncrt,1001) ii, itchan
1001					format (' ERROR: "OCTET" keyword',
						' encountered before end of',
						' grating step data',/,
						' Expected channel',i3,
						' out of',i3)
				}
				if (iopcon(1:7) == 'SUMMARY') {
					write (luncrt,1003) ii, itchan
1003					format (' ERROR: "SUMMARY" keyword',
						' encountered before end of',
						' grating step data',/,
						' Expected channel',i3,
						' out of',i3)
				}
				if (iopcon(1:9) == 'SIMPLE  =') {
							# ERROR, take action
					write (luncrt,1004) iline, iopcon
					go to 50
				}
				if (iopcon(1:9) == 'SIMPLE  =') {
							# ERROR, take action
					write (luncrt,1005) iline, iopcon
					stop
				}

				i=1
				call wjfren (i,x,il)
				if (il.ne.0) {
					write (luncrt,1011) il, iopcon
1011					format (1x, 'strange character ', 1a, 
						' encountered in the ',
						'GRATING STEP data line:',
						/, a, /)
				}
				gxtmp = x

				call wjfren (i,x,il)
				if (il.ne.0) {
					write (luncrt,1010) il, iopcon
				}
				igxtmp = x + 0.5
				if (ii != igxtmp) {
					write (luncrt,1002) ii, igxtmp
1002					format (' ERROR: channel number out',
						' of sequence when reading',
						' grating positions!',/,
						' Wanted:',i5,'  got:',i5)
				}
				gstep(ii) = gxtmp
				wave(ii) = (gn1 + gn2*gstep(ii) + 
						gn3*(gstep(ii)**2))/10000.0
			}
			igtrip = igtrip +1
		}

		go to 1000

1008        do ii = 1, itchan {
		read (lunin,100,end=5000)iopcon  # summary line: data
		iline = iline + 1

		if (iopcon(1:9) == 'SIMPLE  =') {  # ERROR, take action
			write (luncrt,1004) iline, iopcon
			go to 50
		}

		if (iopcon(1:9) == 'Gratwav =') {  # ERROR, take action
			write (luncrt,1005) iline, iopcon
			stop
		}

		i=1
		call wjfren (i,x,il)
		if (il.ne.0) {
			write (luncrt,1010) il, iopcon
1010			format (1x, 'strange character ', 1a, 
				' encountered in the DATA line:',
				/, a, /)
		}
#
# get channel
#
		if ((x.lt.1.0).or.(x.gt.4096)) {
			write (luncrt,1020) x, iopcon
1020			format (1x, 'channel out of range ', f9.2, ' in line:',
				/, a, /)
			go to 60
		}
		if (ii != int(x +0.5)) {
			write (luncrt,1002) ii, x
		}

#
		ichan  = x + 0.5
		nchans = nchans +1
		ixchan = ichan
		if(x < minchn) minchn = x     # minimum channel
		if(x > itchan) itchan = x     # maximum channel
#
# get data value
#
1040		call wjfren (i,x,il)
		xsup=0.0
		if (i.ge.80) go to 60
		if (il.ne.0) {
			if (il != ihchar('e') & il != ihchar('E')) {
				write (luncrt,1010) il, iopcon
			}
			if (il == ihchar('e') || il == ihchar('E')) {
				xtmp=x
				call wjfren (i,x,il)
				if (il == ihchar('+')){
                        		call wjfren (i,x,il)
				}
				if (il.ne.0) {
					write (luncrt,1010) il, iopcon
				}
				xsup = x
				if (xsup.gt.37.0) xsup=37.0
				if (xsup.lt.-37.) xsup=-37.0
				xsup = int(xsup)
				x=xtmp
			}
		}
#
		reflec(ixchan)=x*10.0**xsup
		irflfg = 1
#
# get error: standard deviation of the mean
#
1050		call wjfren (i,x,il)
		xsup=0.0
		if (i.ge.80) go to 60
		if (il.ne.0) {
			if (il != ihchar('e') & il != ihchar('E')){
				write (luncrt,1010) il, iopcon
			}
			if (il == ihchar('e') || il == ihchar('E')){
				xtmp=x
				call wjfren (i,x,il)
				if (il == ihchar('+')){
                        		call wjfren (i,x,il)
				}
				if (il.ne.0) {
					write (luncrt,1010) il, iopcon
				}
				xsup = x
				if (xsup.gt.37.0) xsup=37.0
				if (xsup.lt.-37.) xsup=-37.0
				xsup = int(xsup)
				x=xtmp
			}
		}
#
		sig(ixchan)=x*10.0**xsup
		isgflg = 1
#
           }
#
#  now write the data to the specpr data file
#
2000    itlsav(1:40)=ititl(1:40)
2001	format('itlsav =  ',a)
#
#	write (luncrt,2005)
#2005	format (1x, 'data acquired, now send to specpr file', /)
#
	
	if (iwvflg == 1 & iwgptr == 0) {
		ititl(33:40)=' gstep  '
		ititl(1:32)= 'Grating steps to following data '
		mhist(223:238) = '\\W Grating Steps'

		do ii = 1, itchan {
			data(itchan - ii + 1) = gstep(ii)
		}
		recnum = recnum + 1
		irwav = recnum + 1
		write (luncrt, 2010) recnum, ititl, itchan
		call wrtspr(recnum,lun,ier)

		ititl(33:40)=' waves  '
		ititl(1:32)= 'wavelengths to following data   '
		mhist(223:238) = '                '

		do ii = 1, itchan {
			data(itchan - ii + 1) = wave(ii)
		}
		irwav = recnum   # wave pointer is grating step set.
		recnum = recnum + 1
		recptr = recnum  # wave pointer for data is this wave set.
		write (luncrt, 2010) recnum, ititl, itchan
2010		format (1x,i6,3x, a, 3x, 'channels=', i5)
		call wrtspr(recnum,lun,ier)
		ititl(1:40)=itlsav(1:40)
	}
	if (iwgptr > 0) recptr = iwgptr  # preset wavelength pointer
                                         # from Wavptr keywords
	irwav = recptr   # wave set pointer
#
# compute total integration time
# inresolved question: should "revs" = octets *4?, right now, revs
# is set only by INTEG'S value, but an octet actually reads the array
# 8 times.
#   scantim is computed assuming the settling time of the telescope
# is 3.0 seconds (the usual default), but it could be different, and
# it is not encoded in the cgas data info.
#
	if (mflag == 0) {    #case for octets
		timint = (float(itimch)/1000.0) *
				float(ioctets)*8.0 * float(revs)
		scatim = timint + float(ioctets)*8.0*3.0
	}
	if (mflag == 1) {    # case for scans
		timint = float(itimch)/1000.0 * float(revs) * 2.0
		scatim = timint + float(revs) * 2.0
	}
#
# NORMALIZE input data if it is an Octet (mflag = 0) and have enough channels
#
	inrm = 0  # haven't normalized yet
	if (mflag == 0 & inrmlz == 1 & itchan > 7) {   # normalize
		refsum = 0.0
		itmp = itchan - 7
		if (itmp < 1) {
			write (luncrt,"(' NRMLZ: insufficient channels')")
		}
		do ii = 4, itchan - 4 {
			refsum = refsum + reflec(ii)
		}
		refsum = refsum / float(itmp)
		if (abs(refsum) < 0.1e-15) {
			write (luncrt, 2015)
2015			format (' WARNING: data to near to zero to',
				' believably normalize.  Not normalizing')
			inrm = 0
		} else {
#			write (luncrt,"(' DEBUG: normalizing')")
			do ii = 1, itchan  {
				reflec(ii) = reflec(ii) / refsum
				sig(ii)    = sig(ii)    / refsum
			}
			inrm = 1
			xnrm = refsum
		}
	}
#
	if (irflfg == 1) {
#
		do ii = 1, itchan {
			data(itchan - ii + 1) = reflec(ii)
		}
		if (inrm == 1) ititl(20:23)='NRML'
		recnum = recnum + 1
		write (luncrt, 2010) recnum, ititl, itchan
		call wrtspr(recnum,lun,ier)
		ititl(33:40)='        '
	}
#
	if (isgflg.eq.1) {
		ititl(1:31)='errors to prev. spec., sig mean'
		ititl(32:40)='         '
		do ii = 1, itchan {
			data(itchan - ii + 1) = sig(ii)
		}
		recnum = recnum + 1
		write (luncrt, 2010) recnum, ititl, itchan
		call wrtspr(recnum,lun,ier)
		ititl(1:40)=itlsav(1:40)
	}
#
# completed now go back to beginning and wait for the next spectrum
#
	go to 50
	}
	go to 60
#
# done
#
5000    close (10,iostat=ier)
	stop
#
	end

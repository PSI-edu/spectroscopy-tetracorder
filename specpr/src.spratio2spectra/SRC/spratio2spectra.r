#HPUX	program spratio2spectra(ic1,ic2,ic3,ic4,ic5)
#IA64HPUX	program spratio2spectra
#	Ratfor
#************************spprint*************************************
#code words:
#ccc  name:
#ccc  version date:
#ccc  author(s):
#ccc  language:
#ccc
#ccc  short description:   Changes wavelength and resolution pointers in a specpr record/
#ccc
#ccc                           1              2                3                    4                 5         6    7     8
#ccc    spratio2spectra  specprfilename  recordnumber   dividorrecordnumber  title(put in quotes)  [force]  [-norm chan1 chan2]
#ccc
#ccc
#ccc  algorithm description:
#ccc  system requirements:
#ccc  subroutines called:
#ccc  argument list description:
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc
#  
#*********************************************************************

	implicit integer*4 (i-n)

	include "../../src.specpr/common/spmaxes"    # max parameters, must be first

	include "../../src.specpr/common/blank"
	include "../../src.specpr/common/label1"
	include "../../src.specpr/common/lbl4"
	include "../../src.specpr/common/lundefs"
	include "../../src.specpr/common/cmdarg"
	include "../../src.specpr/common/alphabet"

#HPUX	character*80 ic1,ic2,ic3,ic4,ic5
	character*1 ichil
	character*8 inm
	character*40 itlsav, newtitle, ititld
	character*80 filnam
	character*1536 dummy

	real*4    xbnrm   # band normalixation constant

	integer*4 fsize, arglen
	integer*4 idummy,filsiz
	integer*4 ier,testb, i, iforce, ifnamlnb

	integer*4 newwavptr, newresptr

	integer*4 recnum     # record number where to write output
	integer*4 sprecnum   # numerator record number
	integer*4 drecnum    # denominator record number

	integer*4  nchansd, nchans

	integer*4  bch1, bch2, bnormflag   # band normalization channels

	integer*2 chkbit, ibit
	equivalence (dummy,ititl)

	call spblockdata   # sets up the alphabet

	charg1 = ' '    # up to 6 command line arguments
	charg2 = ' '
	charg3 = ' '
	charg4 = ' '
	charg5 = ' '
	charg6 = ' '
	filnam = ' '

#HPUX	charg1 = ic1
#HPUX	charg2 = ic2
#HPUX	charg3 = ic3
#HPUX	charg4 = ic4
#HPUX	charg5 = ic5

	call getcmdargs

	#write (ttyout, *) 'debug: ncmdarg=', ncmdarg

        maxrec = SPMAXREC
        maxchn = SPMAXCHAN
        maxtxt = SPMAXTEXT

	iforce = 0    # if zero asks if you want to proceed
	mode = 0
	mode2 = 0
	iclin = 0
	ia=0
	ih=0
	im=0
	ib=0
	ic=1
	id=1
	itf =1
	irec=0
	lpline=0

	nchansd = 0
	nchans  = 0

	bch1 = 0
	bch2 = 0
	bnormflag = 0
	xbnrm = 1.0

#                              1              2                3                    4                 5         6    7     8
#       spratio2spectra  specprfilename  recordnumber   dividorrecordnumber  title(put in quotes)  [force]  [-norm chan1 chan2]
#             variable=      filnam        sprecnum          drecnum              newtitle          iforce          bch1  bch2

	if (ncmdarg.ge.1) {
		ifnamlnb = lnb(charg1)
		#write (ttyout, *) 'debug: lnb(charg1)=', ifnamlnb
		#charg1(80:80) = ' '   # make sure last character is not a null
		filnam = charg1(1:ifnamlnb)
	}
	if (ncmdarg < 4) {    # 4 is the minimum number of valid arguments

		write (ttyout,1)
1		format (1x, 'input file not fully specified after program name',
		/, 1x, 'proper use is ',/,
		'spratio2spectra  specprfilename  recordnumber   ',
		       'dividorrecordnumber  title(put in quotes)  [force]',
			' [-norm chan1 chan2]',/)

		go to 5000
	}

10	lun = 10
 	open (lun,file=filnam,iostat=idummy,status='old',
		access='direct',recl=1536,
		form='unformatted')
	if (idummy.ne.0) {
		write (ttyout,15)idummy
15		format (1x, 'cant open file, error',1x, i6,/)
		stop
	}

	if (ncmdarg.ge.5) {

		if ( charg5(1:lnb(charg5)) == 'force' || charg5(1:lnb(charg5)) == '-force' ) {

			#write (ttyout, *) 'debug:  charg4(1:lnb(charg4)=', charg4(1:lnb(charg4))
			iforce = 1   # write chenges without asking for confirmation
			#write (ttyout, *) 'debug: force = ', iforce
		}
	}
	if (ncmdarg.ge.8) {

		if ( charg6(1:lnb(charg6)) == 'norm' || charg6(1:lnb(charg6)) == '-norm' ) {

			read (charg7, *) bch1
			read (charg8, *) bch2
		}
		if ( bch1 < 1 | bch2 < 1 ) {

			write (ttyout,*) 'ERROR: band normalization channels too small'
			go to 5000
		}
		bnormflag = 1
	}

#
# get current length of file, test if consistent with specpr file
#     and set initial output record number to the end of the data
#     already in the file.
#
	filsiz = fsize(filnam)

	if (mod(filsiz,1536).ne.0) {
		write (ttyout,30) filnam, filsiz
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

	if (recnum <= 0 ) {
		write (ttyout,45) 
45		format (' no data in file, exiting',/)
		go to 5000
	}
	# dangeroue: if (recnum > maxrec) recnum = maxrec

#                              1              2                3                    4                 5
#       spratio2spectra  specprfilename  recordnumber   dividorrecordnumber  title(put in quotes)  [force]
#             variable=      filnam        sprecnum          drecnum              newtitle          iforce

	newtitle = ' '
	#write (ttyout,*) 'test point 1'

	# get the ther command line parameters

		read (charg2, *) sprecnum
		read (charg3, *) drecnum

		newtitle = charg4(1:40)    # new title

	#write (ttyout,*) 'debug rec=', sprecnum,'  wavptr=', newwavptr, '   resptr=', newresptr
        
################ read the divisor spectrum and check for errors ###################

	ikey = drecnum
	if (ikey > recnum) {
		write (ttyout, 77)
77		format ('ERROR: record number greater than file length',/,
			'   exit')
		go to 5000
	}

	itmp = ikey
	call redspr(ikey,lun,ier)
 	if (ier .ne. 0){
		write (ttyout,55) ier, ikey
55		format ('       Read  error ',i6,' at record ',i7, /)
		goto 5000
	}

	ibit = 1
	itxt = 0
	if (chkbit(icflag,ibit) == 1) {
		itxt = 1  # this is a text record
		write (ttyout,*) 'ERROR: ', drecnum, ' is a text record'
		go to 5000 # exit
	}

	ititld  = ititl
	nchansd = itchan   # number of channels

	do i = 1, nchansd {        # copy dividor to datab arrasy
		datab(i) = data(i)
	}
	############# got divisor

################ read the numerator spectrum and check for errors ###################

	# note: by reading the numerator second, all the header information remains

	ikey = sprecnum
	if (ikey > recnum) {
		write (ttyout, 77)
		go to 5000
	}

	itmp = ikey
	call redspr(ikey,lun,ier)
 	if (ier .ne. 0){
		write (ttyout,55) ier, ikey
		goto 5000
	}

	ibit = 1
	itxt = 0
	if (chkbit(icflag,ibit) == 1) {
		itxt = 1  # this is a text record
		write (ttyout,*) 'ERROR: ', sprecnum, ' is a text record'
		go to 5000 # exit
	}
	nchans = itchan   # number of channels
	############# got numerator

############ divide the spectra ######################

	if ( nchans != nchansd ) {

		write (ttyout,*) 'ERROR: channels not equal: ', nchans, nchansd
		go to 5000 # exit
	}
	if ( nchans < 1 | nchansd < 1 ) {

		write (ttyout,*) 'ERROR: channels less than 1: ', nchans, nchansd
		go to 5000 # exit
	}

	do j = 1, nchans {        # copy dividor to datab arrasy

		if(data(j) == -1.23e34 | datab(j) == -1.23e34 | abs(datab(j)) < 0.1e-25) {
			data(j)= -1.23e34
		} else {
			data(j) = data(j) /datab(j)
		}
	}
	if ( bnormflag == 1) {

		if ( bch1 > nchans | bch2 > nchans ) {

			write (ttyout, *) 'ERROR: normalization channel > ', nchans
			go to 5000
		}

		k = 0
		xbnrm = 0.0
		do j = bch1,  bch2 {

			if(data(j) != -1.23e34) {   # include
				xbnrm = xbnrm + data(j)
				k = k + 1
				#write (ttyout,*) 'DEBUG: data=', data(j),'  xbnorm=', xbnrm, '  k=',k
			}
		}
		if ( k > 0 ) {
			
			xbnrm = xbnrm / float(k)
		} else {
			write (ttyout, *) 'ERROR: no normalization points'
			go to 5000
		}

		do j = 1, nchans {                  # normalize
			if(data(j) != -1.23e34 ) {
				data(j) = data(j) / xbnrm
			}
		}
		iband(1) = bch1    # set in labael1 common block for output
		iband(2) = bch2
		xnrm     = xbnrm

		write (ttyout, 38) bch1,  bch2, xbnrm
38		format (1x, 'Normalizing over ',i6,' to ',i6, '    norm=',f15.6)
	}

	write (ttyout, 40) filnam(1:ifnamlnb), sprecnum, ititl, nchans
40	format (' spectrum:  ',a,' r', i8,' ', a, '   chans=',i6)

	write (ttyout, *) '  divided by'

	write (ttyout, 41) filnam(1:ifnamlnb), drecnum, ititld, nchansd
41	format ('            ',a,' r', i8,' ', a, '   chans=',i6)

	ititl  = newtitle
	recnum = recnum +1 # output record number ar end of file

	write (ttyout, 43) newtitle, recnum
43	format (//, 'output title= ', a,'  record=', i9, /)

############## make history  #################################

	# TBD
	write (ihist, 88) filnam(1:ifnamlnb), sprecnum, filnam(1:ifnamlnb), drecnum
88	format ('spratio2spectra ',a8,1x,i7,' / ',a8,1x,i7,9x)

############## write results ################################

	if (iforce == 0) {
		write (ttyout, *) ' Type  y  to write results, anything else to quit'
		call crtin
		i=1
		call wjfren (i, x, il)

#		write (ttyout, 1005) il, ihy
#1005		format ('debug il=',a,'  need ',a,' for yes')
		if ( il != ihy ) {

			write (ttyout, *) ' Aborting'
			go to 5000
		}
	}

	ikey = recnum
	itmp = ikey
	#write (ttyout,*) 'debug: itmp=',itmp,'  ikey=',ikey,'  recnum=', recnum
	ier  = 0
	#write (ttyout, *) 'writing specpr file ', charg1, recnum, 'wavepointer=', newwavptr, newresptr
	call wrtspr(itmp,lun,ier)
        if (ier .ne. 0){
                write (ttyout,56) ier, ikey
56		format ('       wrtspr error ',i6,' at record ',i7, /)
		goto 5000
	}
	
	write (ttyout, *) ' Done'
	close (10,iostat=ier)
	stop

	
5000	write (ttyout, 5001)
5001	format (' exit')
	close (10,iostat=ier)
	stop
 
	end 

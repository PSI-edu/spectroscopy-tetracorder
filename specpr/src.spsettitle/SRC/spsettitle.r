#HPUX	program spsettitle(ic1,ic2,ic3,ic4)
#IA64HPUX	program spsettitle
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
#ccc                      1              2            3                    4           5
#ccc    spsettitle  specprfilename  recordnumber  title(put in quotes)  [force]
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
#	hdr:header
#	dat:data
#	stat:complete spectrum is complete
#	stat:new  new spectrum has started
#  
#*********************************************************************

	implicit integer*4 (i-n)

	include "../../src.specpr/common/spmaxes"
	include "../../src.specpr/common/label1"
	include "../../src.specpr/common/lbl4"
	include "../../src.specpr/common/lundefs"
	include "../../src.specpr/common/cmdarg"
	include "../../src.specpr/common/alphabet"

#HPUX	character*80 ic1,ic2,ic3,ic4
	character*1 ichil
	character*8 inm
	character*40 itlsav, newtitle
	character*80 filnam
	character*1536 dummy
	integer*4 recnum, fsize, arglen
	integer*4 idummy,filsiz
	integer*4 ier,testb, i, iforce, ifnamlnb

	integer*4 sprecnum, newwavptr, newresptr

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

	if (ncmdarg.ge.1) {
		ifnamlnb = lnb(charg1)
		#write (ttyout, *) 'debug: lnb(charg1)=', ifnamlnb
		#charg1(80:80) = ' '   # make sure last character is not a null
		filnam = charg1(1:ifnamlnb)
	}
	if (ncmdarg < 3) {    # 3 is the minimum number of valid arguments

		write (ttyout,1)
1		format (1x, 'input file not fully specified after program name',
		/, 1x, 'proper use is spsettitle (specpr)filname recordnumbertochange new-title-in-quotes [force]',/)

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

	if (ncmdarg.ge.4) {

		if ( charg4(1:lnb(charg4)) == 'force' || charg4(1:lnb(charg4)) == '-force' ) {

			#write (ttyout, *) 'debug:  charg4(1:lnb(charg4)=', charg4(1:lnb(charg4))
			iforce = 1   # write chenges without asking for confirmation
			#write (ttyout, *) 'debug: force = ', iforce
		}
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
	if (recnum > maxrec) recnum = maxrec

	newtitle = ' '
	#write (ttyout,*) 'test point 1'
	if (ncmdarg.ge.2) {
		#sprecnum = something(charg2)   # how to convert string to integer
		#write (ttyout,*) 'charg2=', charg2
		read (charg2, *) sprecnum
	}
	if (ncmdarg.ge.3) {
		newtitle = charg3(1:40)    # new title
	}

	#write (ttyout,*) 'debug rec=', sprecnum,'  wavptr=', newwavptr, '   resptr=', newresptr
        
	ikey = sprecnum
	if (ikey > recnum) {
		write (ttyout, 77)
77		format ('ERROR: record number greater than file length',/,
			'   exit')
		go to 5000
	}

################ read the spectrum and check for errors ###################

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
	}


	if ( itxt == 0) {
		write (ttyout, 41) filnam(1:ifnamlnb), sprecnum, itchan
41		format (' Changing File: ',a,'  record:', i8,'   chans=',i6,' data record')
	} else {
		write (ttyout, 42) filnam(1:ifnamlnb), sprecnum
42		format (' Changing File: ',a,'  record:', i8,'   TEXT Record')
	}
	write (ttyout, 43) ititl
43	format ('                       OLD title = ' a)
	write (ttyout, 44) newtitle
44	format ('                       NEW title = ' a,/)

	if (iforce == 0) {
		write (ttyout, *) ' Type  y  to change, anything else to quit'
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

	ititl = newtitle   # set new title

	ikey = sprecnum
	itmp = ikey
	#write (ttyout,*) 'debug: itmp=',itmp,'  ikey=',ikey,'  sprecnum=',sprecnum
	ier  = 0
	#write (ttyout, *) 'writing specpr file ', charg1, sprecnum, 'wavepointer=', newwavptr, newresptr
	call wrtspr(itmp,lun,ier)
        if (ier .ne. 0){
                write (ttyout,56) ier, ikey
56		format ('       wrtspr error ',i6,' at record ',i7, /)
		goto 5000
	}
	
	write (ttyout, *) ' Change made'
	close (10,iostat=ier)
	stop

	
5000	write (ttyout, 5001)
5001	format (' exit')
	close (10,iostat=ier)
	stop
 
	end 

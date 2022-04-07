#HPUX	program spsearch(ic1,ic2,ic3)
#	Ratfor
#************************spprint*************************************
#code words:
#ccc  name:
#ccc  version date:
#ccc  author(s):
#ccc  language:
#ccc
#ccc  short description:
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
#*********************************************************************

	implicit integer*4 (i-n)

	include ../specpr/src.specpr/common/label1
	include ../specpr/src.specpr/common/lbl4
	include ../specpr/src.specpr/common/lundefs
	include ../specpr/src.specpr/common/cmdarg
	include ../specpr/src.specpr/common/blank
	include ../specpr/src.specpr/common/alphabet

#HPUX	character*80 ic1,ic2,ic3
	character*1 ichil
	character*8 inm
	character*40 itlsav 
	character*80 filnam,sstring
	character*1536 dummy
	integer*4 recnum, fsize, arglen,ic,ichoice,lensstr
	integer*4 idummy,filsiz,nextrec,descrec,nowrec,icount,igcount
	integer*4 ier,testb,mm,dd,yy,a
	integer*2 chkbit,ibit
	equivalence (dummy,ititl)
	character xptime*11,xpdate*10
	include template.h

	include tempfill.h	# this must be last -- not declarations

#HPUX	charg1 = ic1
#HPUX	charg2 = ic2
#HPUX	charg3 = ic3

	call getcmdargs

	maxrec=999999
	maxchn=4852
	maxtxt=19860
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

	write(6,*) charg1

	if (ncmdarg.ge.1) filnam = charg1
	if (ncmdarg.ge.1) go to 10
#
	write (ttyout,1)
1      format (1x, 'input file not fully specified after program name',
		/, 1x, 'proper use is spsearch (specpr)filname',/)
#
	go to 5000
#
#	redlun is defined as unit 10 in lundefs
#
10	open (redlun,file=filnam,iostat=idummy,status='old',
		access='direct',recl=1536,
		form='unformated')
       if (idummy.ne.0) {
		write (ttyout,15)idummy
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
# put basename of filename into mhistb
#
	mhistb = filnam
	mhista = filnam
	do j = len(mhista),1,-1 {
		if (mhista(j:j)=='/') {
			mhistb = mhista(j+1:)
			break
		}
	}
#
# open cmd file for crtin
#
	open (16, file='.cmd', access='direct',recl=80,
		iostat=ier,form='unformatted')
#
	if (recnum <= 0 ) {
		write (ttyout,45) 
45		format (' no data in file, exiting',/)
		go to 5000
	}
	if (recnum > maxrec) recnum = maxrec

	call eralph
	write(ttyout,41) filnam
41	format('SPSEARCH - Search spectral library'/,
	'reading file: ',a)

	ikey = 0
	icount = 0
	do i =1,maxrec {
		ikey = ikey+1
		if (ikey > recnum) goto 3999
		call redspr(ikey,redlun,ier)
 		if (ier .ne. 0){
			write (ttyout,55)
55			format (' read error, exiting',/)
			goto 5000
		}
		ibit=1
		itmp = chkbit(icflag,ibit)
		if (itmp != 1) {
			if (index(ititl,'ABS REF') > 0) {
				icount = icount +1
				call addgtbl(icflag,icount)
			} else if (ititl(1:6) == 'errors') {
				call adderr(icflag,icount)
			} else if (index(ititl,'FEATANL') > 0) {
				call addfeat(icflag,icount)
			}
		}
#
# for addgtbl():
# icflag  is passed here as the starting address of label1
# icount  is used as a consecutive array index for table[]
# 	  NOTE: only data records are given a table entry.  
# 		"ABS REF" && itmp==0 
# NsG
	}
	
3999	call mkgtbl(itmp)		# puts gtable into ntable
	igcount = icount

#
# ichoice = array index of search field :: template(ichoice)
#
	write(ttyout,42)
42	format(/'Current search parameters:'/,
	'Data Description:   ABS REF'/)
4000	call doit(xa,ic,icount,filnam,igcount,recnum)
	if (icount == -1) stop

#
#  If we were doing things correcly, out table would be in node
#  format now, and we would call mktbl to put it into array format.
#
# But dont bother, its not that much memory.
#
#	call mktbl()
	if (ic == ihx) goto 5000
	goto 4000
	stop
5000	write (ttyout, 5001)
5001	format (' done')
	close (10,iostat=ier)
	stop
 
	end 

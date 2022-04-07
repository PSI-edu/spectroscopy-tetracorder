#HPUX	program spprint(ic1,ic2,ic3)
#IA64HPUX	program spprint
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

#HPUX	character*80 ic1,ic2,ic3
	character*1 ichil
	character*8 inm
	character*40 itlsav 
	character*80 filnam
	character*1536 dummy
	integer*4 recnum, fsize, arglen
	integer*4 idummy,filsiz
	integer*4 ier,testb
	integer*2 chkbit	
	equivalence (dummy,ititl)

#HPUX	charg1 = ic1
#HPUX	charg2 = ic2
#HPUX	charg3 = ic3

	call getcmdargs

# was pre 10/25/2015:
#	maxrec=999999
#	maxchn=4852
#	maxtxt=19860

        maxrec = SPMAXREC
        maxchn = SPMAXCHAN
        maxtxt = SPMAXTEXT

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
	
	if (ncmdarg.ge.1) filnam = charg1
	if (ncmdarg.ge.1) go to 10
#
	write (ttyout,1)
1      format (1x, 'input file not fully specified after program name',
		/, 1x, 'proper use is sprint (specpr)filname',/)
#
	go to 5000
#
10	lun = 10
 	open (lun,file=filnam,iostat=idummy,status='old',
		access='direct',recl=1536,
		form='unformatted')
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
# open cmd file for crtin
#
	open (16, file='/dev/null', access='direct',recl=80,
		iostat=ier,form='unformatted')
#
	if (recnum <= 0 ) {
		write (ttyout,45) 
45		format (' no data in file, exiting',/)
		go to 5000
	}
	if (recnum > maxrec) recnum = maxrec

	ikey = 0
	do i =1,maxrec {
		ikey = ikey+1
		if (ikey > recnum) go to 5000
		itmp = ikey
		call redspr(ikey,lun,ier)
 		if (ier .ne. 0){
			write (ttyout,55) ier, ikey
55			format ('       readspr error ',i6,' at record ',i7, /)
			goto 4999
		}
		itw = 0
		inm = ' '
		itmp2 = itmp
		mode=2
		call cprint2 (mode, mode2,ia,ih,ib,itmp,iclin,itmp2)
4999		continue
	}
5000	write (ttyout, 5001)
5001	format (' done')
	close (10,iostat=ier)
	stop
 
	end 

#HPUX	program asciitosp (ach1, ach2, ach3)
#IA64HPUX	program asciitosp
#
#     Ratfor
#************************* asciitosp   *******************************
# 
# This program converts ascii data and textfiles to specpr format.
# The data must have been previously converted to a textfile by
# the sptoascii program operating on a existent specpr file.
# When modifying text, be sure to leave the ZZZZZZZZZZZZZZZZZZZZ in
# place, because this marks the end of a text record.
#
#********************************************************************
#
	implicit integer*4 (i-n)

	include "../../src.specpr/common/spmaxes"
	include  "../../src.specpr/common/lbl4"
	include  "../../src.specpr/common/label1"
	include  "../../src.specpr/common/cmdarg"
#
	character*1 ichil
	character*40 itlsav
	character*80 filnam
	character*1536 dummy
 	equivalence (dummy,ititl)

#HPUX	character*80 ach1, ach2, ach3

	integer*4 fsize
	integer*4 recnum
	integer*4 idummy, filsiz
	integer*4 N,arglen
	integer*2 chkbit,ibit,bitnum
	real*4 sec
	integer*4 itest(32)
#
	luncrt = 6
	lunin = 5
	lun =10

#HPUX	charg1 = ach1
#HPUX	charg2 = ach2
#HPUX	charg3 = ach3

# open cmd file for crtin (found in wrtspr subroutine)
#
	open(16, file='/dev/null', access='direct',recl=80,
			iostat=ier,form='unformatted')

#      get number of arguments from command line after program.
#
	call getcmdargs

	if (ncmdarg.ge.1) filnam = charg1
	if (ncmdarg.ge.1) go to 10
#
	write (luncrt,1)
1      format (1x, 'Input file not fully specified after program name',
		/, 1x, 'Proper use is:  asciitosp specpr_filname',
                        ' < sourcefilename',/)
#
	go to 5000

#
#
10     open (lun,file=filnam,iostat=idummy,status='old',
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
# record in which to write the data is the next one after the end.
#
# set initial values of bit flag (icflag) to zero
#
50	do i = 0,31 {
		bitnum = i
		call clrbit(icflag,bitnum)
	}
 
	isgn = 1
	istb = 0
	isra = 0
	isdec = 0
	irmas = 0
	revs = 1
	iband(1) = 1
	iband(2) = 1
	irwav = 0
	irespt = 0
	itpntr = 0
	nruns = 1
	siangl = 0
	seangl = 0
	sphase = 0
	ieros = 0
	iwtrns = 1
	xnrm = 1.0
	tempd = 293.0
	nchant = 0
	itchan=0
	nchans = 0
	minchn = 0
	maxchn = 4852
	maxrec = 999999
	maxtxt = 19680
#
#	set flag
#
	idatfl = 1   # set to use nchans unless itchan defines itself

#
# set initial values for channel to deleted point
#
	do j = 1, maxchn {
		data(j)=-1.23e34
	}
#
 
#
# read header info from standard input and put into specpr labeled
#      common
#
	repeat {
60	read (lunin,100,end=5000)iopcon
100    format (a)
#
	if (iopcon(1:13)=='stat:complete') go to 2000
#
#
	if (iopcon(1:6)=='ititl:' | iopcon(1:6)=='title:') {
		ititl(1:40)=iopcon(8:47)
		do j= 1,40 {
			if (ititl(j:j)==char(0)) ititl(j:j)=' '
		}
#
	next
	}
#
#  set the bit flag for icflag
#
	if (iopcon(1:10)== 'bit flags:'){
		i =11 
		j = 1 
		while(iopcon(i:i) == ' '){
			i = i + 1
		}
		while(iopcon(i:i) == '0' | iopcon(i:i)=='1' & j<=31){
			call decod1(iopcon(i:i),m)
			ibit = j - 1
			if(m == 1) call setbit(icflag,ibit)

			i = i+1
			j = j + 1
		}
#
# This section used in debugging to check on icflag values
#
#		do i = 0,31{
#			ibit=i
#			itest(i+1)=chkbit(icflag,bitnum)
#		}
#		write(luncrt,122)(itest(i),i= 1,32)
#122		format('bit: ',1x,32I1)

		next
	}
		
#
# text translation routine
#
	if (iopcon(1:5)== 'text:'){
		call readtx(ier)
		if(ier != 0){
			write(luncrt,56)
56			format('ERROR in readtx')
		}
##
	go to 2000
	}

	if (iopcon(1:7)=='usernm:') {
		usernm(1:7)=iopcon(9:15)
		do j= 1,8 {
			if (usernm(j:j)==char(0)) usernm(j:j)=' '
		}
	next
	}
##
	if (iopcon(1:6)=='datea:' | iopcon(1:7)== 'jdatea:') {
		i = 8
		if(iopcon(1:6)== 'datea:') i=7
		call wjfren(i,x,il)
		if(il != 0 & il != ihchar('/')){
			write(luncrt,211)il
211			format(1x,'error getting datea',a1,
				'encountered')
		}
		imon = x
		call wjfren(i,x,il)
		if(il != 0 & il != ihchar('/')){
			write(luncrt,211)il
		}
		iday = x
		call wjfren(i,x,il)
		if(il != 0 ){
			write(luncrt,211)il
		}
		iy = x
		call tojuld(iy,imon,iday,jday)
		jdatea = jday

	next
	}
##
	if (iopcon(1:6)=='dateb:' | iopcon(1:7)== 'jdateb:') {
		i = 8
		if(iopcon(1:6)== 'dateb:') i=7
		call wjfren(i,x,il)
		if(il != 0 & il != ihchar('/')){
			write(luncrt,212)il
212			format(1x,'error getting dateb',a1,
				'encountered')
		}
		imon = x
		call wjfren(i,x,il)
		if(il != 0 & il != ihchar('/')){
			write(luncrt,212)il
		}
		iday = x
		call wjfren(i,x,il)
		if(il != 0 ){
			write(luncrt,212)il
		}
		iy = x
		call tojuld(iy,imon,iday,jday)
		jdateb = jday

	next
	}
#
	if (iopcon(1:4)=='cta:' | iopcon(1:6)== 'iscta:') {
		i = 7
		if(iopcon(1:4)== 'cta:') i=5
		call wjfren(i,x,il)
		if(il != 0 & il != ihchar(':')){
			write(luncrt,213)il
213			format(1x,'error getting cta',a1,
				'encountered')
		}
		id = x
		call wjfren(i,x,il)
		if(il != 0 & il != ihchar(':')){
			write(luncrt,213)il
		}
		im = x
		call wjfren(i,x,il)
		if(il != 0 ){
			write(luncrt,213)il
		}
		sec = x

		call frdms(itime,24000,id,im,sec,isgn)
		iscta = itime

	next
	}
#
#
	if (iopcon(1:4)=='ctb:' | iopcon(1:6)== 'isctb:') {
		i = 7
		if(iopcon(1:4)== 'ctb:') i=5
		call wjfren(i,x,il)
		if(il != 0 & il != ihchar(':')){
			write(luncrt,214)il
214			format(1x,'error getting ctb',a1,
				'encountered')
		}
		id = x
		call wjfren(i,x,il)
		if(il != 0 & il != ihchar(':')){
			write(luncrt,214)il
		}
		im = x
		call wjfren(i,x,il)
		if(il != 0 ){
			write(luncrt,214)il
		}
		sec = x

		call frdms(itime,24000,id,im,sec,isgn)
		isctb = itime
	next
	}
#
# stb statement here to prevent error in translating old format

	if (iopcon(1:4)=='sta:') next


	if (iopcon(1:4)=='stb:' | iopcon(1:5)=='istb:') {
		i=6
		if(iopcon(1:4)== 'sta:') i=5
		call wjfren(i,x,il)
		if(il != 0 & il != ihchar(':')){
			write(luncrt,220)il
220			format(1x,'error getting istb',a1,
				'encountered')
		}
		id = x
		call wjfren(i,x,il)
		if(il != 0 & il != ihchar(':')){
			write(luncrt,220)il
		}
		im = x
		call wjfren(i,x,il)
		if(il != 0 ){
			write(luncrt,220)il
		}
		sec = x

		call frdms(itime,24000,id,im,sec,isgn)
		istb = itime
	next
	}
#
#
	if (iopcon(1:5)== 'isra:') {
		i= 6
		call wjfren(i,x,il)
		if (il.ne.0) {
			ichil = char(il)
			write (luncrt,230) char(il)
230			format (1x, 'error getting isra: ', 1a,
				' encountered')
		}
		isra=x
	next
	}
#

	if (iopcon(1:4)=='ira:') {
		i=5
		isgn =1
		call wjfren(i,x,il)
		if(il != 0 & il != ihchar(':')){
			write(luncrt,231)il
231			format(1x,'error getting ira',a1,
				'encountered')
		}
		id = x
		call wjfren(i,x,il)
		if(il != 0 & il != ihchar(':')){
			write(luncrt,231)il
		}
		im = x
		call wjfren(i,x,il)
		if(il != 0 ){
			write(luncrt,231)il
		}
		sec = x

		call frdms(itime,1000,id,im,sec,isgn)
		isra = itime
	next
	}
#
	if (iopcon(1:6) == 'isdec:') {
		i=7
		call wjfren(i,x,il)
		if (il.ne.0) {
			ichil = char(il)
			write (luncrt,240) char(il)
240			format (1x, 'error getting isdec: ', 1a,
				' encountered')
		}
		isdec=x
	next
	}
#

	if (iopcon(1:5)=='idec:') {
		isgn=1
		do i=6,80 {          # search for first char.
			if (iopcon(i:i) != ' ') break
		}
		if (iopcon(i:i) == '-') {   # check if 1st char is -
			isgn = -1
			i=i+1
		}
		call wjfren(i,x,il)
		if(il != 0 & il != ihchar(':')){
			write(luncrt,241)il
241			format(1x,'error getting idec',a1,
				'encountered')
		}
		id = x
		call wjfren(i,x,il)
		if(il != 0 & il != ihchar(':')){
			write(luncrt,241)il
		}
		im = x
		call wjfren(i,x,il)
		if(il != 0 ){
			write(luncrt,241)il
		}
		sec = x

		call frdms(itime,1000,id,im,sec,isgn)
		isdec= itime
	next
	}
#
#
	if (iopcon(1:6)=='ichan:' | iopcon(1:7) == 'itchan:') {
		i=8
		if (iopcon(1:6)=='ichan:') i=7 
		call wjfren(i,x,il)
		if (il.ne.0) {
			ichil = char(il)
			write (luncrt,250) char(il)
250			format (1x, 'error getting itchan: ', 1a,
				' encountered')
		}
		itchan =x
		idatfl = 0
	next
	}
#
#
	if (iopcon(1:6)=='irmas:') {
		i= 7
		call wjfren(i,x,il)
		if (il.ne.0) {
			ichil = char(il)
			write (luncrt,255) ichil
255			format (1x, 'error getting irmas: ', 1a,
				' encountered')
		}
		irmas = x
	next
	}
###
#
	if (iopcon(1:5)=='revs:') {
		i=6
		call wjfren(i,x,il)
		if (il.ne.0) {
			ichil = char(il)
			write (luncrt,260) char(il)
260			format (1x, 'error getting revs: ', 1a,
				' encountered')
		}
		revs=x
	next
	}
#
#
	if (iopcon(1:7)=='iband1:') {
		i=8
		call wjfren(i,x,il)
		if (il.ne.0) {
			ichil = char(il)
			write (luncrt,270) char(il)
270			format (1x, 'error getting iband1: ', 1a,
				' encountered')
		}
		iband(1) =x
	next
	}
#
#
	if (iopcon(1:7)=='iband2:') {
		i=8
		call wjfren(i,x,il)
		if (il.ne.0) {
			ichil = char(il)
			write (luncrt,280) char(il)
280			format (1x, 'error getting iband2: ', 1a,
				' encountered')
		}
		iband(2) =x
	next
	}
#
#
	if (iopcon(1:6)=='irwav:') {
		i=7
		call wjfren(i,x,il)
		if (il.ne.0) {
			ichil = char(il)
			write (luncrt,290) char(il)
290			format (1x, 'error getting irwav: ', 1a,
				' encountered')
		}
		irwav=x
	next
	}
#
#
	if (iopcon(1:7)=='irespt:') {
		i=8
		call wjfren(i,x,il)
		if (il.ne.0) {
			ichil = char(il)
			write (luncrt,300) char(il)
300			format (1x, 'error getting irespt: ', 1a,
				' encountered')
		}
		irespt=x
	next
	}
#
#
	if (iopcon(1:7)=='irecno:') {
		i=8
		call wjfren(i,x,il)
		if (il.ne.0) {
			ichil = char(il)
			write (luncrt,310) char(il)
310			format (1x, 'error getting irecno: ', 1a,
				' encountered')
		}
		irecno=x
	next
	}
#
#
	if (iopcon(1:7)=='itpntr:') {
		i= 8
		call wjfren(i,x,il)
		if (il.ne.0) {
			ichil = char(il)
			write (luncrt,320) char(il)
320			format (1x, 'error getting itpntr: ', 1a,
				' encountered')
		}
		itpntr =x
	next
	}
#
	if (iopcon(1:6)=='ihist:') {
		ihist(1:60)=iopcon(7:66)
		do j = 1, 60 {
			if (ihist(j:j)==char(0)) ihist(j:j) =' '
		}
	next
	}
#
	if (iopcon(1:3)=='m1:') {
		mhist(1:74) = iopcon(4:77)
		do j = 1, 74 {
			if (mhist(j:j)==char(0)) mhist(j:j) = ' '
		}
	next
	}
#
	if (iopcon(1:3)=='m2:') {
		mhist(75:148) = iopcon(4:77)
		do j = 75, 148 {
			if (mhist(j:j)==char(0)) mhist(j:j) = ' '
		}
	next
	}
#
	if (iopcon(1:3)=='m3:') {
		mhist(149:222) = iopcon(4:77)
		do j = 149, 222 {
			if (mhist(j:j)==char(0)) mhist(j:j) = ' '
		}
	next
	}
#
	if (iopcon(1:3)=='m4:') {
		mhist(223:296) = iopcon(4:77)
		do j = 223, 296 {
			if (mhist(j:j)==char(0)) mhist(j:j) = ' '
		}
	next
	}
#
#
	if (iopcon(1:6)=='nruns:') { 
		i = 7
		call wjfren(i,x,il)
		if (il != 0 ) {
			write (luncrt,330)il 
330			format (1x, 'error getting nruns: ', a1,
				' encountered')
		}
		nruns = x
	next
	}
#
##########################################
########################
# These next three need decod1 and loop to work
#
	if (iopcon(1:7)=='siangl:') { 
		N = 0
		i = 8
		while(iopcon(i:i) == ' '){
			i = i + 1
		}
		while(iopcon(i:i) >= '0' & iopcon(i:i)<='9' & i<=30){
			call decod1(iopcon(i:i),m)
			N = N*10 + m
			i = i+1
		}
		siangl =N 
	next
	}
#
#
#
	if (iopcon(1:7)=='seangl:') { 
		N = 0
		i = 8
		while(iopcon(i:i) == ' '){
			i = i + 1
		}
		while(iopcon(i:i) >= '0' & iopcon(i:i)<='9' & i<=30){
			call decod1(iopcon(i:i),m)
			N = N*10 + m
			i = i+1
		}
		seangl = N
	next
	}
#
#
	if (iopcon(1:7)=='sphase:') { 
		N = 0
		i = 8
		while(iopcon(i:i) == ' '){
			i = i + 1
		}
		while(iopcon(i:i) >= '0' & iopcon(i:i)<='9' & i<=30){
			call decod1(iopcon(i:i),m)
			N = N*10 + m
			i = i+1
		}
		sphase = N
	next
	}

#
#
#
#
	if (iopcon(1:7)=='iwtrns:') { 
		i = 8
		call wjfren(i,x,il)
		if (il != 0 ) {
			write (luncrt,370)il 
370			format (1x, 'error getting iwtrns: ', a1,
				' encountered')
		}
		iwtrns = x
	next
	}

#
#
	if (iopcon(1:7)=='itimch:') { 
		i = 8
		call wjfren(i,x,il)
		if (il != 0 ) {
			write (luncrt,380)il 
380			format (1x, 'error getting itimch: ', a1,
				' encountered')
		}
		itimch = x
	next
	}



#
#
	if (iopcon(1:5)=='xnrm:') {
		i= 6
		call wjfren(i,x,il)
		xsup =0
		if (il == ihchar('E') | il == ihchar('e')) {
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
		if (il.ne.0) {
			ichil = char(il)
			write (luncrt,390) ichil
390			format (1x, 'error getting xnrm: ', a,
				' encountered')
		}
		xnrm = x*10.0**xsup
	next
	}
###
#
	if (iopcon(1:7)=='scatim:') {
		i= 8
		call wjfren(i,x,il)
		if (il.ne.0) {
			ichil = char(il)
			write (luncrt,400) ichil
400			format (1x, 'error getting scatim: ', 1a,
				' encountered')
		}
		scatim = x
	next
	}
###
#
	if (iopcon(1:7)=='timint:') {
		i=8
		call wjfren(i,x,il)
		if (il.ne.0) {
			ichil = char(il)
			write (luncrt,410) ichil
410			format (1x, 'error getting timint: ', 1a,
				' encountered')
		}
		timint=x
	next
	}
###
#
	if (iopcon(1:6)=='tempd:') {
		i= 7
		call wjfren(i,x,il)
		if (il.ne.0) {
			ichil = char(il)
			write (luncrt,420) ichil
420			format (1x, 'error getting tempd: ', 1a,
				' encountered')
		}
		tempd = x
	next
	}
############
######
##
#
#--- now read data --------------------------
#
#
		i= 1
		call wjfren (i,x,il)
		if (il.ne.0) {
			write (luncrt,1010) il, iopcon
1010			format (1x, 'strange character ', 1a, 
				' encountered in the line:',
				/, 80a, /)
		}
#
# get channel
#
		if ((x.lt.1.0).or.(x.gt.maxchn)) {
			write (luncrt,1020) x, iopcon
1020			format (1x, 'channel out of range ', f9.2, ' in line:',
				/, 80a, /)
			go to 60
		}

#
		ichan  = x         
		nchans = nchans +1
#
#
# get  data
#
1040		call wjfren (i,x,il)
		xsup=0.0
		if (i.ge.80) go to 60
		if (il.ne.0) {
			if (il != ihchar('E')& il != ihchar('e')) {
				write (luncrt,1010) il, iopcon
			}
			if (il == ihchar('E') | il == ihchar('e')) {
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
#
		data(ichan)=x*10.0**xsup
		}
#
	}     #bracket for repeat at beginning of program

#
#  now write the data to the specpr data file
#
#  set number of channels (should have been given)
#  but if not received
#  then must use actual count of data received (nchans).
#
2000	if(idatfl == 1) itchan = nchans

	recnum = recnum + 1
	call wrtspr(recnum,lun,ier)

	write (6,2005) recnum, ititl, itchan
2005	format (1x,i6, 3x, a, 5x, i8, ' channels')
#
# completed now go back to beginning and wait for the next spectrum
#
	go to 50
#
# done
#
5000    close (10)
	stop
#
	end
#

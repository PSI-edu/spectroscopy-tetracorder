	subroutine f8(ic)
	implicit integer*4 (i-n)

#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc         This subroutine transposes the file-channel matrix
#ccc         by creating nchan file, each containing one channel.
#ccc  algorithm description:none
#ccc  system requirements:  none
#ccc  subroutines called:
#ccc          rstart,hreset,crtin,wjfren,filid,devlun,redfil,
#ccc          mthwrt,filid,dread,erored,wrifil
#ccc  argument list description:
#ccc     arguments: none
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc

	include "../common/spmaxes"   # max parameters, must be first

	include "../common/blank"
	include "../common/lbl7"
	include "../common/lbl6"
	include "../common/lbl4"
	include "../common/label1"
	include "../common/lbl3"
	include "../common/label3"
	include "../common/labelf"
	include "../common/lblg"
	include "../common/info"
	include "../common/lblvol"
	include "../common/lblprt"
	include "../common/hptrm"
	include "../common/lundefs"
	include "../common/alphabet"
	include "../common/dscrch"

#RED
	integer*4 manhst         # function manhst

	integer*4 ier,ndlt

	integer*4 ids(SPMAXCHAN),ifls(SPMAXCHAN),idltpt(SPMAXCHAN)
	character*2 inm

        equivalence (ids(1),datsc1(1)),(ifls(1),datsc2(1))
        equivalence (idltpt(1),datsc3(1))

	inm = 'f8'
	call hreset(1)
	call whedr2
	key = 0
	write(ttyout,1)
	write (ttyout,10)

	call crtin
	i=1
	call wjfren(i,x,il)
	if (il == ihx || il == ihe) go to 1000
	ndlt = 0
	if (il == ihd) {
		call dltpts(i,ndlt,idltpt,nchans,il)
		if (il == ihx || il == ihe) go to 1000
	}

260	write(ttyout,2)
	repeat {
		write(ttyout,3)
		call crtin
		i = 1
		call wjfren(i,x,il)
		if (il==ihe || il==ihx) go to 1000
		if (il==ihb) break
		if (il==ihr) {
			if (key==0) write(ttyout,11)
			else write(ttyout,12)key
			if (key >0) key=key-1
			next
 		}
			
		i = 1
		call filid(i,ifiln,ifilid)
		if (iops(1:1)=='x') next
		lun = 0
		call devlun (4,ifilid,lun)
		if (lun==0) {
			write(ttyout,4)
			next
		}
		itmp = ifiln
		call redfil(itmp,lun,ier)
		if (ier!=0) {
			write(ttyout,5)
			next
		}
		icnt = 1024
		key = key + 1
		write (addlun,rec=key+1,iostat=ier)data
		call erored(icnt,ier,1024)
		write(ttyout,15) key,ititl,ifilid,ifiln
		ids(key+1) = ifilid
		ifls(key+1) = ifiln
		if (key>=maxchn) break
	}
	if (key==0) go to 260
	do i = 1,maxchn
		data (i) = 0
	write(ttyout,20)
	iprodp=0
	call mthwrt (igo,lun,ifiln,iprodp)
	il=0
	if (igo==110) il=ihe
	if (igo==1101) il=ihx
	if (il==ihe || il==ihx) go to 1000
	i = 1
	call filid (i,ix,ifilid)
	ids(1) = ifilid
	ifls(1) = ifiln
	write(ttyout,25)
	call crtin
	write(ttyout,50)
	kfile = ifiln - 1
	iflag = 0
	itmp = nchans
	nchans = key
	itchan = key
	do i = 1,itmp {
		if (ndlt > 0) {  # check for deleted points
			do ii = 1, ndlt {
				if (i == idltpt(ii)) go to 600
			}
		}
		do j = 1,key {
			icnt = 1024
			read (addlun,rec=j+1,iostat=ier)dataa
			call erored (icnt,ier,1024)
			data(j) = dataa(i)
		}
		kfile = kfile + 1
		write(ihist,35) key,i
		write(ititl,30) iopcon(1:33),i
		idump = manhst(inm,2,ids,ifls,(key+1),mhist,iflag,cx)
		filno = kfile
		call wrifil (kfile,lun,ier)
		if (ier!=2) {
			write(ttyout,45) i,kfile
			if (ier==4) go to 1000
		}
		if (lun==8) ifilex = ifilex + 1
		write(ttyout,55) i,ifilid,kfile
600		continue
	}
	ic = ihe
	return
1000    ic = ihe
	if (il==ihx) ic = ihx
	return
1       format (' Special Function f8:'/,
		' This routine TRANSPOSES the file-channel MATRIX',/,
		' by creating nchan files, each containing one',
		    ' channel''s data')
10	format (/,' Type  d  to DELETE points, then Type in the',
		' channels to be deleted', /, 
		' or Press Return to CONTINUE')
2       format (/' Type in the file id''s, one per line, after',
		' the CONTINUE message.'/,
		' Type  e  to EXIT,  b  to Begin TRANSPOSE,  r  to ',
		'REENTER last entry.'/)
3       format('CONTINUE',/)
4       format (' INCORRECT ENTRY',/)
5       format (' NO DATA READ',/)
11      format (' ** NO FILE to Reenter **, CONTINUE',/)
12      format (' RETURN to ',i4,', continue.'/)
15      format (10x,i4,':',a,5x,'file = ',a,i4)
20      format (' Type in the file at which WRITE is to START:',/)
25      format (' Type in desired TITLE',11(1h-),'I',/)
30      format (a33,' ch',i4)
35      format ('f8: transpose of',i4,' files: ch',i4,26(' '))
40      format (a)
45      format (/,1x,'BAD WRITE - channel ',i4,' file ',i4,/)
50      format (/' ***** WORKING *****'/)
%55      format ('\r','Writing channel',i4,' into file ',a,i4,$)
	end

	subroutine f3 (ic, ipcn, cx, ispfcn,constx,iopsav)
	implicit integer*4 (i-n)

#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language : Ratfor
#ccc
#ccc  short description:
#ccc          This routine setsup sequential operation processing.
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc          hreset,whedr2,crtin,wjfren,devlon,devsta
#ccc  argument list description:
#ccc     arguments: ic,ipcn,cx,ispfcn,constx,iopsav
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
#     this subroutine sets up sequential operation processing.
#

	include "../common/spmaxes"   # max parameters, must be first

	include "../common/lbl4"
	include "../common/lundefs"
	include "../common/alphabet"

#RED
	integer*4 ihchar      # function ihchar

	character*80 ipcn
	character*10 iopsav


	data ih  /1h /
	data ihadd, ihsub, ihmul, ihdiv /1h+, 1h-, 1h*, 1h//

100	call hreset(1)
	call whedr2

	write(ttyout,1)

	iops = ' '
	iopsav = ' '

	x=0
4	call crtin
	i=1
	il=0
	itemp = i
	do  i= itemp,80 {
		if (iopcon(i:i)!=' ') {
			il=ihchar(iopcon(i:i))
			break
		}
	}
	if (il == ihe || il == ihx) go to 1000
	if (il == ihadd) {
		write(ttyout,5)
		go to 4
	}

#
#     ipc(1)=function
#     ipc(2)=function
#     ipc(3)=first file id
#     ipc(4)=first file #
#     ipc(5)=last file # (of first id)
#     ipc(6)=first file increment
#     ipc(7)=second file id
#     ipc(8)=second beginning file
#     ipc(9)=second ending file
#     ipc(10)=second file increment
#
#
	if (il!=ihsub && il!=ihmul && il!=ihdiv && il!=ihf) {
8	continue
		do j= 1,10
			ipc(j) = 0

		write(ttyout,6)
		go to 4
	}
	ipc(1)=il
	ipc(2)=ih
	i=i+1
	if (il==ihf) {
		call wjfren (i,x,ilf)
		if (x==3 || x==4 || x<=0) go to 8
		ipc(2)= x
	}

14	write(ttyout,15)
	call crtin

#get first file id & check if ok.
	i=1
	call wjfren (i,x,il)
	if (il==ihx || il==ihe) go to 1000
	if ((il!=ihv && il!=ihw && il!=ihd && il!=ihu && il!=ihy)
		|| i>80) go to 14
	ipc(3)=il
	call devlun (4,il,ifid)
	call devsta (ifid,ista,0,iprt)
	if (ista<=0) go to 14

#get first file starting record number & check if ok
	call wjfren (i,x,il)
	if (i>80 || il!=0) go to 14
	ipc(4) = x

#get first file ending record number & check
	call wjfren (i,x,il)
	if (i>80 || il!=0 || x<ipc(4)) go to 14
	ipc(5)=x

#get first file record increment & check
	call wjfren (i,x,il)
	if (il!=0 || x<0) x = 0
	ipc(6)=x
	ipc(4)= ipc(4)-ipc(6)
	if (ipc(1) == ihf ) {
		do j=7,10
			ipc(j) = 0
		go to 34
	}

24	write(ttyout,25)
	call crtin
	i=1

#get second file id
	call wjfren(i,x,il)
	if (il==ihx || il==ihe) go to 1000
	if (il!=ihv && il!=ihw && il!=ihd  &&
		il!=ihc && il!=ihs && il!=ihu && il!=ihy) go to 24
	ipc(7)=il
	if (il==ihc) go to 27
	call devlun (0,il,ifid)
	call devsta (ifid,ista,0,iprt)
	if (ista<=0) go to 24

#get second file starting record number (or constant value)
27	il1=il
	call wjfren(i,x,il)
	if (il!=0 || i>80) go to 24
	ipc(8) = x
	if (il1==ihc) constx=x
	cx= constx
	if (il1==ihc) {
		ipc( 9)=0
		ipc(10)=0
	} else {

#get second file ending record number
		call wjfren(i,x,il)
		if (il!=0) go to 24
		if (x<0) x=0
		ipc(9)=x
		if (ipc(9) < ipc(8)) go to 24

#get second file increment
		call wjfren(i,x,il)
		if (il!=0 || x<0) x=0
		ipc(10)=x
		ipc(8)= ipc(8)- ipc(10)
	}

34	write(ttyout,35)
	call crtin
	i=1
	do j=1,10 {
		call wjfren (i,x,il)
		if (il==ihx) go to 2000
		if (il==0 || i>=80) break
		iops(j:j) = char(il)
		iopsav(j:j)=char(il)
	}
	ic=3
	ispfcn=4
	print *,'exiting f3,ispfcn=',ispfcn
	return

1000 ic=ihe
	if (il!=ihx) {
		do j= 1,10
			ipc(j)= 0
		return
	}
2000 ic= ihx
	do j= 1,10
		ipc(j)= 0
	return

1  format (' Function f3: Sequential Processor',//,5x,
	'This function sets up sequential processing for one or two files',/,5x,
	'Processing is then carried out by special funtion f4',//,5x,
	'Type in the function or  e  or  x  to exit',/)
5  format (' addition not covered under this routine--retype input',/)
6  format (' function not covered under this routine--retype input',/)
15 format (' type in the first file id, first record number, ',
						' last record number,',/,
	   ' and the record increment (separate numbers by spaces)',/)
25 format (' type in the second file id, beginning record number, ',
						'ending record number',/,
	   ' and the record increment',/)
35 format (' type in the options',/)
      end

	subroutine f2(ic)
	implicit integer*4 (i-n)
###################################################################
#	%W%		%G% %U%
###################################################################
#ccc  version date: %G%
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc                   this routine shifts the data
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc                    none
#ccc  argument list description:
#ccc     arguments: ic
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc
##########################################################

	include "../common/spmaxes"   # max parameters, must be first

	include "../common/blank"
	include "../common/lbl6"
	include "../common/lbl4"
	include "../common/label1"
	include "../common/lbl3"
	include "../common/lblg"
	include "../common/lundefs"
	include "../common/alphabet"
	
	character*8     inm
	character*10    ilr

#    data ihplus/'+'/
	ihplus = ihchar('+')
#
# this routine shifts data left or right by N channels
#
	if (ictrl == -1) {
		ic = ihx
		return
	}
2	call hreset(1)
	call whedr2
	write(ttyout,5) idv1,ifl1,ititl
	call crtin
	i=1
11	call wjfren(i,x,il)
	if (il==ihplus) goto 11
	if (il==ihe || il==ihx) {
		ic = il
		return
	}
	if (il!=0) goto 2
	if (x>maxchn | x<-maxchn) {
		write(ttyout,21)
21		format(' data number out of range'/)
		goto 2
	}
	n=x
	write(ttyout,52)
52	format(' working'/)
	if (ictrl==ihe) write(ttyout,53)
53	format(' shifting errors also'/)
#
# xn = fractional shift = (shift-integer shift)
#
	xn=n
	xn=x-xn
	if (abs(xn) < 0.001) xn=0
	xn = -xn
	do i=1,maxchn {
		datac(i)=0.
		datab(i)=0.
		error(i)=0.
	}
	if (ictrl==ihe) {
		idad=2
		itmp=ifl1
		is=0
		call devlun(4,idv1,is)
		call devsta(is,ista,0,iprt)
		if (ista<=0 | is==0) goto 1000
		call finfil(itmp,is,2,ier) # position before errors
		itmp = itmp+1              # compute error record number
		call finfil(itmp,is,2,ier) # get errors
		if (ier!=0) goto 1000
		itmp = ifl1
		call finfil(itmp,is,1,ier)
		if (ier!=0) goto 1000
	} else {
		call devlun(4,idv1,is)
		call devsta(is,ista,0,iprt)
		if (ista<=0 | is==0) goto 1000
		itmp = ifl1
		call finfil(itmp,is,1,ier)
		if (ier!=0) goto 1000
	}
	do i=1,maxchn {
		j=i+n
		if (j<1 | j>maxchn) next
		datac(j)=dataa(i)
		error(j)=datab(i)
	}
	do i=1,maxchn {
		data(i)=datac(i)
		datab(i)=error(i)
	}
	if (xn==0.) goto 69
#
# do fractional shift
#
	do i=1,maxchn {
		if (i==1 & xn>0.) {
61			x1=data(i)
			x2=data(i+1)
			e1=datab(i)
			e2=datab(i+1)
			error(i)=(e2-e1)*xn+e1
			datac(i)=(x2-x1)*xn+x1
		}
		else if (i==maxchn & xn<0.) {
62			x1=data(i-1)
			x2=data(i)
			e1=datab(i-1)
			e2=datab(i)
			error(i)=(e2-e1)*xn+e2
			datac(i)=(x2-x1)*xn+x2
		}
		else if (i>1 & i<maxchn) {
			datac(i)=data(i)
			if (xn<0) goto 62
			else goto 61
		}
	}
#
# decode history
#
69	if (x > 0.) ilr = 'right (+) '
	else ilr = ' left (-) '
	call namdev(idv1,inm)
	xn=abs(x)
	write(ihist,73) inm,ifl1,ilr,xn
	ic=0
	return
1000	ic=ihe
	write(ttyout,1001)
	call crtin
	return
5       format (' ',
'Function f2: Data Shift Left or Right # of Channels',//,
'     This function shifts data right of left by N number of channels',//,
'     Operating on: ',a,i4,':',a,//,
'     Enter the number of channels to shift data (+ = right, - = left)',/,
'     or type  e  to exit:',/)
73      format('f2: ',a8,' file',i5,' shift ',a,f8.3,
		' channels',4(1h ))
1001    format(' error: press return to exit'/)
	end

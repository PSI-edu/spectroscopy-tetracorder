	subroutine bndrem(ic)
	implicit integer*4 (i-n)

#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover & Lucy McFadden
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc         This subroutine is function 9 band removal function.
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc          er,crtin,wjfren,filid,devlun,finfil
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

	include "../common/spmaxes"   # max parameters, must be first

	include "../common/blank"
	include "../common/lbl6"
	include "../common/lbl7"
	include "../common/lblg"
	include "../common/lbl3"
	include "../common/lbl4"
	include "../common/label1"
	include "../common/lundefs"
	include "../common/alphabet"

	character*8 inm1


	call eralph
	ic=0
	xo1=1.0
	xo2=1.0
	xh1=1.0
	idv1=0
	ifle=0
	min=1.0
	write(ttyout,6)

5       continue
	write(ttyout,11)
	call crtin

#     decode input
	ir=0
	i=1
	call wjfren(i,x,il)
	if(il==ihe) ic=ihe
	if(il==ihx) ic=ihx
	if(ic==ihe || ic==ihx) return
	i=1
	call filid(i,ifl1,idv1)
	call devlun (4,idv1,idev1)
	if(idev1==0 || ifl1<=0) goto 5

	do j=1,10 {
		if(iops(j:j)=='e') ifle=ifl1+1
		if(iops(j:j)=='l') iref=ihl
		if(iops(j:j)=='r') iref=ihr
	}

	if(iref!=ihl && iref!=ihr) ir=1
	if(ir!=0) goto 5
16      continue
	ir=0
	write(ttyout,21) itrol(1), itrol(2)
	call crtin
	write(ttyout,25)
	i=1
	call wjfren(i,x,il)
	if (il==ihx) ic=ihx
	if(il==ihe) ic=ihe
	if(ic==ihe || ic==ihx) return
	if (il!=0 || x==0) ir=1
	xo1=x
	call wjfren(i,x,il)
	if(il!=0 || x==0) ir=1
	xo2=x
	call wjfren(i,x,il)
	if(il!=0 || x==0) ir=1
	xh1=x
	call wjfren(i,x,il)
	if(il==0 && x==0) il=ihh
	if(il!=ihh && il!=iha && il!=ihn) ir=1
	if(il==ihh && (xo1<2.6 || xo2>nchans)) goto 16
	il1=il

	call wjfren(i,x,il)
	if ((il==ihcv)|(il==ihcw)|(il==ihcd)|(il==ihcu)|(il==ihcy)) {
		iwtmpf = il
		call wjfren(i,x,il)
		if (il!=0) ir=1
		if((x > 0) & (x < maxrec)) {
			iwavfl = x
			itrol(1) = iwtmpf
			itrol(2) = x
		}
	}
	if(ir!=0) {
		write(ttyout,1000) xo1,xo2,xh1,il1,x
		goto 16
	}
	if(nchans>maxchn) nchans=maxchn

#      get wavelength file
	if(il1!=ihh) call wavlng (itrol(1), itrol(2), ier)
        if (ier!=0) goto 16

#     convert to energy space
	if (il1==ihn) {
		do j=1,nchans
			dataa(j)=1.0/dataa(j)

#     convert to channel space
	} else if (il1==iha || il1==ihn) {
		call bchop(xo1,mo1)
		call bchop(xo2,mo2)
		call bchop(xh1,mh1)
	} else if (il1==ihh) {
		mo1=xo1
		mo2=xo2
		mh1=xh1
	}

#     read file to calculate ideal band
	itmp = ifl1
	call finfil(itmp,idev1,1,ier)
	if(ier!=0) goto 5

#     get error file
	if(ifle!=0) {
		ifle = itmp +1
		call finfil(itmp,idev1,2,ier)
	}
	if(ifle==0) goto 47
	if(ier!=0) goto 5

	do j=1,nchans
		error(j)=datab(j)

	ictrl=ihe
	idad=2
47      write(ttyout,26) ititl,idev1,ifl1
	ititl1 = ititl(1:26) // ' band removed'

	if(ifle!=0) write(ttyout,27)

#     find band minimum - smaller value than 3 points on either side
	if(mh1<4) mh1=4
	do i=mh1,mo2 {
		if ( (dataa(i) <= dataa(i+1))
		   & (dataa(i) <= dataa(i-1))
		   & (dataa(i) <  dataa(i+2))
		   & (dataa(i) <  dataa(i-2))
		   & (dataa(i) <  dataa(i+3))
		   & (dataa(i) <  dataa(i-3)) ) {
			min=i
			goto 60
		}
	}
	write(ttyout,61)

#     if no band min, goto entering parameters
	goto 16

60      height=1.0-dataa(min)
	write(ttyout,28) min,dataa(min)
	do k=1,nchans
		datab(k)=dataa(k)

#     reflect band
	if(iref==ihr) goto 54
	if(iref==ihl) im=min-mo1
	mo=mo1
	do j=1,im
		datab(min+j)=dataa(min-j)

54      if (iref==ihr) im=mo2-min
	mo=mo2

	do j=1,im
		datab(min-j)=dataa(min+j)

#     status: reflected band in datab, continuum removed spect in
#     dataa.  procedure to follow: divide dataa/datab, exit.
	do i=1,nchans {
		if(abs(datab(i))<=0.1e-35) datab(i)=0.1e-35
		if(abs(dataa(i))<=0.1e-35) dataa(i)=0.1e-35
		datac(i)=dataa(i)/datab(i)
		error(i)=datac(i)*(error(i)/dataa(i))
	}

# title and history
	call namdev(idv1,inm1)
	write(ihist,100) inm1,ifl1,mo,min
	write (ititl2,104) inm1,ifl1
	write(mhist,102) ititl2,height

      return

6       format( ' Function f9',/,
' Band Removal, Reflection Method. ',/,
'     Band minimum is found and reflected about axis at band minimum ')

11      format(' ',
'Type in file id and record # to be processed,  e to include ERRORS,',/,
'     followed by l or r for left or right side of band to be reflected.',/,
'     Type  e  or  x  to EXIT. ')

21      format(' ',
'Enter OUTER LIMITS of Band (2 values),estimated Half Height point on side ',/,
' to be reflected followed by h,a,n (Channel,Wavelength,Energy)',/,
' and wavelength file id and rec #.(default=',a,i5,')',/,
' Type  e  or  x  to EXIT ')

25      format(1x,'WORKING')

26      format(' Spectrum: ',a,5x,a,i4)

27      format(' Errors Included')

28      format(' band minimum at ',i4,' equals ',f5.3)

61      format(' NO MINIMUM in specified region')

100     format(a8,' file',i4,' band reflection,channels',i4,'-',i4)

102     format(a,'height= ',f6.3,20(1h ))

104     format('band removed from ',a,1x,i4)

1000    format(' INVALID INPUT. Band limits =',2e14.6,/,
	       ' Half height point =',e14.6,/,
	       ' Wavelength file =', a, I4)
	end

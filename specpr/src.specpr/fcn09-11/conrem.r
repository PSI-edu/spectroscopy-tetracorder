	subroutine conrem(ic)
	implicit integer*4 (i-n)

#ccc  version date: 06/01/83
#ccc  author(s): Lucy McFadden, Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc         This subroutine matches a continuum to two given
#ccc         points of a spectrum and removes the continuum
#ccc         by dividing.
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc          wjfren,crtin,finfil,namdev,filid,devlun,devsta
#ccc  argument list description:
#ccc    arguments: ic
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
	include "../common/label1"
	include "../common/lbl4"
	include "../common/lbl3"
	include "../common/lblg"
	include "../common/lbl6"
	include "../common/lundefs"
	include "../common/alphabet"

	character*8	inm1,inm2

	m1=0
	m2=0
	ic=0
	call hreset(1)
1	write(ttyout,4)
4	format(' Function f5',//,
		' This routine Matches a CONTINUUM to two given points',/,
		' of a spectrum and REMOVES the continuum by DIVIDING ',/)
3	continue
	ir=0

	write(ttyout,6) itrol(1), itrol(2)
6	format (
' Enter two values to which CONTINUUM is to be MATCHED,',/,
' followed by  h,a or n  representing units of channel,wavelength',/,
' and energy space, and the wavelength file id and rec # (default=',a,i5,'). ',/,
' enter e or x to exit ')
	call crtin

	i=1
	call wjfren(i,x,il)
	if(il==ihx || il==ihe) {
		ic = il
		return
	}
	if(il!=0 || x==0) ir=1
	x1=x
	call wjfren(i,x,il)
	if(il!=0 || x==0) ir=1
	x2=x
	call wjfren(i,x,il)
#
#     default is channel input, wavelength file given by itrol
#
	if(il==0 && x==0) il=ihh
	if(il!=ihh && il!=iha && il!=ihn) ir=1
#
#     check to see if default is valid
#
	if(il==ihh && (x1<2.6 || x2>nchans)) {
		write(ttyout,100) x1,x2
100		format(' il==ihh x1,x2=',2e16.6)
		goto 3
	}
	il1=il
	call wjfren(i,x,il)
	iwtmpf = il
	call wjfren(i,x,il)
	if(il!=0) ir=1
	iwavfl=x
	if(ir!=0) {
		write(ttyout,102) x1,x2
102		format(' ir!=0, x1,x2=',2e16.6)
		goto 3
	}
#
#   read wavelength file
#
	if(nchans>maxchn) nchans=maxchn
	if(il1!=ihh) call wavlng(iwtmpf,iwavfl,ier)

	if (ier!=0) goto 3

#
#     convert to energy space
#
      if(il1==ihn) {
		do j=1,nchans {
			if(dataa(j)<0.1e-25) dataa(j)=-1.0
			dataa(j)=1.0/dataa(j)
		}
#
#  convert  to channel space
#
	} else if(il1==iha || il1==ihn) {
		call bchop(x1,m1)
		call bchop(x2,m2)
	} else {
		m1=x1
		m2=x2
	}
	if(m1>=nchans || m2>=nchans) {
		write(ttyout,104) nchans,m1,m2
104		format(' m1,m2>nchans.',3i7)
		goto 1
	}
#
#     define and get file to work on
#
20	write(ttyout,30)
30	format (' Enter spectrum file id and #,  e to Include Errors,',/,
		'       followed by continuum file id and #',/,
		' Type  x  to EXIT ',/)
	call crtin
	write(ttyout,35)
35	format(' working')
	idv1=0
	idv2=0
	i=1
	call wjfren(i,x,il)
	if(il==ihx) ic=ihx
	if(il==ihe) ic=ihe
	if(ic==ihx || ic==ihe) return
	i=1
	call filid(i,ifl1,idv1)
	call devlun(4,idv1,idev1)
	if(idev1==0 || ifl1<=0) goto 20
	call devsta(idev1,ista,1,iprt)
	if(ista<=0) goto 20
	iftmp=ifl1
	call finfil(ifl1,idev1,1,ier)
	ifl1=iftmp
	if(ier!=0) goto 20
	write(ttyout,36) ititl,idv1,ifl1
36	format(' spectrum:   ',a,5x,a,i4)
	ititl1 = ititl(1:22) // ' continuum removed'
#
#     get error file, set variables for plotting
#
	do j=1,10 {
		if(iops(j:j)=='e') {
			ifle=ifl1+1
			call finfil(ifle,idev1,2,ier)
		}
		if(iops(j:j)!='e') break
		if(ier!=0) goto 20
		do k=1,nchans
			error(k)=datab(k)
		ictrl=ihe
		if(iops(j:j)=='e') goto 14
	}
14	if(ictrl==ihe) write(ttyout,29)
29	format(' errors included')
#
# get continuum file
#
	call filid(i,ifl2,idv2)
	call devlun(4,idv2,idev2)
	if(idev2==0 || ifl2<=0) goto 20
	call finfil(ifl2,idev2,2,ier)
	if(ier!=0) goto 20
	write(ttyout,37) ititl,idv2,ifl2
37	format(' continuum:  ',a,5x,a,i4)
#
#     calculating fitted slope and intercept
#
	y=datab(m1)-datab(m2)
	if(abs(y)<0.1e-35) y=0.1e-35
	scalm=(dataa(m1)-dataa(m2))/y
	bint=dataa(m1)-(scalm*datab(m1))
#
#     calculating fitted continuum
#
	do i=1,nchans
		if (datab(i)!=-1.23e34) datab(i)=(scalm*datab(i))+bint
#
#     dividing spectrum by fitted continuum
#
	do i=1,nchans {
		if (datab(i)==-1.23e34 ||
			dataa(i)==-1.23e34) {
				datac(i)=-1.23e34
		} else {
			if(abs(datab(i))<0.1e-35) datab(i)=0.1e-35
			datac(i)=dataa(i)/datab(i)
			if(abs(dataa(i))<0.1e-35) dataa(i)=0.1e-35
			error(i)=datac(i)*(error(i)/dataa(i))
		}
	}
#
#     calculate title and history
#
	call namdev(idv1,inm1)
	write(ititl2,23) inm1, ifl1
23	format('continuum removed from ', a,' file',i4)
	mhist(1:74) = ititl2

	call namdev(idv2,inm2)
	write (ihist,25) inm1,ifl1,inm2,ifl2,m1,m2
25	format('f5: ',a,' f',i4,'; cont.=',a,' f',i4,
		'; fit @ ch.',i3,' & ',i3)

	return
	end

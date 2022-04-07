	subroutine featur(dataa,datab,datac,idat,data,jdat,anadat,number,qp)
	implicit integer*4 (i-q)

#ccc  name: featur
#ccc  version date: June 30, 1987
#ccc  author(s): Noel Gorelick
#ccc  language: RATFOR
#ccc
#ccc  short description: Contains all the calls to do a feature analysis
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

	include "../common/spmaxes"   # max parameters, must be first

	real*4 dataa(number),datab(number),datac(number),jdat(number),
			data(number),anadat(SPMAXCHAN),maxnm,temp
# RED
	integer*4 g
	integer*4 rc,num,it,idat(number),f,h,qp,itnum,number

# RED Initialize maxnm to 0
	maxnm = 0
#	initialize
	qp=18
	g=0
	imp=1
	do i=1,number {
		if (maxnm<datab(i)) maxnm=datab(i)
	}
	do i=1,number {
		idat(i)=0
		jdat(i)=maxnm
	}
#	first iteration
	call hull(dataa,datab,idat,number,data,imp)
	call confix(dataa,datab,idat,datac,data,number)
	call fill(idat,dataa,datab,number,datac)	
	call zfirst(dataa,datab,datac,data,jdat,number,anadat)
#----------------------------------------------------
#	start other iterations
	itnum=2
332	call fill(idat,dataa,datab,number,datac)	
	call remove(datab,datac,datab,number)
	do i=1,number {
		if (datac(i)==-1.23e34) next
		if (abs(1.0-datac(i))>0.01) go to 334
	}
	go to 333
334	call remove(data,datac,data,number)
	do l=1,number { 
		if (datac(l)==-1.23e34) next
		jdat(l)=jdat(l)*datac(l)
	}
	g=0
 	call pfind(idat,number,g,ilast)
71	if (ilast<number) {
		f=0
		h=0
	 	call pfind(idat,number,ilast,inext)
		call lowp(datab,ilast,inext,ilow)
	 	call hull(dataa(ilast),datab(ilast),idat(ilast),
			ilow-ilast+1,data(ilast),f)
	 	call hull(dataa(ilow),datab(ilow),idat(ilow),
			inext-ilow+1,data(ilow),h)
		if (f==1 & h==1) {
			idat(ilow)=1
			call should(dataa(ilast),datab(ilast),
				ilow-ilast+1,idat(ilast),data(ilast),1)
			call should(dataa(ilow),datab(ilow),
				inext-ilow+1,idat(ilow),data(ilow),-1)
			call fill(idat,dataa,datab,number,datac)	
			goto 444
		}
		call confix(dataa(ilast),datab(ilast),idat(ilast),
			datac(ilast),data(ilast),inext-ilast+1)
444		ilast=inext
	 	go to 71
	}
	call fill(idat,dataa,datab,number,datac)	
	call zfeat(dataa,datab,datac,data,jdat,number,itnum,anadat,qp)
	itnum=itnum+1
	go to 332
#	-----------------------------------------------
333	return
	end

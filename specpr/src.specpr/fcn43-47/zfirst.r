	subroutine zfirst(dataa,datab,datac,data,jdat,number,anadat)
	implicit integer*4 (i-q)

#ccc  name: zfirst
#ccc  version date: June 30, 1987
#ccc  author(s): Noel Gorelick
#ccc  language: RATFOR
#ccc
#ccc  short description: Does the first iteration (2 channels) for
#ccc			feature analysis (zstuf for upper hull)
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
	real*4 dataa(number),datab(number),datac(number),jdat(number)
	real*4 anadat(18),data(number),center,width,depth,error,ewidth
	real*4 width1,y1,y2,x1,x2,asym,contum,fract,maxnm,low,hfmax,ehfmax
	integer*4 number,itnum,upchan,lwchan,counter
	maxnm=0
	imax=1
	low=1.0e25
#RED Initialize to 1
	ilow=1

	do i=1,number {
		if (datac(i)>maxnm) {
			maxnm=datac(i)
			imax=i
		}
	}
#
# make sure that there are some no-delted channels here.
#
	if (imax!=1) {
		do i=1,imax-1 {
			if (datac(i)!=-1.23e34 & dataa(i)!=-1.23e34) {
				go to 30
			}
		}
	}
	go to 50
30	do i=1,imax {
		if (datac(i)< low & datac(i)!=-1.23e34 & 
					dataa(i)!=-1.23e34) {
			ilow=i
			low=datac(i)
		}
	}
	if (maxnm==0) {
		return
	}
	center=dataa(ilow)
	depth=1-datac(ilow)/maxnm
	error=(data(imax)+data(ilow))/2.0
	hfmax=(maxnm+datac(ilow))/2.0
	ehfmax=maxnm-(maxnm-datac(ilow))*(3.0/4.0)
	do i=ilow,imax {
		if (datac(i)>hfmax) {
			upchan=i
			go to 10
		}
	}
	upchan=imax
10	do i=upchan-1,ilow,-1 {
		if (datac(i)!=-1.23e34 & dataa(i)!=-1.23e34) {
			lwchan=i
			go to 20
		}
	}
	lwchan=ilow
20	if (upchan==lwchan) {
		upchan=imax
		lwchan=ilow
	}
	y1=datac(upchan)
	y2=datac(lwchan)
	x1=dataa(upchan)
	x2=dataa(lwchan)
	width1=((hfmax-y1)/(y2-y1))*(x2-x1)+x1
	width=(width1-center)*2

	do i=ilow,imax {
		if (datac(i)>ehfmax) {
			upchan=i
			go to 210
		}
	}
	upchan=imax
210	do i=upchan-1,ilow,-1 {
		if (datac(i)!=-1.23e34 & dataa(i)!=-1.23e34) {
			lwchan=i
			go to 220
		}
	}
	lwchan=ilow
220	if (upchan==lwchan) {
		upchan=imax
		lwchan=ilow
	}
	y1=datac(upchan)
	y2=datac(lwchan)
	x1=dataa(upchan)
	x2=dataa(lwchan)
	width1=((ehfmax-y1)/(y2-y1))*(x2-x1)+x1
	ewidth=(width1-center)*2
	asym=1.0
	contum=jdat(ilow)
	counter=0

	do i=1,imax {
		if (datab(i)!=-1.23e34 & dataa(i)!=-1.23e34) {
			if (datab(i)==datac(i)) {
				counter=counter+1
			}
		}
	}
	if (imax-1>0) {
		fract=(float(counter)-1.0)/(float(imax)-1.0)
	} else {
		fract=-1.23e34
	}
	
	anadat(1)=center
	anadat(2)=width
	anadat(3)=depth
	anadat(4)=error
	anadat(5)=asym
	anadat(6)=contum
	anadat(7)=1
	anadat(8)=fract
	anadat(9)=ewidth
#
# check fo delete points again
#
50	if (imax!=number) {
		do i=imax+1,number {
			if (datac(i)!=-1.23e34 & dataa(i)!=-1.23e34) {
				go to 60
			}
		}
	}
	go to 90
60	ilow=0
	low=1.0e25
	do i=imax,number {
		if (datac(i)< low & datac(i)!=-1.23e34 
					& dataa(i)!=-1.23e34) {
			ilow=i
			low=datac(i)
		}
	}
	center=dataa(ilow)
	depth=1-datac(ilow)/maxnm
	error=(data(imax)+data(ilow))/2.0
	hfmax=(maxnm+datac(ilow))/2.0
	ehfmax=maxnm-(maxnm-datac(ilow))*(3.0/4.0)
	do i=imax,ilow {
		if (datac(i)<hfmax) {
			lwchan=i
			go to 70
		}
	}
	lwchan=ilow
70	do i=lwchan-1,imax,-1 {
		if (datac(i)!=-1.23e34 & dataa(i)!=-1.23e34) {
			upchan=i
			go to 80
		}
	}
	upchan=imax
80	if (upchan==lwchan) {
		upchan=imax
		lwchan=ilow
	}
	y2=datac(upchan)
	y1=datac(lwchan)
	x2=dataa(upchan)
	x1=dataa(lwchan)
	width1=((hfmax-y1)/(y2-y1))*(x2-x1)+x1
	width=(center-width1)*2
	do i=imax,ilow {
		if (datac(i)<ehfmax) {
			lwchan=i
			go to 270
		}
	}
	lwchan=ilow
270	do i=lwchan-1,imax,-1 {
		if (datac(i)!=-1.23e34 & dataa(i)!=-1.23e34) {
			upchan=i
			go to 280
		}
	}
	upchan=imax
280	if (upchan==lwchan) {
		upchan=imax
		lwchan=ilow
	}
	y2=datac(upchan)
	y1=datac(lwchan)
	x2=dataa(upchan)
	x1=dataa(lwchan)
	width1=((ehfmax-y1)/(y2-y1))*(x2-x1)+x1
	ewidth=(center-width1)*2
	asym=1
	contum=jdat(ilow)
	counter=0
	do i=imax,number {
		if (datab(i)!=-1.23e34 & dataa(i)!=-1.23e34) {
			if (datab(i)==datac(i)) {
				counter=counter+1
			}
		}
	}
	if (number-imax>0) {
		fract=float(counter-1)/float(number-imax+1)
	} else {
		fract=-1.23e34
	}
	
	anadat(10)=center
	anadat(11)=width
	anadat(12)=depth
	anadat(13)=error
	anadat(14)=asym
	anadat(15)=contum
	anadat(16)=1
	anadat(17)=fract
	anadat(18)=ewidth
90	return
	end

	subroutine confix(dataa,datab,idat,datac,data,number)
	implicit integer*4 (i-q)
	real*4 dataa(number),datab(number),datac(number),maxnm,data(number)
	integer*4 imax,q,idat(number),number,b,que

#ccc  name: confix  
#ccc  version date: May 27, 1987
#ccc  author(s): Noel Gorelick
#ccc  language: ratfor
#ccc
#ccc  short description: Routine fixes lower-than continuum values
#ccc
#ccc  algorithm description: Find highest data-removed value, above
#ccc			     continuum, and set to equal continuum
#ccc  system requirements:
#ccc  subroutines called:
#ccc			fill(idat,dataa,datab,number,datac)
#ccc			remove(datab,datac,datac,number)
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
#	initialize
	que=0
	if (number==1) return
1	do i=1,number { 
		if (datab(i)!=-1.23e34) que=1
		datac(i)=0.0
	}	
	if (que==0) return
#	start checking
	call fill(idat,dataa,datab,number,datac)
	call remove(datab,datac,datac,number)
	high=0.0
	ihigh=0
	do i=1,number {
		if (datac(i)+data(i)>1.0 && idat(i)!=1) {
			if (datac(i)+data(i)>high) {
				high=datac(i)+data(i)
				ihigh=i
			}
		}
	}
	if (ihigh==0) return
	idat(ihigh)=1
	go to 1
	end	

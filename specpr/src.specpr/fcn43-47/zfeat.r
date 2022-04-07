	subroutine zfeat(dataa,datab,datac,data,jdat,number,itnum,anadat,y)
	implicit integer*4 (i-q)

#ccc  name: zfeat
#ccc  version date: June 30, 1987
#ccc  author(s): Noel Gorelick
#ccc  language: RATFOR
#ccc
#ccc  short description: Finds continuum points for feature analysis
#ccc			and calls zstuf to do the dirty work
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

# RED Declared ilast to ensure integer*4 vs default of real*4 for undeclared
	integer*4 ilast
	integer*4 itnum,number,y,z,ilow
	real*4 center,width,depth,asym,contum,fract,anadat(SPMAXCHAN),low,error
	real*4 dataa(number),datab(number),datac(number),
		data(number),jdat(number),ewidth
#RED Initialize ilast to number 
	ilast=number

1	do i=1,number {
		if (datac(i)==-1.23e34 | dataa(i)==-1.23e34) next
		if (datac(i)==1.0) {
			ilast=i
			go to 2
		}
	}
2	if (ilast==number) return
	if (y+9 > SPMAXCHAN) return   # will overflow 4864 limit
	do i=ilast+1,number {
		if (datac(i)==-1.23e34 | dataa(i)==-1.23e34) next
		if (datac(i)!=1.0) {
			go to 3
		} else {
			ilast=i
			go to 2
		}
	}
3	do j=i,number {
		if (datac(j)==1.0) {
			inext=j
			go to 4
		}
	}
	return
4	if (inext-ilast==2) {
		error=(data(ilast)+data(inext))/2+data(ilast+1)
		if (1.0-datac(ilast+1)>2*error) {
			ilow=ilast+1
			go to 10
		} else {
			ilast=inext
			go to 2
		}
	}
	low=1.0e25
	do k=ilast,inext {
		if (datac(k)==-1.23e34 | dataa(k)==-1.23e34) next	
		if (datac(k)<low) {
			low=datac(k)
			ilow=k
		}
	}
	error=(data(ilast)+data(inext))/2+data(ilow)
	if (1.0-datac(ilow)>error) {
		go to 10
	} else {
		ilast=inext
		go to 2
	}
#	--------
10	call zstuf(dataa(ilast),datab(ilast),datac(ilast),jdat(ilast),
		inext-ilast+1,center,depth,width,asym,contum,fract,
		ilow-ilast+1,ewidth)
#	write (6,*) 'ewidth',ewidth
# 	-----------
#	put in data
#	-----------
		anadat(y+1)=center
		anadat(y+2)=width
		anadat(y+3)=depth
		anadat(y+4)=error/2
		anadat(y+5)=asym
		anadat(y+6)=contum
		anadat(y+7)=itnum
		anadat(y+8)=fract
		anadat(y+9)=ewidth
	y=y+9
	ilast=inext
	go to 2
	end	

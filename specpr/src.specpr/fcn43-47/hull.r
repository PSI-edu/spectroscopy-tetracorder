	subroutine hull(dataa,datab,idat,number,data,mark)
	implicit integer*4 (i-q)
	real*4 dataa(number),datab(number),data(number),maxnm,low,high
	integer*4 idat(number),q,que,b,t,d,mark,qr

#ccc  name: hull
#ccc  version date: May 27, 1987 (original), noe Dec 15, 1997
#ccc  author(s): Noel Gorelick
#ccc  language: ratfor
#ccc
#ccc  short description: finds peaks using min/max method
#ccc
#ccc  algorithm description: find local min, then global maximum
#ccc  system requirements:
#ccc  subroutines called:
#ccc  argument list description:
#ccc				mark == que for left/right no peak filling
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc                      added checks for wavelengths not deleted (=dataa)
#ccc                      Roger N. Clark 12/15/1997
#ccc  NOTES:
#ccc
#	initialize
	maxnm=-0.1e25
	imax=0
#	find max value
	do i=1,number {
		if(maxnm<=datab(i) & dataa(i) != -1.23e34) {
			maxnm=datab(i)
			imax=i
		}
	}
	if (imax==0) return
	idat(imax)=1
	inix=imax
# -------------  
#
# -------------
#	set left/right values
	if (imax==1) {
		b=number
		q=1
		que=1
	} else {
		b=1
		q=-1
		que=0    # added so que is surely initialized.  3/19/90 R. Clark
		if (imax==number) que=1
	}
	qr=0
#	Find a valid low point
1	low=1.0e25
	do i=imax,b,q {
		if (i==b) {
#	this bit here fills in only on first iteration, with no lowp
			if (mark==1) {
				do l=imax,b,q {
					if (datab(l)!=-1.23e34 & 
						dataa(l) != -1.23e34) idat(l)=1
				}
			}
#	-----
			if (que==1) {
				if (qr==0) mark=1
				imax=inix
				return
			} else {
				b=number
				q=1
				que=1
				imax=inix
				qr=0
				go to 1
			}
		}
		if (datab(i)==-1.23e34 | dataa(i) == -1.23e34) next
		if (datab(i)<low) {
			low=datab(i)
			ilow=i
		} else {
			if ((datab(i)-low) <= (data(ilow)+data(i))) next
			else {
				go to 10
			}
		}
	}
#	find the highest value
10	high=-1.0e25
	do j=i,b,q {
		if (datab(j)>high & dataa(j) != -1.23e34) {
			high=datab(j)
			ihigh=j
		}
	}
	idat(ihigh)=1
	qr=1
	imax=ihigh
	go to 1
	end

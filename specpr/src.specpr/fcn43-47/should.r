	subroutine should(dataa,datab,number,idat,data,q)
	implicit integer*4 (i-q)

	include "../common/spmaxes"   # max parameters, must be first

	integer*4 number,idat(number),q,a,b
	real*4 dataa(number),datab(number),junk(SPMAXCHAN),data(number)
	real*4 last,tol,high

	call fill(idat,dataa,datab,number,junk)	
	call remove(datab,junk,junk,number)

	lowchk=0
	last=junk(1)
	ilast=1
	do i=2,number {
		tol=data(i)+data(ilast)
		if (lowchk==0) {		#Ok, it's going down
			if (junk(i)>(last+tol)) {	#ok, it's up
				ilast=i
				last=junk(i)
				lowchk=1
			} else if (junk(i) < last) {	# Still down
				ilast=i
				last=junk(i)
			} else {
				next
			}
		} else {			#Ok, it's going up
			if ((junk(i)+tol)<last) {	#Good point
				idat(ilast)=1
				ilast=i
				last=junk(i)
				lowchk=0
			} else if (junk(i) > last) {	#Still up
				ilast=i
				last=junk(i)
			} else {
				next
			}
		}
	}
#
# Fill in from low back to first idat
#

	if (q==1) {
		a=number-1
		b=1
	} else {
		a=2
		b=number
	}
	do i=a,b,-q {
		if (idat(i)==0) {
			idat(i)=1
		} else {
			goto 20
		}
	} 
#
# Now, make it do it's own confix
#
20	call fill(idat,dataa,datab,number,junk)	
	call remove(datab,junk,junk,number)
	high = 0.0
	do i=1,number {
		if (junk(i)>high) {
			high=junk(i)
			ihigh = i
		}
	}
	if (high-1.00 > 1.0e-5) {
		idat(ihigh)=1
		goto 20
	}
		
	return
	end

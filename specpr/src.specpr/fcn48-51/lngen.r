	subroutine lngen(nbs,nch,ypt,xpt,ic)

	implicit integer*4 (i-n)
	include "../common/dscrch"
	include "../common/lundefs"
	real*4 xpt(20),ypt(20)

#	do k=1,nbs{
#	   write(ttyout,25) k, xpt(k), ypt(k)
#25	   format('pt # ',i3,' x-val= ',f9.4,' y-val= ',f9.4)
#	 }

	do k=1,nbs-1 {
# 	*********** find slope (m) and y-intercept (b) ***
		slope=ypt(k+1)-ypt(k)
# 	*********** check to see if denominator is zero ***
		denom=xpt(k+1)-xpt(k)
		if (denom==0) {
			write(ttyout,525)
			denom=1.0e-37
		   }
		slope=slope/denom
		b=ypt(k)-slope*xpt(k)
		n1 = nint(xpt(k))
		n2 = nint(xpt(k+1))

		do j= n1+1,n2 {
			datsc4(j) = slope*j + b
		    }

		if (k==1) {
			do j= 1,n1
			    datsc4(j) = slope*j + b
		   }
		if (k==nbs-1) {
			do j= n2,nch 
			     datsc4(j) = slope*j + b
		   }
	    }


525     format (/,
         ' *** ERROR: denom. in equ. is zero. it is reset to 1.0e-37.  ***',/)

	return
	end

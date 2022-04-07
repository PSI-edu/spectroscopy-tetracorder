	subroutine lsqft(c,a,b,nchans,sigmay,factx,factc,n,ictrl)

	implicit integer*4 (i-n)
	
	double precision sum,sumx,sumy,sumx2,factx,delta,
			 sumxy,sumy2,x,y,weight,factc,sumsig,avsig

	real*4 sigmay(nchans),a(nchans),b(nchans),c(nchans)

	include "../common/alphabet"
	include "../common/lundefs"

	sumsig = 0.0
	sum = 0.0
	sumx = 0.0
	sumy= 0.0
	sumx2 = 0.0
	sumxy = 0.0
	sumy2 = 0.0
	weight=1
	n = 0
	do i=1,nchans{
	    sumsig = sumsig + sigmay(i)
	}
	avsig = sumsig/nchans

	do i = 1,nchans {

		if (ictrl==ihe){
			if (sigmay(i)<=0.1*avsig) { 
				sigmay(i)=0.1*avsig
				weight= (avsig/sigmay(i))**2
			} else {
		   		weight = 1/sigmay(i)**2
			}
		}
		if (ictrl != ihe) {
		     weight=1
		}

		if (a(i)<1.1e-36 || a(i)>1.0) a(i)=-1.23e34
		if (b(i)<1.1e-36 || b(i)>1.0) b(i)=-1.23e34
		if (c(i)<1.1e-36 || c(i)>1.0) c(i)=-1.23e34

		if (c(i) == -1.23e34 ||
		    a(i) == -1.23e34 ||
		    b(i) == -1.23e34) write(ttyout,60) i
		else {
		      x= (b(i)-a(i))/c(i)
		      y= b(i)/c(i)
		      sum = sum+weight
		      sumx= sumx+weight*x
		      sumy= sumy+weight*y
		      sumx2= sumx2+weight*x*x
		      sumxy=sumxy+weight*x*y
		      sumy2=sumy2+weight*y*y
		      n = n + 1
#  DEBUGS:
#		      write(ttyout,45)i,a(i)
#		      write(ttyout,46)i,b(i)
#		      write(ttyout,47)i,c(i)
#		      write(ttyout,44)avsig
#		      write(ttyout,50)x
#		      write(ttyout,51)y
#		      write(ttyout,49)weight

#45	format(1x,'DEBUG: a(',i3,')= ',1pe13.6)
#46	format(1x,'     : b(',i3,')= ',1pe13.6)
#47	format(1x,'     : c(',i3,')= ',1pe13.6)
#49	format(1x,'     : weight = ',1pe13.6)
#44	format(1x,'     : avsig = ',1pe13.6)
#50	format(1x,'     : (b(i)-a(i))/c(i) = ',1pe13.6)
#51	format(1x,'     : b(i)/c(i) = ',1pe13.6)
		}
	}

	delta = sum*sumx2 - sumx*sumx
	factc = (sumx2*sumy - sumx*sumxy)/delta

	write(ttyout,70)factc
70	format(1x,'DEBUG factc = ',1pe12.6)
	if (delta == 0.0) {
		write(ttyout,80)
80		format(1x,' *** solution does not exist')
	} else {
		factx = (sumxy*sum - sumx*sumy)/delta
	}

	return

60	format(' channel',i3,' is a deleted point')

	end

	subroutine wavavg(wav,istart,nchans,i,avgn,xn,dir)
#	subroutine wavavg(wav,istart,nchans,i,avgn,xn)
	implicit integer*4 (i-q)

	real*4 wav(nchans),avgn,xn(nchans)
	integer*4 nchans,dir

	if (istart==i) {
		avgn=xn(i)
		return
	}
	wstart=wav(i)
	if (dir>0) {
		do j=i-1,istart,-1 {
			if (abs(wstart-wav(j))>.1) {
			go to 10
			}
		}
	} else {
		do j=i+1,istart,1 {
			if (abs(wstart-wav(j))>.1) {
			go to 10
			}
		}
	}
10 	xtmp=0
	x=0
	do k=j,i,dir {
		if (xn(k)<0) next
		x=x+1
		xtmp=xtmp+xn(k)
	}
	if (x==0) {
		avgn=xn(i)
		return
	}
	avgn=xtmp/x
	if (avgn<0) {
		avgn=xn(i)
	}
	return
	end

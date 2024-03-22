	subroutine kfind (nminer,w,xn,xk,d,weight,dens,mu0,mu,wsmean,g,
			wband,currch,sd,inrmlc,r,debug,rt)
	implicit integer*4 (i-q)


	include "../../src.specpr/common/spmaxes"   # max parameters, must be first

	include "defs.h"
	real*4 d(nminer), weight(nminer)
	integer*4 currch,aflag,flag,debug
	real*4 xn(nminer), xk(nminer), r(nminer),rt(nminer)
	real*4 dens(nminer)
	real*4 ws1(NMINL), ws(NMINL), s(NMINL), wband(SPMAXCHAN,NMINL)
	real*4 mu,mu0,mu4,xkmin,xkmax,rx,r1

###### NOTE: 10/2015 needs to get iflgse and pass to mrefl3

	iflag=1
	do j=1,nminer {
		step=10
		xkmax=10000
		xkmin=.00001
		rlast=1.0e30
		dir=1

                #### redo!
                #### This call is out of date.  the 
                #### newer mrefl3 has 3 more variables.  this won't work
20		call mrefl3(j,w,xn,xk,d,weight,dens,mu0,mu,
			rx,wsmean,r1,g,wband,currch,sd,inrmlc,iflag,iflgse)

	if (debug==2) {
		write (6,100) '|K:',j,' k=',xk(j),' n=',xn(j),' st=',step,
			' mx=',xkmax,' mn=',xkmin,' r=',r(j),'rx=',rx
100	format (a,i1,a,f9.3,a,f6.3,a,f8.5,a,f9.3,a,f5.3,a,f7.5,a,f7.5)
	}

		if (r(j)==1.23e34) {
			r(j)=-1.23e34
			xk(1)=-1.23e34
			return	
		}
		if (abs(r(j)-rx) <= 0.00001) {
			rt(j)=rx
			next
		}
		
		adiff=abs(rx-r(j))
		bdiff=abs(rlast-r(j))

		if (adiff<bdiff) {			# right 
			if (dir==1) {
				xlast=xk(j)
				xk(j)=xk(j)*(1.0+step)
			} else {
				xlast=xk(j)
				xk(j)=xk(j)/(1.0+step)
			}
		} else if (adiff>bdiff) {		# wrong
			if (dir==1) {
				xlast=xk(j)
				xk(j)=xk(j)/(1.0+step)
			} else {
				xlast=xk(j)
				xk(j)=xk(j)*(1.0+step)
			}
			step=step*.5
			dir=dir*(-1)
		} else {				# very wrong
			if (dir==1) {
				xlast=xk(j)
				xk(j)=xk(j)/(1.0+step)*(1.0+step*.5)
			} else {
				xlast=xk(j)
				xk(j)=xk(j)*(1.0+step/.5)
			}
			step=step*.5
		}
		rlast=rx
		if (xk(j)>xkmax) xk(j)=xkmax
		if (xk(j)<xkmin) xk(j)=xkmin
		if (xlast==xk(j)) {
			rt(j)=rx
			next
		}
		go to 20
	}
	return
	end

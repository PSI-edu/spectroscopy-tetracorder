	subroutine nweigh(xn,nminer,r,debug,rt,xk,chan,iter,xnl,
			wlast,wav,avgn,dir,iwav1)
	implicit integer*4 (i-q)

	real*4 xn(nminer),wn(9),r(nminer),top,bot,rt(nminer),xk(nminer),avgn
	integer*4 debug,chan,dir

	top = 0
	bot = 0
	n=15

	if (debug==3) {
		write (6,400) chan,iter
400		format (//1x,'Channel=',i3,t19,'k',t34,'|N|',t46,'weight',
			t62,'real r',t73,'rx'/,1x,'Iteration=',i2)
	}
	do j=1,nminer {
		if (r(j)==1) {
			xn(1)=0
			return
		}
#		wn(j)=(1-r(j))**n
		wn(j)=1
		if (debug==2) {
			write (6,*) 'weight of n:',j, ' - ',wn(j)
		} else if (debug==3) {
			write (6,410) xk(j),xn(j),wn(j),r(j),rt(j)
410			format (t15,f9.4,t30,f5.3,t45,f7.4,t60,f7.6,t70,f7.6)
		}
		top=top+wn(j)*xn(j)
		bot=bot+wn(j)
	}
	xn(1)=top/bot
	diff=xn(1)-xnl
	if (iwav1==chan) {
		xn(1)=xnl
		go to 132
	}
	if (diff==0) {
		go to 132
	}
	if (wlast!=wav) {
		adiff=abs((xn(1)-xnl)/(wlast-wav))
	} else {
		adiff=0
	}
	xslope=(0.1+(0.1/wav))
	if (adiff>xslope) {
		if (diff>0) {
			if (dir>0) {
				xn(1)=avgn
			} else {
				xn(1)=xnl+xslope*abs(wlast-wav)
			}
		} else {
			if (dir<0) {
				xn(1)=avgn
			} else {
				xn(1)=xnl-xslope*abs(wlast-wav)
			}
		}
	}
132	if (debug==2) {
		write (6,*) 'final weighted n:',xn(1)
		write (6,*) '---------------------------'
	} else if (debug==1) {
		write (6,120) xn(1)
120	format (' wn=',f9.6)
	} else if (debug==3) {
		write (6,420) xn(1)
420		format('final weighted n',t30,f5.3/,79('-'))
	}
	return
	end

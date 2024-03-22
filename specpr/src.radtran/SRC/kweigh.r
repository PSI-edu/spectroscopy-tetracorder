	subroutine kweigh(xk,nminer,r,debug,rt,xn,chan,iter)
	implicit integer*4 (i-q)

	real*4 xk(nminer),wk(9),r(nminer),top,bot,total,rt(nminer),xn(nminer)
	integer*4 debug,chan

	character*1 icrrt

	icrrt=char(13)   # carriage return character

	top = 0
	bot = 0

	if (debug==3) {
		write (6,400) chan,iter
400		format (//1x,'Channel=',i3,t19,'|K|',t34,'n',t46,'weight',
			t62,'real r',t73,'rx'/,1x,'Iteration=',i2)
	}
	do j=1,nminer {
		if (r(j)==1) {
			xk(1)=0
			return
		}
#		wk(j)=((abs(r(j)-0.5)+0.5)**(-4))-1
#		wk(j)=1
		wk(j)=((abs(r(j)-0.5)+0.5)**(-2))-1

	if (debug==2) {
		write (6,*) 'weight of k:',j,' - ',wk(j)
	} else if (debug==3) {
		write (6,410) xk(j),xn(j),wk(j),r(j),rt(j)
410		format (t15,f9.4,t30,f5.3,t45,f7.4,t60,f7.6,t70,f7.6)
	}
		top=top+wk(j)*xk(j)
		bot=bot+wk(j)
	}
	xk(1)=top/bot
	if (debug==2) {
		write (6,*) 'final weighted k:',xk(1)
		write (6,*) '---------------------------'
	} else if (debug==1) {
		write (6,120) icrrt, xk(1)
%120		format (a,' wk=',f10.5,$)
	} else if (debug==3) {
		write (6,420) xk(1)
420		format('final k',t15,f9.4/)
	}
	return
	end

	subroutine iplot
	implicit integer*4(i-n)
#ccc  version date: 06/01/83
#ccc  author(s): roger clark & jeff hoover
#ccc  language:  fortran
#ccc
#ccc  short description:
#ccc         this subroutine ?
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc          crtin,er,dread,erored,sb,ma,va,wjfren
#ccc  argument list description:
#ccc     arguments: none
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  notes:
#ccc


	include "../common/spmaxes"   # max parameters, must be first

	include "../common/blank"
	include "../common/label1"
	include "../common/lbl4"
	include "../common/lblg"
	include "../common/xtnct"
	include "../common/alphabet"
	include "../common/lundefs"

	integer*4 ier, jchar
	character*80 outline

integer*4 ichp(35)
real*4    y(5),coef(SPMAXCHAN*2)
equivalence (coef(1),datab(1))

#data ichp/'1','2','3','4','5','6','7','8','9',
#		  'a','b','c','d','e','f','g','h','i',
#		  'j','k','l','m','n','o','p','q','r',
#		  's','t','u','v','w','x','y','z'/
ichp(1) = ihchar('1')
ichp(2) = ihchar('2')
ichp(3) = ihchar('3')
ichp(4) = ihchar('4')
ichp(5) = ihchar('5')
ichp(6) = ihchar('6')
ichp(7) = ihchar('7')
ichp(8) = ihchar('8')
ichp(9) = ihchar('9')
ichp(10) = ihchar('a')
ichp(11) = ihchar('b')
ichp(12) = ihchar('c')
ichp(13) = ihchar('d')
ichp(14) = ihchar('e')
ichp(15) = ihchar('f')
ichp(16) = ihchar('g')
ichp(17) = ihchar('h')
ichp(18) = ihchar('i')
ichp(19) = ihchar('j')
ichp(20) = ihchar('k')
ichp(21) = ihchar('l')
ichp(22) = ihchar('m')
ichp(23) = ihchar('n')
ichp(24) = ihchar('o')
ichp(25) = ihchar('p')
ichp(26) = ihchar('q')
ichp(27) = ihchar('r')
ichp(28) = ihchar('s')
ichp(29) = ihchar('t')
ichp(30) = ihchar('u')
ichp(31) = ihchar('v')
ichp(32) = ihchar('w')
ichp(33) = ihchar('x')
ichp(34) = ihchar('y')
ichp(35) = ihchar('z')

# RED Initalize to 0
m=0

repeat {
    write(ttyout,40)
    d = 1.0
    call crtin
    i = 1
    call wjfren(i,x,il)
    if (il==ihe) break 1
    if (il==ihd) {
		call er
		write(ttyout,100)
		call crtin
		ii = 1
		call wjfren(ii,x,jchar)
		if (jchar!=ihr) go to 10
		repeat {
			call er
			write(ttyout,150)
			do i = 1,20
				write(ttyout,160)i,(idel(i,j),j = 1,11)
			repeat {
				write(ttyout,170)
				call crtin
				ich = 1
				call wjfren(ich,x,il)
				if (il==ihd) {
					call er
					go to 10
				}
				if (il==0) {
					if (x<=0) go to 30
					if (x<=20) {
						i = x
						call wjfren(ich,x,il)
						if (il==0)
							break 1
					}
				}
			}
			if (x<=0) x = 0
			if (x>4864) x = 0
			j = x
			if (j==0) idel(i,1) = 0
			if (j!=0)
			do n = 2,11
				if (idel(i,n)==j) {
					idel(i,n) = 0
					idel(i,1) = idel(i,1)-1
				}
			write(ttyout,180)
			call crtin
			i = 1
			call wjfren(i,xi,jchar)
			if (jchar==ihd) next 1
			go to 30
10			repeat {
				write(ttyout,110)
				call crtin
				ll = 1
				call wjfren(ll,x,il)
				if (il==0) {
					if (x<=0 || x>20) go to 950
					i = x
					call wjfren(ll,x,il)
					if (il==0) {
						if (x<=0) x = 0
						if (x>10) x = 10
						j = x
						if (j==0) {
							idel(i,1) = -1
							go to 20
						}
						write(ttyout,120)j
						do n = 1,j
							if (idel(i,(n+1))==0) {
								repeat {
									call crtin
									ll = 1
									call wjfren(ll,x,il)
									if (il==ihe||il==ihx) go to 920
									if (il!=0) write(ttyout,130)
								} until(il==0)
								if (x<=0) x = 0
								if (x>4864) x = 0
								m = x
								idel(i,n+1) = m
							} else if (n+1==11) write(ttyout,140)i
						break 1
					}
				}
920                             continue
			}
			idel(i,1) = idel(i,1)+j
20			write(ttyout,180)
			call crtin
			ii = 1
			call wjfren(ii,x,jchar)
			if (jchar!=ihd) break 1
		}
		if (j!=0) j = m
#**************************************************************
#
#     redo extinction calculations
#
#**************************************************************
30		do i = 1,nchans
		call linfit(i)

		if (j==0) next 1
		ichan = j
    } else {
		if (il!=0) next 1
		ichan = x
		if (ichan<1||ichan>nchans) next 1
    }
    call er
    n = 0
    x = -100000.
    xx = 100000.
    xl = 100000.
    xr = -100000.
    do ii = 1,nfile {
	read(addlun,rec=ii+1,iostat=ier)data
	call ertyp('iplot',addlun,ier)
	if (data(ichan)!=100000.) {
		if (idel(ii,1)!=-1) {
			do i = 2,11 {
				if (idel(ii,i)==ichan) go to 940
			}
			if (data(ichan)>=xr) xr = data(ichan)
			if (data(ichan)<=xl) xl = data(ichan)
			if (air(ii)>=x) x = air(ii)
			if (air(ii)<=xx) xx = air(ii)
		}
	}
940   continue
    }
    x = x*1.01
    xx = xx*0.99
    xb = xx
    xt = x
    if (n==0) {
		x = xr
		xx = xl
		x = x*1.01
		xx = xx*0.99
		if (x<0) x = x*0.99/1.01
		if (xx<0) xx = xx*1.01/0.99
    }
    repeat {
		diff = x-xx
		y(1) = x-diff
		y(2) = x-diff*.75
		y(3) = x-diff*.50
		y(4) = x-diff*.25
		y(5) = x
		if (x<=xx) x = xx
		if (diff<0.005) x = x+0.01
		if (diff>=0.005) {
			if (n==0) u = x
			if (n==0) d = diff
			if (n==20) break 1
			call ma(62*2,70*2)
			call sb(0)
			write(outline,50) y
			call gwrite(outline)	 #write horizontal axis labels
			n = 20
			x = xt
			xx = xb
		}
    }
    call ma(10*2,610*2)				#write vertical axis labels
    call sb(0)
    do i = 1,5 {
	write(outline,60) y(6-i),char(0)
	call gwrite(outline)
	do xtmpj = 1,5 {
		write(outline,61)char(0)
		call gwrite(outline)
	}
    }
    call ma(100*2,630*2)
    call sb(0)
    write(outline,70)ichan
    call gwrite(outline)
#
#     plot graph boundaries
#
    call ma(100*2,95*2)
    call va(100*2,600*2)
    call ma(95*2,600*2)
    call va(940*2,600*2)
    call va(940*2,95*2)
    call ma(940*2,100*2)
    call va(95*2,100*2)
    do i = 1,3 {
	iy = 100*2+125*2*i
	call ma(95*2,iy)
	call va(100*2,iy)
    }
    do i = 1,3 {
	iy = 100*2+210*2*i
	call ma(iy,95*2)
	call va(iy,100*2)
    }
    call ma(100*2,100*2)
    do index = 1,nfile {
	do i = 2,11 {
		if (idel(index,i)==ichan) go to 950
	}
	if (idel(index,1)!=-1) {
		read(addlun,rec=index+1,iostat=ier)data
		call ertyp('iplot',addlun,ier)
		if (data(ichan)!=100000.) {
			if (abs(diff)<0.1e-15) diff = 0.1e-15
			if (abs(d)<0.1e-15) d = 0.1e-15
			yii = ((air(index)-xx)/diff)*2*500.
			xii = (((data(ichan))-(u-d))/d)*2*840.
			if (xii>32500.) xii = 32500.
			if (yii>32500.) yii = 32500.
			if (xii<(-32500.)) xii = -32500.
			if (yii<(-32500.)) yii = -32500.
			iix = xii+0.5
			iiy = yii+0.5
#
#     plot the character
#
			call ma(96*2+iix,110*2+iiy)
			call sb(0)
			kchar = ichp(index)
			write(outline,80)kchar,char(0)
			call gwrite(outline)
		}
	}
950   continue
    }
#
#     plot the line
#
    i = 0
#
#     bx= (slope)*(lower airmass) + (intercept)
#
    bx = coef(ichan)*xx+coef(4864+ichan)
    repeat {
		iiy = 100
		if (i==1) iiy = 600
#
#     u= upper bound of x-axis (log i axis)
#     d= difference: upper-lower bound for x axis
#     diff= difference: upper-lower bound for y axis
#
		if (bx>u) {
			bx = u
			if (abs(diff)<0.1e-15) diff = 0.1e-15
			if (abs(coef(ichan))<0.1e-15) coef(ichan) = 0.1e-15
			iiy = ((((u-coef(4864+ichan))/coef(ichan))-xx)/diff)*2*500.
			iiy = iiy+100*2
		} else if (bx<u-d) {
			bx = u-d
			if (abs(diff)<0.1e-15) diff = 0.1e-15
			if (abs(d)<0.1e-15) d = 0.1e-15
			if (abs(coef(ichan))<0.1e-15) coef(ichan) = 0.1e-15
			iiy = ((((u-d-coef(4864+ichan))/coef(ichan))-xx)/diff)*2*500.
			iiy = iiy+100*2
		}
		iix = ((bx-u+d)/d)*840.*2.0
		if (i==1) break 1
		call ma(100*2+iix,iiy)
		bx = coef(ichan)*x+coef(4864+ichan)
		i = 1
    }
    call va(iix+100*2,iiy)
    call ma(0,750*2)
    call sb(0)
    write(outline,90)ititl1(17:40)
    call gwrite(outline)
    write(outline,61)char(0)	# Blank line
    call gwrite(outline)
}
call er
return

40  format(' type in channel number'/,
' type d to do deletion or restoration of runs and channels'/,
' type e to exit routine'/)

50  format(1x,f8.5,4(7x,f8.5))

60  format(2x,f4.2,a1)
61  format(1x,a1)

70  format(' airmass vs log intensity for channel ',i3)

80  format(1x,a1,a1)

90  format(' title:  ',a)

100  format(' type d for deletion, type r for restoration'/)

110  format(' to delete a run type the run number and 0'/,
' to delete channels type run number and number of channels',/,
' press return to exit',/)

120  format(' type in ',i2,' channel numbers',5x,'1 channel per line',/)

130  format(' illegal entry - retype',/)

140  format(' 10 channels have been deleted from run ',i2/)

150  format(' runs and channels that have been deleted'/,
' run  channel numbers, if number is 0 that channel is unchanged-------'//,
' run   del   1    2    3    4    5    6    7    8    9    10',/,
1x,72('-'),/)

160  format(2x,i2,11(2x,i3))

170  format(' to restore a run type in run number and 0'/,
' to restore a channel type in run and channel number',/,
' type d for deletions',/,' press return to do extinction calculations again',/)

180  format(' to display status again type d',/,
' or press return to do extinction calculations again',/)

end

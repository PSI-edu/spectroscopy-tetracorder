    subroutine extplt
    implicit integer*4(i-n)
#ccc  version date: 06/01/83
#ccc  author(s): roger clark & jeff hoover
#ccc  language:  fortran
#ccc
#ccc  short description:
#ccc         this subroutine does extintion plots.
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc          er,ma,sb, alplty,chplta,crtpltmaw,vawcrtin,wjfren
#ccc          dread,erored,lprcpt
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
	include "../common/lbl7"
	include "../common/lbl6"
	include "../common/lbl4"
	include "../common/label1"
	include "../common/lbl3"
	include "../common/lblg"
	include "../common/xtnct"
	include "../common/alphabet"
	include "../common/lundefs"
	include "../common/dscrch"

	integer*4 ier, jchar

real*4 coef(14592),chans(4864)
real*4 wav(4864)
equivalence (coef(1),dataa(1)),(chans(1),wav(1),datsc1(1))



n = 4864
repeat {
    call er
    exu = -100000.

    do i = 1,nchans
		if (coef(i+n)>exu) exu = coef(i+n)
    exl = 100000.

    do i = 1,nchans
		if (coef(i+n)<exl) exl = coef(i+n)

    repeat {
		diff = exu-exl
		if (exu<exl) exl = exu
		if (diff<=0.001) exu = exu+0.001
		if (diff>0.001) {
			call er
			call ma(100*2,630*2)
			call sb(0)
			if (n==0) write(ttyout,10)
			if (n==120) write(ttyout,20)
			if (n==240) write(ttyout,30)
			do i = 1,nchans {
				data(i) = coef(i+n)
				error(i) = 0.0
			}
	#
			call alplty(exl,exu)
			call chplta(nchans,xmax,xmin,chans,iline)
			iline = 1
			call crtplt(nchans,xmax,xmin,exl,diff,data,chans,iline)
			i2 = nchans/10
			do i = 1,i2 {
				xi = i
				an = nchans
				x = 112.0+xi*(10.0*(1000.0-112.0)/an)+0.5
				ix = x
				call movabs(ix,46*2)
				call drwabs(ix,276*2)
			}
	#
			y0 = ((-1.*exl)/diff)*1000.+200.+0.5
			if (y0<= 1200 && y0>=200) {
				iy = y0+0.5
				call maw(112*2,iy)
				call vaw(1003*2,iy)
			}
			call ma(0,760*2)
			call sb(0)
			if (n==0) write(ttyout,40)ititl1
			if (n==4864) write(ttyout,40)ititl2
			if (n==4864*2) write(ttyout,40)ititle
			write(ttyout,50)ihista,ihistb,ihistc
			call ma(0,700*2)
			call sb(0)
			call crtin
			i = 1
			call wjfren(i,x,jchar)
			if (jchar==ihx || jchar==ihe) {
				call er
				go to 900
			}
			if (jchar==ihg) n = 0
			if (jchar==ihs) n = 4864
			if (jchar==ihi) n = 4864*2
			if (jchar==ihp) {
				do i = 1,nchans {
					error(i) = 0.0
					data(i) = coef(i+n)
				}

				if (n==4864*2) ititl = ititle
				if (n==4864) ititl = ititl2
				if (n==0) ititl = ititl1
				if (n==4864*2) ihist = ihistc
				if (n==4864) ihist = ihistb
				if (n==0) ihist = ihista
				idv1 = ihs
				key = itrol(2)
				read(wavlun,rec=key+1,iostat=ier)wav
				call ertyp('extplt',11,ier)
				call lprpct(1,nchans,exl,exu,wav)
				call er
			} else {
				if (jchar!=ihc) break 1
				call er
				repeat {
					write(ttyout,60)
					call crtin
					i = 1
					call wjfren(i,x,il)
					if (il==0) {
						exl = x
						i = i+1
						call wjfren(i,x,il)
						if (il==0) break 1
					}
				}
				exu = x
				call er
			}
		}
    }
}
900   continue
return

10  format(' goodness of fit plot')

20  format(' extinction plot')

30  format(' intensity plot')

40  format(' type g: goodnes of fit, s: extinction slopes, i: intercepts',/,
' type p to printout plot displayed'/,
' type c to change scale, e to exit routine',//,1x,a)

50  format(' files used  ',a/,13x,a/,13x,a)

60  format(' type in lower and upper bounds free format'/)

end

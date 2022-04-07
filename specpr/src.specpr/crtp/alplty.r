	subroutine alplty(lbnd, ubnd)
	implicit integer*4 (i-n)

#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc         This subroutine draws the box and tick marks
#ccc         along the vertical axis of the crt plot.
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc          ticks,signif,movabs,drwabs,sb
#ccc  argument list description:
#ccc     arguments: lbnd,ubnd
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc

	include "../common/lundefs"

	character*12    ilabel(6),itmp
	real*4 lbnd,label(6),lincr,lstrt
	integer*4 fnb,lnb

	character*80 outline 	# for X-window writes
#
#       draw box
#
#	axl = 56.   # original standard size
#	axh = 500.
#	ayl = 46.
#	ayh = 276.

	axl = 112.   # 2x size
	axh = 1000.
	ayl = 92.
	ayh = 552.

	iayl = ayl
	iayh = ayh
	iaxl = axl
	iaxh = axh
	call movabs (iaxl,iayl)
	call drwabs (iaxl,iayh)
	call drwabs (iaxh,iayh)
	call drwabs (iaxh,iayl)
	call drwabs (iaxl,iayl)
	diff= ubnd-lbnd
	if (diff < 0.1e-15) lbnd=0.0
	if (diff < 0.1e-15) ubnd=1.0
	diff= ubnd-lbnd
#
#     determine nice tick marks
#
	call ticks(lbnd,ubnd,label,lstrt,lincr,tincr)
	call signif(label,ilabel)
#
#     x1 = first nice tick mark
#
	x= lstrt-lincr

	do i = 1,6 {
		x= x+ lincr
		if (x > ubnd+0.0005*ubnd) next
		ax= ((x-lbnd)/diff) * (ayh-ayl) + ayl
		if (ax < ayl) ax= ayl
		if (ax > ayh) ax= ayh
		iax= ax +0.5
		#call movabs (iaxl, iax)   moved to after labels 8/16/2011
		#call drwabs (iaxl+5, iax)
		#call movabs (iaxh, iax)
		#call drwabs (iaxh-5, iax)
		call movabs (0, iax-5)
		call sb (0)
		if (label(i) != -1.23e34) {
			ifnb = fnb(ilabel(i))
			ilnb = lnb(ilabel(i))
			if (ifnb < 1) ifnb = 1
			if (ilnb < ifnb) ilnb = ifnb
			ispc=ilnb-ifnb+1   # number of spaces occupied
			if (ispc < 8) {  # right justify to space 8
				itmp(1:ispc) = ilabel(i)(ifnb:ilnb)
				ilabel(i) = '            '
				ilabel(i)(9-ispc:8)=itmp(1:ispc)
				ifnb=1
				ilnb=8
			}
			write(outline,128) ilabel(i)(ifnb:ilnb),char(0)
			call gwrite(outline)
		}
		call movabs (iaxl, iax)
		call drwabs (iaxl+5, iax)
		call movabs (iaxh, iax)
		call drwabs (iaxh-5, iax)

		# repaint veritcle line because some x-windows sizes the text overwrite the line
		#   RNC 08/16/2011
		call movabs (iaxl,iayl)
		call drwabs (iaxl,iayh)
	}
	x = lstrt-tincr
	while (x >= lbnd) {
		ax= ((x-lbnd)/diff)*(ayh-ayl) + ayl
		iax = ax + .5
		call movabs(iaxl,iax)
		call drwabs(iaxl+2,iax)
		call movabs(iaxh,iax)
		call drwabs(iaxh-2,iax)
		x=x-tincr
	}
	x = lstrt+tincr
	while(x <= ubnd) {
		ax= ((x-lbnd)/diff)*(ayh-ayl) +ayl
		iax = ax+.5
		call movabs(iaxl,iax)
		call drwabs(iaxl+2,iax)
		call movabs(iaxh,iax)
		call drwabs(iaxh-2,iax)
		x = x+tincr
	}

128	format(a,a1)
	return
	end

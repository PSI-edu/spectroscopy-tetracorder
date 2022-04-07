	subroutine pltxtx(wvmax,wvmin)
	implicit integer*4 (i-n)

#ccc  version date: 08/06/87
#ccc  author(s): Noel Gorelick
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc         This subroutine plots the tick marks and the
#ccc         horizontal label.
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc          ticks,signif,movabs,drwabs,sb
#ccc  argument list description:
#ccc     arguments: wvmax,wvmin
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc

	include "../common/spmaxes"   # max parameters, must be first

	include "../common/blank"
	include "../common/label1"
	include "../common/lblwav"
	include "../common/lbl4"
	include "../common/lbl7"
	include "../common/lundefs"
	include "../common/alphabet"

	real*4            lincr,lstrt,label(6)
	character*12    ilabel(6)
	character*74	hlabel
	integer*4		fnb,lnb

	character*1 ihbcksl

	ihbcksl = char(92)  # this is the backslash character

	axh = 500.
	axl = 56.
	ayl = 46.
	iayl= ayl

#
#     determine max and min wavelengths
#

	wdiff= wvmax- wvmin
	if (wdiff<0.00001) {
		wvmin= 0.1
		wvmax= 1.0
		wdiff= 0.9
	}
#
#     determine nice tick interval
#
	call ticks(wvmin,wvmax,label,lstrt,lincr,tincr)
	call signif(label,ilabel)

#
#     determine amount to shift labels to center under ticks
#
	x = amax1(abs(wvmin),abs(wvmax))
	ishift = int(abs(alog10(x))) * 7 + 14

#
#     draw ticks and labels
#
	x= lstrt-lincr
	do i= 1,6 {
		x= x + lincr
		if (x > wvmax) next
		ax= ((x-wvmin)/wdiff) * (axh-axl) + axl
		if (ax < axl) ax= axl
		if (ax > axh) ax= axh
		iax= ax + 0.5
		call movabs(iax, iayl)
		call drwabs(iax, iayl-4)
		call movabs (iax-ishift,iayl-18)
		call sb(0)
		ifnb = fnb(ilabel(i))
		ilnb = lnb(ilabel(i))
		if (ifnb > ilnb) ifnb = ilnb  # occurs when all blank
		write(ttyout,128) ilabel(i)(ifnb:ilnb)
	}
	x = lstrt-tincr
	while (x >= wvmin) {
		ax= ((x-wvmin)/wdiff)*(axh-axl) + axl
		iax = ax + .5
		call movabs(iax,iayl)
		call drwabs(iax,iayl-2)
		x=x-tincr
	}
	x = lstrt+tincr
	while(x <= wvmax) {
		ax= ((x-wvmin)/wdiff)*(axh-axl) + axl
		iax = ax+.5
		call movabs(iax,iayl)
		call drwabs(iax,iayl-2)
		x = x+tincr
	}
#
# determine horizontal axis label (default= Wavelength (microns))
#
	if (itrol(1) == ihcc) {
		hlabel = 'Channel Number'
	} else {
		i=0
		do j = 223, 294 {
			if (mwhist(j:j+1) == ihbcksl // 'W') {
				i = j+2
				break
			}
		}
		if (i
==0) {
			hlabel = 'Wavelength (microns)'
		} else {
			do k = 296, i+1, -1 {
				if (mwhist(k:k) != ' ') break
			}
			hlabel = mwhist(i:k)
		}
	}
	call movabs(185, 17)
	call sb(0)
	write(ttyout, 128) hlabel
	return
128	format (a)
	end

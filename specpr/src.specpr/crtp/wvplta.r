	subroutine wvplta (nchans, wvmax, wvmin,iline)
	implicit integer*4 (i-n)

#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc         This subroutine plots the tick marks and the
#ccc         horizontal label when plotting is done in
#ccc         wavelength.
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc          ticks,signif,movabs,drwabs,sb
#ccc  argument list description:
#ccc     arguments: nchans,wvmax,wvmin,iline
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc
#################################################################
#                                                               #
#       this routine plots the tick marks and the horizontal    #
#       label when plotting is done in wavelength               #
#                                                               #
#       Arguments:                                              #
#        nchans         input   number of channels in the       #
#                               wavelength file                 #
#        wvmax          output  maximum wavelength limit        #
#                               for plotting                    #
#        wvmin          output  minimum wavelength limit        #
#                               for plotting                    #
#        iline          ?????   unused!!!                       #
#                                                               #
#       Author: Roger Clark                                     #
#                                                               #
#       Modified:       JAH     03-02-83  fix tic labels        #
#                                   due to signif modification  #
#                                                               #
#################################################################

	include "../common/spmaxes"   # max parameters, must be first

	include "../common/blank"
	include "../common/label1"
	include "../common/lblwav"
	include "../common/lbl4"
	include "../common/lbl7"
	include "../common/hptrm"
	include "../common/lundefs"
	include "../common/wavemarks"
	include "../common/alphabet"

	real*4            lincr,lstrt,label(6)
	real*4		axl, axh, ayl, ayh
	character*12    ilabel(6)
	character*74	hlabel
	integer*4	fnb,lnb, j, iayl, iayh
	integer*4       iwm        # wavelength marker number

	character*80	outline	# for X-window writes

	character*1 ihbcksl

	ihbcksl = char(92)  # this is the backslash character

#	axl = 56.   # original standard size
#	axh = 500.
#	ayl = 46.
#	ayh = 276.

	axl = 112.   # 2x size
	axh = 1000.
	ayl = 92.
	ayh = 552.

	iayl= ayl
	iayh= ayh

#
#     determine max and min wavelengths
#

	if (wmaxa>wmina) {
		wvmax= wmaxa
		wvmin= wmina
	} else {
		for (i=1; i<nchans & dataa(i)==-1.23e34; i=i+1)
			;
		wvmax = dataa(i)
		wvmin = dataa(i)
		do i= 1, nchans {
			if (dataa(i)!=-1.23e34) {
				if (dataa(i)>wvmax) wvmax= dataa(i)
				if (dataa(i)<wvmin) wvmin= dataa(i)
			}
		}
	}
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
		call movabs (iax-ishift+8,iayl-36)
		call sb(0)
		ifnb = fnb(ilabel(i))
		ilnb = lnb(ilabel(i))
		if (ifnb > ilnb) ifnb = ilnb  # occurs when all blank
		write(outline,128) ilabel(i)(ifnb:ilnb),char(0)
		call gwrite(outline)
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
		if (i==0) {
			hlabel = 'Wavelength (microns)'
		} else {
			do k = 296, i+1, -1 {
				if (mwhist(k:k) != ' ') break
			}
			hlabel = mwhist(i:k)
		}
	}
	call movabs(370, 34)
	call sb(0)
	write(outline, 128) hlabel, char(0)
	call gwrite(outline)

# now do wavelength markers
	#       wmrflg  = -1 not defined  
	#               = 0 do not show marks
	#               = 1 show marks
	#       wmrflgb = stored setting of wmrflg when the mark gets turned off
	#                 when turned back on, wmrflg(n) = wmrflgb(n)
	#       wmrclr  =     color (to be defined, future)
	#       wmrwav  = wavelength markers (up to 9)
	#       wmnum   = number of wavelength markers (up to 9)
	#       wmops   = mark options

	for (iwm=1; iwm<=6; iwm=iwm+1) {

	   if (wmrflg(iwm) == 1) {  # wavelength marker on

		if (igrmod >= 50 && igrmod <= 53) {  # set line color
#XWIN 			call xset_color(iwm+1)   # 2=red, 3=blue, 4=green, 5=orange, 6=magenta
		}

		# draw wavelength marker line from bottom to top of plot

		for (j=1; j<=wmnum(iwm); j=j+1) {  # up to 9 wavelength markers

			x=wmrwav(j,iwm)
			if (x > wvmin && x < wvmax) {  # marker is within range of plot
				ax=((x-wvmin)/wdiff)*(axh-axl) + axl
				iax = ax+.5
				call movabs(iax,iayl)
				call drwabs(iax,iayh)
			}
		}

		if (igrmod >= 50 && igrmod <= 53) {
#XWIN 			call xset_color(0)   # black
		}
	   }
	}
#       end of wavelength markers

	return
128	format (a,a1)
	end

      subroutine xplta (inv,nchans, wvmax, wvmin)
	  implicit integer*4 (i-n)
      dimension label(6)
      logical inv
	integer*4 fnb, lnb
      character*12 ilabel(6)
      real label,lincr,lstrt
      data ihl  /1hl/
      axh = 500.
      axl = 56.
      ayl = 46.
      iayl= ayl
#
%      write(6,9876)
9876    format(' entering wvplta....')
#     determine whether to connect points
#
#

#     determine max and min waveleng
122   wdiff= wvmax- wvmin
      if (wdiff.lt.0.00001) wvmin= 0.1
      if (wdiff.lt.0.00001) wvmax= 1.0
      wdiff= wvmax- wvmin
#
#     determine nice tick interval
#
      call ticks(wvmin,wvmax,label,lstrt,lincr,tincr)
      call signif(label,ilabel)
#
#     x1= first nice tick mark
      x=amax1(abs(wvmin),abs(wvmax))
	  ishift = int(abs(alog10(x)))*7+14
#
      x= lstrt-lincr
      do i= 1,6 {
      x= x + lincr
      if (x.gt.wvmax) next
      ax= ((x-wvmin)/wdiff) * (axh-axl) + axl
      if (ax.lt.axl) ax= axl
      if (ax.gt.axh) ax= axh
      iax= ax + 0.5
      call movabs(iax, iayl)
      call drwabs(iax, iayl-4)
      call movabs (iax-ishift,iayl-18)
      call sb(0)
      write(6,128) ilabel(i)(fnb(ilabel(i)):lnb(ilabel(i)))
128   format (a)
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
	ax= ((x-wvmin)/wdiff)*(axh-axl) +axl
	iax = ax+.5
	call movabs(iax,iayl)
	call drwabs(iax,iayl-2)
	x = x+tincr
      }
      call movabs(190, 17)
      call sb(0)
      if (!inv) write (6, 250)
	else {
	write(6,251)
	}
250   format (/ , 'wavelength space (microns)')
251   format (/ , 'inverse wavelength space')
      return
      end

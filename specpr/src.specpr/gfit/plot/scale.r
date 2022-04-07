      subroutine scale (bbnd,ubnd,nchans,datac,error,wmina,wmaxa,i)
	  implicit integer*4 (i-n)

      dimension datac(256),error(256)

      data iha,ihn,ihw /1ha, 1hn, 1hw/
      data ihc /1hc/
      data ihx, ihe /1hx,1he/

      call er
6     if (wmaxa.le.wmina) write (6,62) bbnd,ubnd
62    format (' scale: verticle=', 1pe11.4,', ', 1pe11.4, 2x,
	     'horiz.= automatic')
      if (wmaxa.gt.wmina) write (6,61) bbnd,ubnd,wmina,wmaxa
61    format (' scale: verticle=',1pe11.4, ', ', 1pe11.4, 2x,
	     'horiz.=' 1pe11.4, ', ', 1pe11.4)
      write (6, 3)
3     format(/,
 ' specify lower bound and upper bound intensities verticle axis, or:',/,
 ' type  a  to auto scale (the verticle axis), or:', /,
 ' type  w  and left and right hand wavelengths limits', /,
 '           (if you type  w  only,  the program will find the limits ', /,
 '           from the current wavelength file)', /)

      call crtin
      j= 1
#
#     decode lower and upper bound
#
      call wjfren(j, xx, il)
      if (j.ge.80) go to 4
      if (il.eq.ihn) go to 4
      if (il.eq.ihw) go to 19
      if (il.eq.iha) {
	bbnd = 0.
	ubnd = 0.
	call er
	return
      }
      if (il.ne.0) go to 16
      bbnd=xx
      call wjfren (j, xx, il)
      if (j.ge.80) go to 4
      if (il.ne.0) go to 16
      ubnd=xx
#
      call er
      igo=0
      return
#
19    call wjfren (j,xx,il)
      if (il.ne.0) go to 16
      call wjfren (j,yy,il)
      if (il.ne.0) go to 16
      if (yy.lt.xx) go to 16
      if ((yy.eq.xx).and.(xx.ne.0.)) go to 16
      wmina=xx
      wmaxa=yy
      go to 6
4     call er
      return
7     igo=7
      return
16    write (6,17)
17    format (1x, '** error: reenter')
      go to 6
      end

C Output from Public domain Ratfor, version 1.04
      subroutine bdmset (wav,rflib,cl1,cl2,cr1,cr2, rflibc,minch,maxch,i
     *feattype,ier)
      implicit integer*4 (i-n)
      integer*4 ttyout, imod12
      integer*4 cl1,cl2,cr1,cr2,ier,minch,maxch,ifeattype
      real*4 wav(cr2), rflib(cr2)
      real*4 rflibc(cr2)
      ttyout = 6
      minch=0
      maxch=0
      imod12 = 0
      if(ier .eq. 12)then
      imod12 = 12
      endif
      ier = 0
      if(cl1 .gt. cl2)then
      write (ttyout, 100)
100   format (' ERROR: left continuum segment point 1 is', ' greater tha
     *n point 2')
      ier = 1
      endif
      if(cr1 .gt. cr2)then
      write (ttyout, 101)
101   format (' ERROR: right continuum segment point 1 is', ' greater th
     *an point 2')
      ier = 1
      endif
      if(cl2 + 1 .gt. cr1 - 1)then
      write (ttyout, 102)
102   format (' ERROR: left continuum segment point 2 + 1 ', 'channel is
     * greater than ',/, '        right continuum segment point 1 - 1 ch
     *annel',/, '        This means there are no channels for', ' the ac
     *tual absorption band' )
      ier = 1
      endif
      if(ier .eq. 1)then
      return
      endif
      delpt = -1.23e34
      avlc = 0.0
      avwlc = 0.0
      n = 0
      do23010 i = cl1, cl2 
      if(wav(i) .eq. delpt .or. rflib(i) .eq. delpt)then
      goto 23010
      endif
      avlc = avlc + rflib(i)
      avwlc = avwlc + wav(i)
      n = n +1
23010 continue
23011 continue
      if(n .lt. 1)then
      if(imod12 .eq. 12)then
      write (ttyout,119)
119   format (9x,'WARNING: all points in reference spectrum,', ' left co
     *ntinuum deleted.')
      ier = 12
      return
      else
      write (ttyout,120)
120   format (' ERROR: all points in reference spectrum,', ' left contin
     *uum deleted.')
      ier = 1
      return
      endif
      endif
      avlc = avlc / float(n)
      avwlc = avwlc / float(n)
      avrc = 0.0
      avwrc = 0.0
      n = 0
      do23018 i = cr1, cr2 
      if(wav(i) .eq. delpt .or. rflib(i) .eq. delpt)then
      goto 23018
      endif
      avrc = avrc + rflib(i)
      avwrc = avwrc + wav(i)
      n = n +1
23018 continue
23019 continue
      if(n .lt. 1)then
      write (ttyout,121)
121   format (' ERROR: all points in reference spectrum,', ' right conti
     *nuum deleted.')
      ier = 1
      return
      endif
      avrc = avrc / float(n)
      avwrc = avwrc / float(n)
      bottom = avwrc - avwlc
      if(abs(bottom) .lt. 0.1e-20)then
      write (ttyout, 130)
130   format (' ERROR: wavelength range of continuum is',' too small!')
      ier = 1
      return
      endif
      a = (avrc - avlc)/bottom
      b = avrc - a * avwrc
      if(cl1 .gt. 1)then
      do23028 i = 1, cl1-1 
      rflibc(i) = delpt
23028 continue
23029 continue
      endif
      do23030 i = cl1, cr2 
      if(wav(i) .eq. delpt .or. rflib(i) .eq. delpt)then
      rflibc(i) = delpt
      else
      contin = a * wav(i) + b
      if(abs(contin) .gt. 0.1e-20)then
      rflibc(i) = rflib(i) / contin
      else
      rflibc(i) = delpt
      endif
      endif
23030 continue
23031 continue
      il = cl2 + 1
      ir = cr1 - 1
195   if(rflibc(il) .eq. -1.23e+34)then
      il = il + 1
      if(il .gt. ir)then
      write (ttyout, 196)
196   format (' ERROR in band map setup:',/, '       can not find a min 
     *or max: ', 'all channels deleted')
      ier = 1
      return
      endif
      go to 195
      endif
      rmin = rflibc(il)
      rmax = rmin
      minch = il
      maxch = il
      do23040 i = il, ir 
      if(rflibc(i) .eq. delpt)then
      goto 23040
      endif
      if(rflibc(i) .lt. rmin)then
      minch = i
      rmin = rflibc(i)
      endif
      if(rflibc(i) .gt. rmax)then
      maxch = i
      rmax = rflibc(i)
      endif
23040 continue
23041 continue
      depth = 1.0 - rmin
      emiss = rmax - 1.0
      if(rmin .lt. 0.0)then
      write (ttyout, 200)
200   format (' ERROR: band depth in the library spectrum ',/, '        
     *is LESS THAN ZERO: that is invalid')
      ier = 1
      return
      endif
      ifeattype = 0
      if(emiss .gt. depth)then
      write (ttyout, 201)
201   format (' NOTE: this feature is an emission feature.')
      ifeattype = -1
      else
      ifeattype = 1
      endif
      if(ifeattype .eq. -1 .and. maxch .eq. 0)then
      write (ttyout,303) maxch
303   format ('ERROR: emission feature maximum channel=', i7)
      ier = 1
      return
      endif
      if(ifeattype .eq. 1 .and. minch .eq. 0)then
      write (ttyout,304) minch
304   format ('ERROR: absorption feature minimum channel=', i7)
      ier = 1
      return
      endif
      return
      end

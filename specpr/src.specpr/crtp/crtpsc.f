C Output from Public domain Ratfor, version 1.04
      subroutine crtpsc (igo,bbnd,ubnd,nchans,datac,error,wmina,wmaxa,i,
     *dataa,iscale)
      implicit integer*4 (i-n)
      integer*4 spmaxchan
      integer*4 spmaxtext
      integer*4 spmaxrec
      integer*4 spmaxconrec
      integer*4 spmaxaddrecl
      integer*4 imax3pt
      integer*4 imaxtet
      integer*4 maxcline
      parameter (spmaxrec=999999)
      parameter (spmaxchan=69398)
      parameter (spmaxtext=278000)
      parameter (spmaxconrec=182)
      parameter (spmaxaddrecl=129712)
      parameter (imax3pt=9)
      parameter (imaxtet=9)
      parameter (maxcline=80)
      common /hptrm/ igrmod,ihpout,imode,ixlast,iylast,ipen
      common /hptrm/ iot
      character*80 ihpout
      integer*4 igrmod,imode,ixlast,iylast,ipen,iot
      integer*4 luntxt,ulun,ylun,ttyin,ttyout,dlun,vlun,wlun
      integer*4 addlun,wavlun,lstlun,pltlun,wvhlun
      integer*4 ttllun,cmdlun,slun,rlun,cpylun,wrtlun,redlun
      parameter (luntxt=1)
      parameter (ulun=3)
      parameter (ylun=4)
      parameter (ttyin=5)
      parameter (ttyout=6)
      parameter (dlun=7)
      parameter (vlun=8)
      parameter (wlun=9)
      parameter (addlun=10)
      parameter (redlun=10)
      parameter (wrtlun=11)
      parameter (wavlun=11)
      parameter (lstlun=12)
      parameter (pltlun=13)
      parameter (wvhlun=14)
      parameter (ttllun=15)
      parameter (cmdlun=16)
      parameter (slun=17)
      parameter (rlun=18)
      parameter (cpylun=19)
      common /alpha/ iha,ihb,ihc,ihd,ihe,ihf,ihg,ihh,ihi,ihj, ihk,ihl,ih
     *m,ihn,iho,ihp,ihq,ihr,ihs,iht, ihu,ihv,ihw,ihx,ihy,ihz
      common /alpha/ ihca,ihcb,ihcc,ihcd,ihce,ihcf,ihcg,ihch,ihci,ihcj, 
     *ihck,ihcl,ihcm,ihcn,ihco,ihcp,ihcq,ihcr,ihcs,ihct, ihcu,ihcv,ihcw,
     *ihcx,ihcy,ihcz
      common /digit/ ih0,ih1,ih2,ih3,ih4,ih5,ih6,ih7,ih8,ih9
      common /asciic/ihprd,ihandp
      integer*4 iha,ihb,ihc,ihd,ihe,ihf,ihg,ihh,ihi,ihj, ihk,ihl,ihm,ihn
     *,iho,ihp,ihq,ihr,ihs,iht, ihu,ihv,ihw,ihx,ihy,ihz
      integer*4 ihca,ihcb,ihcc,ihcd,ihce,ihcf,ihcg,ihch,ihci,ihcj, ihck,
     *ihcl,ihcm,ihcn,ihco,ihcp,ihcq,ihcr,ihcs,ihct, ihcu,ihcv,ihcw,ihcx,
     *ihcy,ihcz
      integer*4 ih0,ih1,ih2,ih3,ih4,ih5,ih6,ih7,ih8,ih9
      integer*4 ihprd,ihandp
      common /lbl7/ idevc(4), idvc(2,4), itl, itrol(3)
      character*40 itl
      integer*4 idevc, idvc, itrol
      real*4 datac(spmaxchan),error(spmaxchan),dataa(spmaxchan)
      character*1 iichar
      integer*4 iscale
      inflag=0
      izflag=0
      if(igo .eq. 6)then
      call eralph
      endif
      if(igo .eq. 6)then
      go to 6
      endif
      if(igo .eq. 8)then
      go to 8
      endif
      if(iscale .eq. ihcb)then
      izflag = 1
      endif
      if(iscale .eq. ihca)then
      izflag = 2
      endif
9     bbnd= 0.9e+30
      ubnd= -0.9e+30
      jb=nchans
      ja=1
      icount=0
      xmax = wmaxa
      xmin = wmina
      if(xmax .lt. xmin)then
      xmax = wmina
      xmin = wmaxa
      endif
      if(itrol(3).eq.ihn)then
      if(abs(xmax) .lt. 0.1e-25)then
      xmax = 0.1e-25
      endif
      if(abs(xmin) .lt. 0.1e-25)then
      xmin = 0.1e-25
      endif
      xtmp = xmin
      xmin = 10000.0/xmax
      xmax = 10000.0/xtmp
      endif
      do23018 jmn= ja, jb 
      if(datac(jmn) .eq. -1.23e34)then
      goto 23018
      endif
      if(dataa(jmn).gt.xmax .or. dataa(jmn).lt.xmin)then
      goto 23018
      endif
      xx= datac(jmn)+ error(jmn)
      yy= datac(jmn)- error(jmn)
      if(yy .lt. bbnd)then
      bbnd= yy
      endif
      if(xx .gt. ubnd)then
      ubnd= xx
      endif
      icount = icount+1
23018 continue
23019 continue
      if(icount .lt. 1)then
      bbnd = 0.0
      ubnd = 2.0
      endif
      if(izflag .eq. 1)then
      bbnd = 0.0
      endif
      if(izflag .eq. 2)then
      diff= abs(ubnd-bbnd)
      ubnd = ubnd + 0.02*diff
      bbnd = bbnd - 0.02*diff
      endif
      diff= ubnd-bbnd
      if(diff .lt. 0.0)then
      go to 6
      endif
      if(ubnd .gt. 0.0)then
      ubnd= ubnd*1.02
      else
      ubnd= ubnd*0.98
      endif
      if(bbnd .gt. 0.0)then
      bbnd= bbnd*0.98
      else
      bbnd= bbnd*1.02
      endif
      call er
8     if(i .ne. ihc)then
      go to 7
      endif
6     if(wmaxa .le. wmina)then
      write (ttyout,62) bbnd,ubnd
      else
      write (ttyout,61) bbnd,ubnd,wmina,wmaxa
      endif
      write (ttyout, 3)
      call crtin
      j= 1
      call rlchng(j, xx, il)
      inflag=0
      izflag=0
      if(j.ge.80)then
      go to 4
      endif
      if(il .eq. ihw)then
      go to 19
      endif
      if(il .eq. ihn)then
      inflag=1
      go to 19
      endif
      if(il .eq. ihca)then
      call getiopconchar(j,iichar)
      if(iichar.eq.'0')then
      izflag=1
      endif
      if(iichar.eq.'z')then
      izflag=1
      endif
      if(iichar.eq.'2')then
      izflag=2
      endif
      i= 0
      call hreset(1)
      go to 9
      endif
      if(il .ne. 0)then
      call what(j)
      go to 16
      endif
      bbnd=xx
      call rlchng(j, xx, il)
      if(j.ge.80)then
      go to 4
      endif
      if(il .ne. 0)then
      call what(j)
      go to 16
      endif
      ubnd=xx
      call er
      if(igrmod .ge. 50 .and. igrmod .le. 53)then
      call eralph
      endif
      igo=0
      return
19    call rlchng(j,xx,il)
      if(il .ne. 0)then
      call what(j)
      go to 16
      endif
      call rlchng(j,yy,il)
      if(il .ne. 0)then
      call what(j)
      go to 16
      endif
      if(yy .lt. xx)then
      go to 16
      endif
      if((yy .eq. xx) .and. (xx .ne. 0.))then
      go to 16
      endif
      if(inflag.eq.1)then
      if(xx.ne.0)then
      xx=10000.0/xx
      else
      xx=0
      endif
      if(yy.ne.0)then
      yy=10000.0/yy
      else
      yy=0
      endif
      if(xx.gt.yy)then
      xtmp=xx
      xx=yy
      yy=xtmp
      endif
      endif
      wmina=xx
      wmaxa=yy
      go to 6
4     igo=4
      call er
      if(igrmod .ge. 50 .and. igrmod .le. 53)then
      call eralph
      endif
      return
7     igo=7
      if(igrmod .ge. 50 .and. igrmod .le. 53)then
      call eralph
      endif
      return
16    write (ttyout,17)
      go to 6
17    format (' ** ERROR: reenter')
62    format (' Current Scale: VERTICAL=', 1pe11.4,'  ', 1pe11.4, /, '  
     *              HORIZ.  = Automatic')
61    format (' Current Scale: VERTICAL=',1pe11.4, '  ', 1pe11.4, /, '  
     *              HORIZ.  =',1pe11.4, '  ', 1pe11.4)
3     format( /,' To scale the plot, type in the mode (n or w) and ', 'h
     *orizontal axis limits first.',/,' When the vertical scale is enter
     *ed, ', 'the routine will exit to the plot.',//,' HORIZONTAL:',/,' 
     *type  n  and left and right hand limits in ', 'INVERSE WAVELENGTH,
     * or:',/,' type  w  and left and right hand WAVELENGTH limits', /,'
     *          (if you type  w  only,  the program will ', 'AUTOSCALE t
     *he limits ', /,'          from the current wavelength set)', //,' 
     *VERTICAL:',/,' Type lower bound and upper bound values for ', 'the
     * VERTICAL AXIS, or:',/,' type  A  to AUTO SCALE (the VERTICAL AXIS
     *), or:',/)
      end

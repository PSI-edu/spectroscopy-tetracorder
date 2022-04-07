C Output from Public domain Ratfor, version 1.04
      subroutine gcrpos (ixx,iyy,xpos,ypos,xmax,xmin,lbnd,diff,iopcon,ie
     *r)
      implicit integer*4(i-n)
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
      common /lbl3/ ictrl, idad, ibncon,ibncn2, ixit
      integer*4 ictrl, idad, ibncon,ibncn2, ixit
      common /lblerr/ error(spmaxchan)
      real*4 error
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
      character*80 atemp, iopcon
      real*4 lbnd
      ier = 0
      axl = 112.
      axh = 1000.
      ayl = 92.
      ayh = 552.
      atemp = iopcon
      if(diff .eq. 0.)then
      diff = 0.1e-36
      endif
      dy = (ayh-ayl)/diff
      an = xmax - xmin
      if(an .le. 0)then
      an = 0.1e-36
      endif
      dx = (axh-axl)/an
      call sb(0)
1     if(igrmod .lt. 50 .or. igrmod .gt. 53)then
      read (ttyin,2,end=2000,err=2000) iopcon
2     format (a)
      i = 1
      call wjfren (i,x,il)
      if(il .eq. ihe .or. il .eq. ihx)then
      iopcon = atemp
      ier = il
      return
      endif
      endif
      call cursrd(ixx,iyy)
      if(ixx .eq. -1 .and. iyy .eq. -1)then
      ier = ihe
      return
      endif
      x = float (ixx)
      y = float(iyy)
      if(y .gt. ayh .or. y .lt. ayl .or. x .gt. axh .or. x .lt. axl)then
      call serase(0,636,1022,696)
      call movabs (0,676)
      call sb(0)
      write (ttyout, 30) ixx,iyy
      call movabs (ixx, iyy)
      go to 1
      endif
      xpos = (x-axl)/dx + xmin
      ypos = (y-ayl)/dy + lbnd
20	format (a,'*s3^',a,$)
30    format (20x,'OUT OF BOUNDS (',i6,',',i6,')')
2000  iopcon = atemp
      return
      end

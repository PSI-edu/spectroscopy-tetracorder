C Output from Public domain Ratfor, version 1.04
      subroutine gcchan(ixx,iyy,xpos,ypos,imatch,nchans,xdata,ydata,ier)
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
      character*80 outline
      real*4 xdata(spmaxchan), ydata(spmaxchan)
      if(ixx .eq. -1 .and. iyy .eq. -1)then
      ier = ihe
      endif
      if(ier .eq. ihe .or. ier .eq. ihx .or. ier .eq. -1)then
      return
      endif
      imatch =0
      xmatch = 0.9e37
      do23004 jj = 1, nchans 
      tstmch = abs(xpos - xdata(jj))
      if(tstmch .lt. xmatch)then
      imatch = jj
      xmatch = tstmch
      endif
23004 continue
23005 continue
      if(imatch .eq. 0)then
      call serase(0,636,1022,696)
      call movabs (0,676)
      call sb(0)
      write (ttyout, 40) ixx,iyy
      call movabs (ixx, iyy)
      ier = -1
      return
      endif
      call serase(0,636,1022,696)
      call movabs (0,676)
      call sb(0)
      write (outline, 50) ixx,iyy,char(0)
      call gwrite(outline)
      write (outline, 51) xpos,ypos,char(0)
      call gwrite(outline)
      write (outline, 52) imatch,xdata(imatch),ydata(imatch),char(0)
      call gwrite(outline)
      call movabs (ixx,iyy)
      call sb(0)
40    format(12x,'CANNOT FIND A CLOSE CHANNEL (',i4,',',i4,')')
50    format('pixel coordinates:       x=',i4,'     y=',i4,a1)
51    format('data coordinates:        x=',1pe13.6,'     y=',1pe13.6,a1)
52    format('closest channel=',i6,'   x=',1pe13.6,'     y=',1pe13.6,a1)
      return
      end

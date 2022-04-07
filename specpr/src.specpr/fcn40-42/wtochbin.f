C Output from Public domain Ratfor, version 1.04
      subroutine wtochbin (waves, ichans, w1, w2, ich1, ich2, ier)
      implicit integer*4 (i-n)
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
      real*4 waves(ichans), w1, w2
      integer*4 ich1, ich2, ier, i, imod12
      imod12 = 0
      if(ier .eq. 12)then
      imod12=12
      endif
      ich1 = 0
      ich2 = 0
      ier = 0
      if(w2 .lt. w1)then
      ier = 1
      write (ttyout,1) w1, w2
1     format ('ERROR in wavelength to channel conversion:',/, '      fir
     *st wavelength (',f12.5, ') is less than the second (',f12.5,')',/)
      return
      endif
      do23004 i = 1, ichans 
      if(w1 .le. waves(i))then
      ich1 = i
      ich2 = ich1
      do23008 j = i, ichans 
      if(w2 .lt. waves(j))then
      go to 100
      endif
      ich2 = j
23008 continue
23009 continue
      go to 100
      endif
23004 continue
23005 continue
      if(imod12 .ne. 12)then
      write (ttyout,2) w1
2     format ('ERROR: wavelength ',f7.5,' not in range',/)
      ier=1
      return
      else
      ier = 12
      return
      endif
100   if((ich1 .eq. ich2) .and. (w1 .le. waves(ich1)) .and. (w2 .ge. wav
     *es(ich2)))then
      return
      else
      if(ich1 .eq. ich2)then
      if((ich1 .gt. 1) .and. (ich1 .le. ichans))then
      if(abs(waves(ich1)-w1) .gt. abs(waves(ich1-1)-w1))then
      ich1 = ich1 - 1
      ich2 = ich1
      return
      endif
      endif
      if((ich1 .gt. 0) .and. (ich1 .lt. ichans))then
      if(abs(waves(ich1)-w1) .gt. abs(waves(ich1+1)-w1))then
      ich1 = ich1 + 1
      ich2 = ich1
      return
      endif
      endif
      endif
      endif
      return
      end

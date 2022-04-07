c*******************************************************************
c   TITLE:        BLOCK EXTRACTION ROUTINE                         *
c                                                                  *
c   PROGRAMMER:   Barry J. Middlebrook                             *
c                 USGS Branch of Geophysics                        *
c                 Denver West Offices                              *
c                 (303) 236-1411                                   *
c------------------------------------------------------------------*
c   DESCRIPTION:                                                   *
c               As part of the 3d i/o package for extracting       *
c               linear arrays from data cubes this routine gets    *
c               handed the upper left coordinates of the block     *
c               to be averaged and the number of columns and rows  *
c               in that block.  It returns the averaged block in   *
c               the form of a single linear array.  The standard   *
c               deviation for each channel is loaded into the      *
c               array in specpr normally used for error bars.      *
c                                                                  *
c------------------------------------------------------------------*
      subroutine blkext (chbuff,i2buff,i4buff,r4buff,ioerr,x,y,z,flag,
     % in,n,a)
c  Set variable type and include common data blocks
      implicit integer*4 (i-n)
      integer*4 ioerr,flag,x,y,z,orgniz,n,a,in,xdim,ydim,zdim
      integer*4 i4buff(384)
      integer*2 i2buff(768)
      real*4 r4buff(384)
      character chbuff*1536
c scratch data arrays
      common /dscrch/ datsc1(4864),datsc2(4864),datsc3(4864),datsc4(
     % 4864)
c the standard common block for header plus complete data set:
      common/label1/ icflag, ititl, usernm, iscta, isctb, jdatea
      common/label1/ jdateb, istb, isra, isdec, itchan, irmas
      common/label1/ revs, iband(2), irwav, irespt, irecno
      common/label1/ itpntr, ihist, mhist
      common/label1/ nruns, siangl, seangl, sphase, iwtrns, itimch
      common/label1/ xnrm, scatim, timint, tempd
      common/label1/ data(4864)
      character*40 ititl
      character*60 ihist
      character*296 mhist
      character*8usernm
      integer*4 icflag, iscta, isctb, jdatea
      integer*4 jdateb, istb, isra, isdec, itchan, irmas
      integer*4 revs, iband, irwav, irespt, irecno, itpntr
      integer*4 nruns, siangl, seangl, sphase, iwtrns, itimch
      real*4 xnrm, scatim, timint, tempd, xfut
      real*4 data
c the following is for compatability with some existing code
      integer*4 ifut(4), filno
      equivalence (iband(1), ifut(1))
      equivalence (irecno, filno)
c the following is for the case when the header is all text
      integer*4 itxtpt
      integer*4 itxtch
      character*19680 itext
      equivalence (iscta,itxtpt), (jdatea,itext(1:1))
      equivalence (isctb,itxtch)
c
c the following included for compatability with the old format
c
      common/labl1b/ cta(3),ctb(3),sta(3),stb(3),datea(3),dateb(3)
      common/labl1b/ ira(3), idec(3)
      character*2 cta,ctb,sta,stb,datea,dateb
      integer*2 ira, idec
c question the need for sta (4/3/85)
c the I/O common block:
      common /recio/ iobuff(384)
      integer*4 iobuff
c now include lmaxes here, because the label1 common is the default
c     data array sizes, and file i/o
      common/lmaxes/ maxrec, maxchn, maxtxt
      integer*4 maxrec, maxchn
      common /lbl3/ ictrl, idad, ibncon,ibncn2, ixit
      common /lblerr/ error(4864)
      integer*4 ictrl, idad, ibncon,ibncn2, ixit
c*******************************************************************
c   TITLE:         3D FILE PARAMETER ARRAY COMMON BLOCK            *
c   PROGRAMMER:    Barry J. Middlebrook                            *
c------------------------------------------------------------------*
c   DESCRIPTION:                                                   *
c               This is the common statements for the 3-d file     *
c               reading routine.  All of the file parameters are   *
c               loaded into the arrays present here.  The array    *
c               <filtyp> contains 11 parameters describing the file *
c               characteristics.  <ftptr> is a pointer array for   *
c               the 11 parameters and other file attributes.  The  *
c               <filreq> array has the pixel coordinates and in-   *
c               dicates the extraction direction.                  *
c                                                                  *
c------------------------------------------------------------------*
c   VARIABLES:                                                     *
c            filtyp      - 12x5 array containing 5 sets of file    *
c                        parameters which are as follows:          *
c                                                                  *
c                     1   specpr file or not                       *
c                                         0 - specpr normal        *
c                                         3 - 3d data file         *
c                     2   file header length in bytes              *
c                     3   record length in bytes                   *
c                     4   record header length in bytes            *
c                     5   DN offset (integer)                      *
c                     6   x - dimension in pixels                  *
c                     7   y - dimension in pixels                  *
c                     8   z - dimension in pixels                  *
c                     9   data type (i2, i4, r4)                   *
c                                         1 - half word integer    *
c                                         2 - full word integer    *
c                                         3 - full word real       *
c                     10  file organization                        *
c                                         1 - BIL                  *
c                                         2 - BIP                  *
c                                         3 - BSQ                  *
c                     11  point deletion flag                      *
c                       * currently, AVIRIS deletion flag = 4096   *
c                                                                  *
c             ftptr      - pointer array for the above parameters  *
c                                                                  *
c             filreq     - coordinates of the pixel and extraction *
c                        direction for the spectra                 *
c                                                                  *
c             fiobox     - box size to extract (default=1)         *
c                                                                  *
c             dnscal     - data number (DN) scale array, this num- *
c                        ber is used as follows:                   *
c                        Reflectance = DNscale*(DN+DNoffset)       *
c                         (real*4)     (real*4)(i2+i4)             *
c                                                                  *
c             NOTE:  The following arrays are 2-d arrays with      *
c                    5 sets of data, one for each file             *
c             when     - integer*4 array with info on time         *
c                     1   Civil or Universal time in seconds       *
c                         scaled by 24000                          *
c                     2   Date in Julian Day number x 10           *
c                                                                  *
c             i4info   - integer*4 array with other pertinent info *
c                     1   Declination coordinate (set to 0)        *
c                     2   Number of channels extracted             *
c                     3   Equivalent atmospheric thickness         *
c                         (airmass x 1000)   (set to 0)            *
c                     4   Number of independent scans made to get  *
c                         spectra  (set to 1)                      *
c                     5   Channel number which defines band nor-   *
c                         malization (set to 1)                    *
c                     6   Record number of wavelength file (set to *
c                         1)                                       *
c                     7   Resolution record (set to 0)             *
c                     8   First record 3d file read encounters     *
c                     9   Text data record pointer (set to 0)      *
c                     10  Number of runs summed together to get    *
c                         resulting spectra (set to 1)             *
c                     11  Angle of incidence of illuminating rad-  *
c                         iation (set to 0)                        *
c                     12  Angle of emission of illuminating radia- *
c                         tion (set to 0)                          *
c                     13  Phase angle between 13 and 14 (set to 0) *
c                     14  Weighted number of runs (set to 1)       *
c                                                                  *
c             titl3d     - record title for extracted spectra      *
c                        to include the pixel coordinates          *
c                                                                  *
c             r4info     - real*4 value information comprised of   *
c                     1   Band normalization factor                *
c                     2   Time (in secs) for one entire spectrum   *
c                         scan                                     *
c                     3   Total integration time (in seconds)      *
c                     4   Temperature in degrees Kelvin            *
c                     5   Time observed in the sample beam for     *
c                         each half chop in milliseconds           *
c                                                                  *
c             autohs     - program automatic 60 character history  *
c                                                                  *
c             manhis     - manual history (4 lines of 74 charac-   *
c                        ters each                                 *
c                                                                  *
c*******************************************************************
c  Set variable type
      integer*4 filtyp(12,5),ftptr(19),filreq(3),when(2,5),fiobox(3)
      real*4 dnscal(5)
      character*24 titl3d(5)
      common /ioftyp/filtyp,ftptr,filreq,dnscal,when,titl3d,fiobox
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
      equivalence(error,num)
c  Initialize working arrays
      do 23000 i=1,4864 
         datsc3(i)=0.0
         datsc4(i)=0.0
         error(i)=0.0
         data(i)=-1.23e34
23000    continue
c  Get the extraction direction and set variable flag
      orgniz=filtyp(10,ftptr(in))
      xdim=filtyp(6,ftptr(in))
      ydim=filtyp(7,ftptr(in))
      zdim=filtyp(8,ftptr(in))
c  Select data format and loop through block of pixels
      if(.not.(x.eq.-1))goto 23002
         lim=xdim
         ylim=y+fiobox(2)
         zlim=z+fiobox(3)
         do 23004 i=y,ylim 
            do 23006 j=z,zlim 
               if(.not.(orgniz .eq. 1))goto 23008
                  call redbil (chbuff,i2buff,i4buff,r4buff,ioerr,x,i,j,
     %             flag,in,n,a)
                  goto 23009
c              else
23008             continue
                  if(.not.(orgniz .eq. 2))goto 23010
                     call redbip (chbuff,i2buff,i4buff,r4buff,ioerr,x,i,
     %                j,flag,in,n,a)
                     goto 23011
c                 else
23010                continue
                     if(.not.(orgniz .eq. 3))goto 23012
                        call redbsq (chbuff,i2buff,i4buff,r4buff,ioerr,
     %                   x,i,j,flag,in,n,a)
                        goto 23013
c                    else
23012                   continue
                        write (6,*)'ERROR: Data format not specified'
                        return
23013                continue
23011             continue
23009          continue
               do 23014 k=1,xdim 
                  if(.not.(data(k) .ne. -1.23e34))goto 23016
                     error(k)=num(k)+1
                     datsc3(k)=datsc3(k)+data(k)
                     datsc4(k)=datsc4(k)+data(k)**2
23016             continue
23014             continue
23006          continue
23004       continue
         goto 23003
c     else
23002    continue
         if(.not.(y.eq.-1))goto 23018
            xlim=x+fiobox(1)
            lim=ydim
            zlim=z+fiobox(3)
            do 23020 i=x,xlim 
               do 23022 j=z,zlim 
                  if(.not.(orgniz .eq. 1))goto 23024
                     call redbil (chbuff,i2buff,i4buff,r4buff,ioerr,i,y,
     %                j,flag,in,n,a)
                     goto 23025
c                 else
23024                continue
                     if(.not.(orgniz .eq. 2))goto 23026
                        call redbip (chbuff,i2buff,i4buff,r4buff,ioerr,
     %                   i,y,j,flag,in,n,a)
                        goto 23027
c                    else
23026                   continue
                        if(.not.(orgniz .eq. 3))goto 23028
                           call redbsq (chbuff,i2buff,i4buff,r4buff,
     %                      ioerr,i,y,j,flag,in,n,a)
                           goto 23029
c                       else
23028                      continue
                           write (6,*)
     %                      'ERROR: Data format not specified.'
                           return
23029                   continue
23027                continue
23025             continue
                  do 23030 k=1,ydim 
                     if(.not.(data(k) .ne. -1.23e34))goto 23032
                        error(k)=num(k)+1
                        datsc3(k)=datsc3(k)+data(k)
                        datsc4(k)=datsc4(k)+data(k)**2
23032                continue
23030                continue
23022             continue
23020          continue
            goto 23019
c        else
23018       continue
            if(.not.(z.eq.-1))goto 23034
               xlim=x+fiobox(1)
               ylim=y+fiobox(2)
               lim=zdim
               do 23036 i=x,xlim 
                  do 23038 j=y,ylim 
                     if(.not.(orgniz .eq. 1))goto 23040
                        call redbil (chbuff,i2buff,i4buff,r4buff,ioerr,
     %                   i,j,z,flag,in,n,a)
                        goto 23041
c                    else
23040                   continue
                        if(.not.(orgniz .eq. 2))goto 23042
                           call redbip (chbuff,i2buff,i4buff,r4buff,
     %                      ioerr,i,j,z,flag,in,n,a)
                           goto 23043
c                       else
23042                      continue
                           if(.not.(orgniz .eq. 3))goto 23044
                              call redbsq (chbuff,i2buff,i4buff,r4buff,
     %                         ioerr,i,j,z,flag,in,n,a)
                              goto 23045
c                          else
23044                         continue
                              write (6,*)'ERROR: Data format not set.'
                              return
23045                      continue
23043                   continue
23041                continue
                     do 23046 k=1,zdim 
                        if(.not.(data(k) .ne. -1.23e34))goto 23048
                           if(.not.(k .eq. 1))goto 23050
23050                      continue
                           error(k)=num(k)+1
                           datsc3(k)=datsc3(k)+data(k)
                           datsc4(k)=datsc4(k)+data(k)**2
23048                   continue
23046                   continue
23038                continue
23036             continue
               goto 23035
c           else
23034          continue
               write (6,*)
     %          'ERROR:  Flag to specify extraction direction not set.'
               return
23035       continue
23019    continue
23003 continue
c  Calculate statistics
      do 23052 k=1,lim 
c  Calculate mean
         data(k)=datsc3(k)/error(k)
c  Calculate standard deviation
         datsc3(k)=datsc4(k)-2*data(k)*datsc3(k)+error(k)*(data(k)**2)
         error(k)=sqrt(datsc3(k)/(num(k)*(num(k)-1)))
23052    continue
c  End subroutine and return to calling program
      return
      end

      subroutine f23(ic)
      implicit integer*4 (i-n)
!ccc
!##### Beginning   ../common/lundefs
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
!##### End   ../common/lundefs
!##### Beginning   ../common/alphabet
      common	/alpha/	iha,ihb,ihc,ihd,ihe,ihf,ihg,ihh,ihi,ihj,ihk,ihl,ihm&
     &,ihn,iho,ihp,ihq,ihr,ihs,iht,ihu,ihv,ihw,ihx,ihy,ihz
      common	/alpha/	ihca,ihcb,ihcc,ihcd,ihce,ihcf,ihcg,ihch,ihci,ihcj,i&
     &hck,ihcl,ihcm,ihcn,ihco,ihcp,ihcq,ihcr,ihcs,ihct,ihcu,ihcv,ihcw,ih&
     &cx,ihcy,ihcz
      common	/digit/	ih0,ih1,ih2,ih3,ih4,ih5,ih6,ih7,ih8,ih9
      common  /asciic/ihprd,ihandp
      integer*4	iha,ihb,ihc,ihd,ihe,ihf,ihg,ihh,ihi,ihj,ihk,ihl,ihm,ihn,&
     &iho,ihp,ihq,ihr,ihs,iht,ihu,ihv,ihw,ihx,ihy,ihz
      integer*4	ihca,ihcb,ihcc,ihcd,ihce,ihcf,ihcg,ihch,ihci,ihcj,ihck,i&
     &hcl,ihcm,ihcn,ihco,ihcp,ihcq,ihcr,ihcs,ihct,ihcu,ihcv,ihcw,ihcx,ih&
     &cy,ihcz
      integer*4	ih0,ih1,ih2,ih3,ih4,ih5,ih6,ih7,ih8,ih9
      integer*4	ihprd,ihandp
!##### End   ../common/alphabet
!##### Beginning   ../common/label1
!     /label1/ common:
!       the data making up the standard specpr file format .
!       cta   : civil or universal time when the data was written.
!       ctb   : civil or universal time when the data run was started.
!       sta   : siderial time when the data was written.
!       stb   : siderial time when the data run was started.
!       datea : date data run was written.
!       dateb : data data run was started.
!       revs  : number of revolutions or scans of the spectrum in a scanning
!          spectrometer (eg: a cvf).
!       filno : file number ( technically a record number ).
!       ira(3): right ascension ( hours, min., secs.)
!       idec(3) : dectination ( deg., min., sec. )
!       irmas : air mass times 1000 (integer).
!       ifut(1) : band normalization lower channel limit.
!       ifut(2) :   "        "       upper    "      "
!       ifut (3 to 7) unused integer variables.
!       itimch  : time observed per object per half chop in milliseconds.
!       ihist   : program automatic history.
!       mhist   : 296 character manual history (sometimes used automatically
!          by program ).
!       nruns   : number of runs (1 run = 1 spectrum ).  more than 1 run can
!          be averaged or summed.
!       ieros   : 1 sigma error bars are located in next record if =1
!          ( variable not fully implemented yet ).
!       iwtrns  : weighted number of runs (not fully implemented yet).
!       xnrm    : band normalization ( scaling factor ) =1.0 for raw data.
!       scatim  : time in seconds to scan spectrum.
!       timint  : total integrating time
!       xfut    : unused variables
!       data    : data
!
! the standard common block for header plus complete data set:
      common/label1/  icflag, ititl, usernm, iscta, isctb, jdatea
      common/label1/  jdateb, istb, isra, isdec, itchan, irmas
      common/label1/  revs, iband(2), irwav, irespt, irecno
      common/label1/  itpntr, ihist, mhist
      common/label1/  nruns, siangl, seangl, sphase, iwtrns, itimch
      common/label1/  xnrm, scatim, timint, tempd
      common/label1/  data(4864)
      character*40    ititl
      character*60    ihist
      character*296   mhist
      character*8	usernm
      integer*4 icflag, iscta, isctb, jdatea
      integer*4 jdateb, istb, isra, isdec, itchan, irmas
      integer*4 revs, iband, irwav, irespt, irecno, itpntr
      integer*4 nruns, siangl, seangl, sphase, iwtrns, itimch
      real*4 xnrm, scatim, timint, tempd, xfut
      real*4 data
! the following is for compatability with some existing code
      integer*4 ifut(4), filno
      equivalence (iband(1), ifut(1))
      equivalence (irecno, filno)
! the following is for the case when the header is all text
      integer*4 itxtpt
      integer*4 itxtch
      character*19680 itext
      equivalence (iscta,itxtpt), (jdatea,itext(1:1))
      equivalence (isctb,itxtch)
!
! the following included for compatability with the old format
!
      common/labl1b/ cta(3),ctb(3),sta(3),stb(3),datea(3),dateb(3)
      common/labl1b/ ira(3), idec(3)
      character*2     cta,ctb,sta,stb,datea,dateb
      integer*2 	ira, idec
! question the need for sta (4/3/85)
! the I/O common block:
      common /recio/ iobuff(384)
      integer*4 iobuff
! now include lmaxes here, because the label1 common is the default
!     data array sizes, and file i/o
      common/lmaxes/ maxrec, maxchn, maxtxt
      integer*4 maxrec, maxchn, maxtxt
!##### End   ../common/label1
!##### Beginning   ../common/lbl4
!     /lbl4/ common:
!       ierr   : error flag in reading and writing files.
!       bbnd, ubnd : lower and upper bounds for the crt plot vertical axis.
!       iopcon: "option control" ; working array for free format input.
!       ipc   : working array ( general )- used in sequential processor (f2,
!          and f3 ).
!       iops  : options array.
!       istarp: starpack; istarp(1)=s if starpack and istarp(2)=file number.
!         if starpack not requested, both are set to 0.  used in math oper-
!         ations.  division by starpack only.
!       iauto : auto scale flag set to a in display routine if auto scaling is
!         requested by user, and is set to c if not requested.
!       iwdgt : name of data tape for data in file w.
!       isavt :  "   "    "    "   "    "   "  "   v.
!       iwrkt :  "   "    "    "   "    "   "  "   d.
!       inmu  :  "   "    "    "   "    "   "  "   u.
!       inmy  :  "   "    "    "   "    "   "  "   y.
!       wmina and wmaxa are the minimum and maximum wavelengths of the crt
!         plot.  if both are 0, the plot routines auto scale the wavlength
!         ( horizontal ) axis using the wavelength file.
!
      common /lbl4/   wmina,wmaxa, bbnd, ubnd
      common /lbl4/   ipc(10), ierr, istarp(2), iauto
      common /lbl4/   iopcon, iops
      common /lbl4/   iwdgt, isavt,iwrkt, inmu,inmy
      real*4		wmina,wmaxa,bbnd,ubnd
      character*80    iopcon
      character*40    iops
      character*8     iwdgt,isavt,iwrkt,inmu,inmy
      integer*4       ipc, ierr, istarp, iauto
!##### End   ../common/lbl4
!##### Beginning   ../common/lbl3
!     /lbl3/ common:
!       ictrl  : control flag for indicating errors are involved or a flag
!         indicating overlaping spectra in the display routine.
!       idad   : flag indicating that erorrs are included in the data (needs
!         to be phased out ).
!       ixit   : flag to indicate that user aborted routine and returned to
!         the main routines.
!
      common /lbl3/ ictrl, idad, ibncon,ibncn2, ixit
      integer*4 ictrl, idad, ibncon,ibncn2, ixit
!######  error  : errors to the data ( 1 standard deviation of the mean ).
      common /lblerr/ error(4864)
      real*4 error
!##### End   ../common/lbl3
!##### Beginning   ../common/lblg
!     /lblg/ common:
!        nchans  : number of channels currently in use.
!        ibnrm1,ibnrm2 : lower and upper channel limits for band normalization
!           which is currently in use.
!        nchsav  : number of channels for wavelength file 1.  for wavelength
!           files >1, the number of channels is stored in the array value 256
!           , thus these wavelength sets can have 255 channels maximum each.
!           NO LONGER USED.
!
      common /lblg/ nchans, ibnrm1, ibnrm2, nchsav,iline
      integer*4 nchans, ibnrm1, ibnrm2, nchsav,iline
!##### End   ../common/lblg
!##### Beginning   ../common/lbl7
!     /lbl7/ common:
!       the following are device control variables:
!       idevc (1) : file w
!       idevc (2) : file v
!       idevc (3) : file d
!       idevc (4) : file s
!
!       idevc  = 0:  dummy
!       idevc  =-1:  disk 1
!       idevc  =-2:  disk 0
!       idevc  =-3:  mag tape 0
!       idevc  =-4:  mag tape 1
!
!       idvc  : used to indicate v file and w file device:
!         idvc (1,j) = mag tape, idvc (2,j) = disk.
!       idevc and idvc were originally used in the mit version of the program
!         but need to be phased out ( they're partially phased out now )
!         completely.  use the volume numbers for this control now.  (in
!             /lblvol/ common).
!       itrol (1) : wavelength file id: V W D U or Y in use
!       itrol (2) : wavelength record currently in use.
!       itrol (3) : channel, wavelength, energy plot control flag.
!       itl  : title option l selected by the user in math operations.
!
      common /lbl7/   idevc(4), idvc(2,4), itl, itrol(3)
      character*40    itl
      integer*4  idevc, idvc, itrol
!##### End   ../common/lbl7
      integer parse
      call eralph
      call whedr
      write (ttyout,100)  
      do   
       call crtin
       i = 1
       call wjfren(i,x,il)
       if (i<80) then
        if (il .eq. ihx) then
         ic = il
         return
        else if (il .eq. ihcv .or. il .eq. ihcw .or. il .eq. ihcd .or.il&
     & .eq. ihcu .or. il .eq. ihcy .or. il .eq. ihcc) then
         iwtmpf=il
         call wjfren(i,x,il)
         if (i>=80 .or. il.ne.0) then
          write (ttyout,200)
          i = 0
          cycle
        end if
         iwrec = x
         call wavlng(iwtmpf,iwrec,ier)
         if (ier.ne.0) then
          write (ttyout,200)
          i=0
          cycle
        end if
        else if (il .eq. ihe) then
         ictrl = ihe
        else
         write(ttyout,200)
        end if
      end if
      end do 
      until (i>=80)
      write (ttyout,300)
      call crtin
      i = parse(iopcon,nchans,ictrl)
      if (i.ne.0) then
       ic = ihe
       print *,' return to continue'
       call crtin
      else
       ic = 0
      end if 
      return
100      format('Math Parsing Routine.',/,'enter  e  to include errors,'&
     &,/,'       wavelength ID (V,W,D,U,Y) followed by record number,',/&
     &,'	   or C followed by the number of channels to change',/,'	   th&
     &e wavelength file,',/,'	x to exit,',/,'	return to continue')
200       format('Error... reenter')		
300        format('Enter equation to be calculated')
      end 

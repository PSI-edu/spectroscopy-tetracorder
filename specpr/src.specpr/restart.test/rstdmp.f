      subroutine	rstdmp
      implicit integer*4 (i-n)
!##### Beginning   ../common/blank
!     explanation of common data:
!       unlabeled common:
!       dataa and datab are the data arrays which the data is put in for
!       an operation ( such as a division, addition, etc. ) and datac is
!       the resultant data calculated.
!       ititl1 ,ititl2 and ititle are the corresponding titles.
!       revs1 and revs2 are the corresponding number of revolutions (number
!         of times the spectrum has been scaned ) of the spectrometer for
!         that data run.
!       ... similarily for iraa, irab, : the right ascension time in seconds.
!       ... and the declination : ideca, idecb ; in arc seconds.
!       ... and the air mass : irmasa, irmasb ; the air mass times 1000.
!           (irmasa and irmasb are integers).
!       ... ihista, ihistb ; the histories fo the 2 files.
!       ... mhista, mhistb ; the manual histories of the two files
!       ... ifl1 and ifl2 are the 2 file (record) numbers.
!       ... ihistc : is the history used only in starpacks.
!       a starpack consists of the 3 data arrays in unlabeled common with a
!         history consisting of ihista, ihistb, and ihistc.
!       ifilex is no longer in use, formerly a file protection variable, see
!          /lblprt/ common.
!       ifutx (32) are unused variables.  the other varialbles correspond to
!          those in /label1/ common with a and b at the end of each variable
!          corresponding to dataa and datab, respectively.
!
      common dataa(4864), datab(4864), datac(4864)
      common revs1, revs2, xnrma, xnrmb, sctma, tminta, tmintb 
      common ifilex, ifl1, ifl2
      common iraa(3),irab(3),ideca(3),idecb(3),irmasa
      common irmasb, itmcha, nruna
      common iwtrna, iwtrnb, ifutx(32)
      common /ch1/ ititl1, ititl2, ititle, ihista, ihistb, ihistc
      common /ch2/ mhista, mhistb
      real*4		dataa,datab,datac
      real*4		revs1,revs2,xnrma,xnrmb,sctma,tminta,tmintb
      integer*4	ifilex,ifl1,ifl2,iraa,irab,ideca,idecb,irmasa
      integer*4	irmasb,itmcha,nruna
      integer*4	iwtrna,iwtrnb,ifutx
      character*40    ititl1,ititl2,ititle
      character*60    ihista,ihistb,ihistc
      character*296   mhista,mhistb
!##### End   ../common/blank
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
!##### Beginning   ../common/lbl6
!     /lbl6/ common:
!       idv1, idv2 : device id from which the data in dataa and datab came
!         from.  these are equal to the letter v,w,d,u, or y.
!                in integer form.
!
      common /lbl6/ idv1, idv2
      integer*4 idv1, idv2
!##### End   ../common/lbl6
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
!##### Beginning   ../common/label3
!     /label3/ common:
!       iwch  : wavelength calibration shift in angstroms ( used oringinally
!         for wedge cvf spectrometer- no longer needed ).
!       alat  : latitude of observatory in radians.
!       ra    : right ascension in radians.
!       dec   : declination in radians.
!       ha    : hour angle in radians.
!       airmas: air mass of object.
!
      common /label3/ alat, ra, dec, ha, airmas,iwch
      real*4 alat, ra, dec, ha, airmas
      integer*4 iwch
!##### End   ../common/label3
!##### Beginning   ../common/labelf
! file protection codes
!     /lblelf/ common:
!       file and device pointers and status flags:
!         value      meaning
!          <-3     illegal device or file
!          =-2     device or file not assigned
!          =-1     logical unit number assigned to dummy
!          =0      assigned but not opened
!          >0      assigned and opened: if the file is assigned, the value
!                  points to the next record ( the last record written or
!                  read plus 1 ).
!
!      the variables are for the following devices and files:
!        mag0   : mag tape drive 0
!        mag1   : mag tape drive 1
!        isavf  : file v
!        iwjf   : file w
!        iwrkf  : file d
!        istrf  : file s ( starpack )
!        ilpt   : line printer
!        icdr   : card reader
!        ipp    : printer / plotter
!        isvcu  : file u
!        iwjcy  : file y
!
!
!        if v file is assigned to mag tape 0, then mag0=isavf.  similarily
!        for the other files.
      common /labelf/mag0,mag1,isavf,iwjf,iwrkf,istrf,ilpt,icdr,ipp
      common /labelf/ isvcu,iwjcy
      integer*4 mag0,mag1,isavf,iwjf,iwrkf,istrf,ilpt,icdr,ipp,isvcu,iwj&
     &cy
      !##### End   ../common/labelf
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
      !##### Beginning   ../common/info
      !     /info/ common:
      !        these are information print on crt control flags.  if = 1 print
      !        info, if = 0 don't print for the following areas :
      !        infmth : math operations information
      !        infopr : program operations control information.
      !        inftrn : file transfer, display, and overlay information.
      !        iother : unused info control flags.
      !
      common /info/ infmth,infopr,inftrn,iother(9)
      integer*4 infmth,infopr,inftrn,iother
      !##### End   ../common/info
      !##### Beginning   ../common/lblvol
      !     /lblvol/ common:
      !               file names of the disk files
      !
      common /lblvol/ ivfl,iwfl,idfl,isfl,iufl
      common /lblvol/ iyfl,ilfl,irfl
      character*80 ivfl,iwfl,idfl,isfl,iufl,iyfl,ilfl,irfl
      !##### End   ../common/lblvol
      !##### Beginning   ../common/lblprt
      !     /lblprt/ common:
      !        device protection:  .ge. 0 = protected ( read .le. protection write
      !                                     to protection + 1 ).
      !                            .eq. 0 = no protection.
      !                            .le. 0 = read only ( up to absolute value of
      !                                     protection ).
      !
      common /lblprt/ iprtu,iprty,iprtd,iprtv,iprtw,iprts
      integer*4 iprtu,iprty,iprtd,iprtv,iprtw,iprts
      !##### End   ../common/lblprt
      !##### Beginning   ../common/hptrm
      common /hptrm/    igrmod,ihpout,imode,ixlast,iylast,ipen
      common /hptrm/    iot
      character*80      ihpout
      integer*4         igrmod,imode,ixlast,iylast,ipen,iot
      !##### End   ../common/hptrm
      character*40 date
      !
      !********* restart file dump ******************************
      !
      data ihv,ihw,ihd,ihs,ihu,ihy/1hv,1hw,1hd,1hs,1hu,1hy/
      data ihl,ihp,ihr/1hl,1hp,1hr/
      data ispa/' '/
      !
      call rstart(2)
      call setspo
      write(12,100)
100	format(82('*'),/,34('*'),' RESTART FILE ',35('*'),/,83('*'),///)
      call ixdate(date)
      write(6,1)
1	format(///,'>>>>>>> dumping restart file <<<<<<<<<<')
      write(12,105)irfl
105	format(/,2x,'Restart File Name: ',a,//)
      write(12,110)
110	format(5x,'File:',t14,'Assigned to:',t62,'Tape Name:',t76,'Protectio&
     &n:')
      write(12,121)ihv,ivfl,isavt,iprtv
121	format(7x,a,7x,a,7x,a,7x,i5)
      write(12,121)ihw,iwfl,iwdgt,iprtw
      write(12,121)ihd,idfl,iwrkt,iprtd
      write(12,121)ihu,iufl,inmu,iprtu
      write(12,121)ihy,iyfl,inmy,iprty
      write(12,122)ihs,isfl,iprts
122	format(7x,a,7x,a,7x,8x,7x,i5)
      write(12,123)ihl,ilfl
123	format(7x,a,7x,a)
      write(12,125)
125	format(//,1x,'Current Status of Files:')
      write(12,126)isavf,iwjf,iwrkf,istrf,isvcu,iwjcy
126	format(3x,'v = f',i5,8x,'w = f',i5,8x,'d = f',i5,8x,'s = f',i5,8x,'u&
     & = f',i5,8x,'y = f',i5)
      write(12,128)nchans,ibnrm1,ibnrm2,nchsav
128	format(/,1x,'Number of Channels:',i5,/,1x,'Lower and Upper Channel L&
     &imits for Band Norm.:',i5,',',i5,/,1x,'Number of Channels for Wave&
     &length file 1:',i5/)
      write(6,12)
12	format('<<<<<<<< finished >>>>>>>>>')
      call dumpsp
      end 

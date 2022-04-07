
!HPUX	program specpr (ach1, ach2, ach3)
!IA64HPUX       program specpr
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
!##### Beginning   ../common/lbl8
!     /lbl8/ common:
!
!       cta   : civil or universal time when the data was written.
!       ctb   : civil or universal time when the data run was started.
!       stb   : siderial time when the data run was started.
!       datea : date data run was written.
!       dateb : data data run was started.
!
! the 1 and 2 values correspond to dataa and datab arrays /blank/
! common, respectively
      common/lbl8/  mcta1,mcta2,mctb1,mctb2
      common/lbl8/  mstb1,mstb2
      common/lbl8/  mdatea1,mdatea2
      common/lbl8/  mdateb1,mdateb2
      integer*4  mcta1,mcta2,mctb1,mctb2
      integer*4  mstb1,mstb2
      integer*4  mdatea1,mdatea2
      integer*4  mdateb1,mdateb2
!##### End   ../common/lbl8
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
!##### Beginning   ../common/labl2
!     /labl2/ common
!     corresponding dates and times as in /label1/ common for the data
!     arrays: dataa and datab (in unlabeled common).
!
      common /labl2/ cta1, ctb1, sta1, stb1, cta2
      common /labl2/ ctb2, sta2, stb2, datea1, dateb1
      common /labl2/ datea2, dateb2
      character*2     cta1(3),ctb1(3),sta1(3),stb1(3)
      character*2     cta2(3),ctb2(3),sta2(3),stb2(3)
      character*2     datea1(3),dateb1(3),datea2(3),dateb2(3)
!##### End   ../common/labl2
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
      !##### Beginning   ../common/cmd
      !
      !  the cmd common block is used by crtin
      !
      !  cfile  = file name to copy commands to
      !  infile = file name to read commands from
      !  copcon = work array for command processing
      !  copy   = tells whether or not commands are to be copied to cfile
      !  redire = tells whether or not command are read from infile
      !  icoman =
      !  iprom  = prompt character
      !  cndx   =
      !  inline =
      !  iend   =
      !
      common /cmd/    cfile,infile,copcon
      common /cmd/    copy,redire
      common /cmd/    icoman,iprom,cndx,inline,iend
      common /cmd/	helppg
      character*40    cfile,infile,helppg
      character*80    copcon
      logical         copy,redire
      integer*4       icoman,iprom,cndx,inline,iend
      !##### End   ../common/cmd
      !##### Beginning   ../common/cmdarg
      !  this common block holds the command line arguments submitted with
      !  the job
      !  ncmdarg is the number of command line arguments
      !  charg1 is the actual argument 1
      common /cmdarg/ ncmdarg, charg1, charg2, charg3
      integer*4 ncmdarg
      character*80 charg1, charg2, charg3
      !##### End   ../common/cmdarg
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
!##### Beginning   ../common/filenames
!	@(#)filenames.h	2.2	05/15/85	16:40:27
!  02/04/2002 RED
!             Modified to accommodate IA64 Fortran90
!  04/08/2002 RED
!             Modified to get rid of define statements
      character*9 NULL
      character*8 TTY
      character*9 DTAPE
      integer*4 TAPELEN
      character*8 TABLET
      character*4 COMMAND
      character*6 WAVFILE
      character*6 WAVHEAD
      character*6 TITLE
      character*29 PLTDAT
      character*30 PLTCMD
      character*26 SPLDAT
      character*26 SPLCMD
      character*26 SPLTMP
      character*7 SPLFILE
      character*9 SPOOLFL
      parameter (NULL = '/dev/null')
      parameter (TTY = '/dev/tty')
      parameter (DTAPE = '/dev/nrmt')
      parameter (TAPELEN = 0)
      parameter (TABLET = '/dev/tab')
      parameter (COMMAND = '.cmd')
      parameter (WAVFILE = '.spwav')
      parameter (WAVHEAD = '.wvhed')
      parameter (TITLE = '.spttl')
      parameter (PLTDAT = '/usr/spool/gplot/pNNxxxxxxxxx')
      parameter (PLTCMD = '/usr/spool/gplot/aaNNxxxxxxxxx')
      parameter (SPLDAT = '/usr/spool/lpd/spxxxxxxxxx')
      parameter (SPLCMD = '/usr/spool/lpd/dfxxxxxxxxx')
      parameter (SPLTMP = '/usr/spool/lpd/tmxxxxxxxxx')
      parameter (SPLFILE = 'spooler')
      parameter (SPOOLFL = 'spoolfile')
!##### End   ../common/filenames
!##### Beginning   ../common/lblwav
! the standard common block for wavelength header plus complete data set:
      common/lblwav/  iwcflg, iwtitl, uwsrnm, iwscta, iwsctb, jdwtea
      common/lblwav/  jdwteb, iwstb, iwsra, iwsdec, iwtchn, iwrmas
      common/lblwav/  wrevs, iwband(2), iwrwav, iwresp, iwrecn
      common/lblwav/  iwtpnt, iwhist, mwhist
      common/lblwav/  nwruns, wiangl, weangl, wphase, wwtrns, wtimch
      common/lblwav/  xwnrm, wcatim, twmint, wtempd
      common/lblwav/  wdata(4864)
      character*40    iwtitl
      character*60    iwhist
      character*296   mwhist
      character*8	uwsrnm
      integer*4 iwcflg, iwscta, iwsctb, jdwtea
      integer*4 jdwteb, iwstb, iwsra, iwsdec, iwtchn, iwrmas
      integer*4 wrevs, iwband, iwrwav, iwresp, iwrecn, iwtpnt
      integer*4 nwruns, wiangl, weangl, wphase, wwtrns, wtimch
      real*4 xwnrm, wcatim, twmint, wtempd
      real*4 wdata
! the following is for io buffering to disk
      integer*4 iowbuf
      common /wavio/ iowbuf(384)
!##### End   ../common/lblwav
!##### Beginning   ../common/sitelogo
      common /sitelogo/ logo
      character*80 logo
!##### End   ../common/sitelogo
!##### Beginning   ../common/hptrm
      common /hptrm/    igrmod,ihpout,imode,ixlast,iylast,ipen
      common /hptrm/    iot
      character*80      ihpout
      integer*4         igrmod,imode,ixlast,iylast,ipen,iot
!##### End   ../common/hptrm
!##### Beginning   ../common/pipes
!
!	This is for command redirection.  ${SPECPR}/free/{crtin,refile}
!
      common/pipes/inpipe,pipelv,pipe
      integer*4 inpipe,pipelv,pipe(9)
!##### End   ../common/pipes
!##### Beginning   ../common/iocontrol
!  I/O control flags for redfil and wrifil routines
      common /iocntl/ rferfl(3), idwcon, ioutverbose
      integer*4 rferfl, idwcon, ioutverbose
! rferfl(1): error message control flag 1: continuation bit flag set
!			on read, but fisrt record of data set expected
!		=0 do not issue error message
!		=1 issue error message
!
! rferfl(2): if error associated to above occurs, set = 1
!					     else set = 0
! rferfl(3): unused
!
! idwcon: device file letter ID decoded by devok, upper case.
!		it is assumed that a specpr record will be read,
!		so this ID corresponds to the current file ID,
!		and is used by wavelength routines for auto-wavelength
!		routines.  if this is set (redfil sets it to zero after
!		read), and puts it in itrol(1), and with wavelength
!		pointer, gives auto waves.
!
! ioutverbose   output verbose level to crt for user prompts:
!                this output suppressor is active only when
!                command files are being read.
!                   = 0 do all output.
!                   = 1 supress level 1 output
!                   = 2 supress level 1 and 2 output
!                   = 3 supress level 1, 2, and 3 output
!##### End   ../common/iocontrol
      character*80 ach1,ach2,ach3
      integer*4 iargc, ier
      data ihzx,ihxz/2hzx,2hxz/
!
!
!
!***********************************************************************
!                                                                      *
!        program written and developed by Roger Nelson Clark           *
!                                                                      *
!***********************************************************************
!                                                                      *
!     LSI 11/23 UNIX version developed 1981,1982                       *
!     converted to ratfor by Jeff Hoover ~1982
!
!     Eunice conversion 1984 by Roger N. Clark
!     HP-UX  conversion 1984 by Roger N. Clark
!                                                                      *
!***********************************************************************
!
!
!     explanation of the file structure :
!       the program uses 6 main files for storage of data.  the user
!       accesses each of the main data records (get a spectrum and its
!       header info) by a letter code signifying one of the files and
!       the record number of the desired spectrum.  the letter id's are
!       : v,w,d,u,y,s ( see user's manual for more details ).  valid
!       record numbers are between 1 and 2000.  note that the program asks
!       the user for the file id and the file number ( technically a record
!       number ) because in the original mit version, each spectrum was a
!       file and not a record as in the current version.
!
!       all i/o is performed by transferring the /label1/ common block to
!       or from disk or tape for the files v,w,d,u, and y.  file 's' is
!       the starpack file which contains the necessary extinction para-
!       meters ( three data arrays - slope, intercept, quality of fit ) so
!       unlabeled common is used.  starpack records 1 to 50 are valid.
!       starpack records can be transferred to the regular data records  for
!       saving ( to w,v,u,d, or y ).  it takes 3 standard specpr data records
!       to hold 1 starpack record.  file d is always a disk file, whereas v,
!       w,u, and y can be either a disk or a tape file.  in order to save
!       disk space, all four of these cannot be assigned to disk or tape only.
!       there are 2 disk and 2 tape files available, thus a special relation-
!       ship is set up between u and v, and w and y.  when v is assigned to
!       disk, u can only be assigned to tape and vice versa.  similarily for
!       w and y.  the histories of operations refer to tape name and file
!       numbers (actually record numbers), thus there are some restrictions
!       for tape to disk and disk to tape transfers.  (eg: to corresponding
!       record numbers only ).  see users manual for more details.
!
!
! TI 980 B (~1982)
!       transfer : disk to tape takes 40 seconds per 100 files transferred
!       transfer : disk to disk takes 40 seconds per 100 files transferred
!       transfer : tape to disk takes 60 seconds per 100 files transferred
!
!
!
!
!
!
!#####################################################################
!##### Beginning   ../logo/set.logo
      logo = 'U. S. Geological Survey, Denver Spectroscopy Lab'
!##### End   ../logo/set.logo
!HPUX   ON REAL UNDERFLOW IGNORE
!HPUX   ON REAL ILLEGAL CALL e1trap
!IA64HPUX ON REAL UNDERFLOW IGNORE
!IA64HPUX ON REAL ILLEGAL CALL e1trap
!
!  check for nulls in site logo and set to blanks.
!
      do i = 1, 80  
       if (logo(i:i) .eq. char(0)) then
        logo(i:i) = ' '
      end if
      end do 
      maxrec = 999999
      maxchn = 4852
      maxtxt = 19860
      iline=0
      helppg =   'specprhelp                              '
!############################################################
!       set command counter to beginning of file            #
!############################################################
      icoman = 1
      redire = .false.
      cndx = 0
      icopy = 0
      ioutverbose = 0
!#########################################################
!       call initialization routine                      #
!#########################################################
!
! first get command line agruments, if any; store in /cmdarg/ common block
      charg1 = ach1
      charg2 = ach2
      charg3 = ach3
      call getcmdargs
!DEBUG:	write (ttyout,7790) ncmdarg,charg1
!DEBUG:	7790	format(' ncmdarg=',i9,20x,'charg1=',a)
      if (ncmdarg < 1) then
       open(cmdlun,file=COMMAND,access='direct',form='unformatted',iosta&
     &t=ier,recl=80)
       if (ier .ne. 0) then
        write (ttyout,151) ier
        stop
      end if
       call wedgea
      else
       if ((ncmdarg .ge. 1) .and. (charg1(1:2) .eq. '-g')) then
        write (ttyout,101)
101		format      ('You must specify a restart file before a ','graphics &
     &mode',/)
        stop
      end if
       call rstart(2)
!############################################################
!       set command counter to beginning of file            #
!############################################################
       icoman = 1
       redire = .false.
       cndx = 0
       icopy = 0
       call taprw
      end if
! check for graphics settings
      if ((ncmdarg .ge. 2) .and. (charg2(1:2) .eq. '-g')) then
       if (charg2(3:5) .eq. 'xhp') then
        igrmod = 50
        call initt(igrmod)
       else if (charg2(3:8) .eq. 'xterm') then
        igrmod = 51
        call initt(igrmod)
       else if (charg2(3:8) .eq. 'xdt') then
! dt for dtterm vs hpterm - result in no io via eralph to clear memory
        igrmod = 60
        call initt(igrmod)
       else if (charg2(3:3) .ge. '0' .and. charg2(3:3) .le. '9') then
        iopcon = charg2
        i = 3
        call wjfren(i,x,il)
        if (il .ne. 0) then
         write (ttyout,*) iopcon
         call what(i)
         write (ttyout,*) 'Graphics mode', x, ' unknown'
         write (ttyout,*) 'Graphics mode ignored'
        else
         igrmod = x
         call initt(igrmod)
        end if
      end if
      end if
      call prochk
111          continue
      call eralph
      call whedr
!
! Initialize random seed (used much later)
!
      call initrand
!
!     write control instructions on crt
!
      call mninst (infopr,id,idvx,idv2,inext,icon,ixit,idisu,idad,ictrl,&
     &idx,x,xx)
      !
      !     call requested routines
      !
      if (id .eq.  ihi) then
       if (idx.eq.ihn) then
        infopr=0
       else
        infopr=1
      end if
      else if (id .eq.  ihg) then
       write (ttyout,1000)
       call gfit
      else if (id .eq. iht .or. id .eq. ihl .or. id .eq. ihm) then
       if (id.ne.ihl) then
        idx=id
      end if
       write (ttyout,1001)
       call reddis(idx)
      else if (id .eq.  ihb) then
       ititle(1:2) = 'zx'
       call wedgea
      else if (id .eq.  ihf) then
       write (ttyout,1002)
       call rstdmp
      else if (id .eq.  ihp) then
       write (ttyout,1003)
       call gpplot
      else if (id .eq.  ihs) then
       write (ttyout,1004)
       call extnct
      else if (id .eq.  ihr) then
       ititle(1:2)='xz'
       call wedgea
      else if (id .eq.  ihce) then
      ! look for capital E
       if (idx .eq. ihcx) then
      ! now if capital X , exit
      ! if graphics = X windows,
      !	reset to HP mode
        if (igrmod .ge. 50 .and. igrmod .le. 59) then
         igrmod = 4
        end if
        call closef
        call rstart(1)
        stop
      end if
      else
       xjunk=0
      end if 
      go to 111
151	format(' open error',      i6, ' on command file')
1000         format(' Transferring to GFIT')
1001         format(' Transferring to display & math routine')
1002         format(' Transferring to restart dump routine')
1003         format(' Transferring to Gould plot routine')
1004         format(' Transferring to extinction routine')
      end 

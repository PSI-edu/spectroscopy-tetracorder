      subroutine rstart (ic)
      implicit integer*4 (i-n)
      integer*4 ic
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!
!     this subroutine sets up the specpr files and protections for
!     restarting the program at the same condition as at the time of
!     the rstart call or it also stores the parameters for a future
!     restart.
!
!      ic= 1: store current parameters for a future restart.
!      ic= 2: restart= recall the parameters and assign and open all
!                      the files and devices.
!      ic= 3: recall the parameters but don't assign and open any files
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      common /blank/  dataa(512)
      common /lblvol/ lblvol(160)
      common /lblprt/ lblprt(6)
      common /label3/ label3(6)
      common /labelf/ labelf(11)
      common /lblg/   lblg(5)
      common /lbl4/	lbl4(58)  
! note: until 5/3/1993 this was 73
      common /info/   info(12)
      common /cmd/    icmd(2)
      common /lbl7/	lbl7(25)
!##### Beginning   ../common/hptrm
      common /hptrm/    igrmod,ihpout,imode,ixlast,iylast,ipen
      common /hptrm/    iot
      character*80      ihpout
      integer*4         igrmod,imode,ixlast,iylast,ipen,iot
!##### End   ../common/hptrm
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
!##### Beginning   ../common/ioftyp
!*******************************************************************
!   TITLE:         3D FILE PARAMETER ARRAY COMMON BLOCK            *
!   PROGRAMMER:    Barry J. Middlebrook                            *
!------------------------------------------------------------------*
!   DESCRIPTION:                                                   *
!               This is the common statements for the 3-d file     *
!               reading routine.  All of the file parameters are   *
!               loaded into the arrays present here.  The array    *
!               <filtyp> contains 11 parameters describing the file *
!               characteristics.  <ftptr> is a pointer array for   *
!               the 11 parameters and other file attributes.  The  *
!               <filreq> array has the pixel coordinates and in-   *
!               dicates the extraction direction.                  *
!                                                                  *
!------------------------------------------------------------------*
!   VARIABLES:                                                     *
!            filtyp      - 12x5 array containing 5 sets of file    *
!                        parameters which are as follows:          *
!                                                                  *
!                     1   specpr file or not                       *
!                                         0 - specpr normal        *
!                                         3 - 3d data file         *
!                     2   file header length in bytes              *
!                     3   record length in bytes                   *
!                     4   record header length in bytes            *
!                     5   DN offset (integer)                      *
!                     6   x - dimension in pixels                  *
!                     7   y - dimension in pixels                  *
!                     8   z - dimension in pixels                  *
!                     9   data type (i2, i4, r4)                   *
!                                         1 - half word integer    *
!                                         2 - full word integer    *
!                                         3 - full word real       *
!                     10  file organization                        *
!                                         1 - BIL                  *
!                                         2 - BIP                  *
!                                         3 - BSQ                  *
!                     11  point deletion flag                      *
!                       * currently, AVIRIS deletion flag = 4096   *
!                                                                  *
!             ftptr      - pointer array for the above parameters  *
!                                                                  *
!             filreq     - coordinates of the pixel and extraction *
!                        direction for the spectra                 *
!                                                                  *
!             fiobox     - box size to extract (default=1)         *
!                                                                  *
!             dnscal     - data number (DN) scale array, this num- *
!                        ber is used as follows:                   *
!                        Reflectance = DNscale*(DN+DNoffset)       *
!                         (real*4)     (real*4)(i2+i4)             *
!                                                                  *
!             NOTE:  The following arrays are 2-d arrays with      *
!                    5 sets of data, one for each file             *
!             when     - integer*4 array with info on time         *
!                     1   Civil or Universal time in seconds       *
!                         scaled by 24000                          *
!                     2   Date in Julian Day number x 10           *
!                                                                  *
!             i4info   - integer*4 array with other pertinent info *
!                     1   Declination coordinate (set to 0)        *
!                     2   Number of channels extracted             *
!                     3   Equivalent atmospheric thickness         *
!                         (airmass x 1000)   (set to 0)            *
!                     4   Number of independent scans made to get  *
!                         spectra  (set to 1)                      *
!                     5   Channel number which defines band nor-   *
!                         malization (set to 1)                    *
!                     6   Record number of wavelength file (set to *
!                         1)                                       *
!                     7   Resolution record (set to 0)             *
!                     8   First record 3d file read encounters     *
!                     9   Text data record pointer (set to 0)      *
!                     10  Number of runs summed together to get    *
!                         resulting spectra (set to 1)             *
!                     11  Angle of incidence of illuminating rad-  *
!                         iation (set to 0)                        *
!                     12  Angle of emission of illuminating radia- *
!                         tion (set to 0)                          *
!                     13  Phase angle between 13 and 14 (set to 0) *
!                     14  Weighted number of runs (set to 1)       *
!                                                                  *
!             titl3d     - record title for extracted spectra      *
!                        to include the pixel coordinates          *
!                                                                  *
!             r4info     - real*4 value information comprised of   *
!                     1   Band normalization factor                *
!                     2   Time (in secs) for one entire spectrum   *
!                         scan                                     *
!                     3   Total integration time (in seconds)      *
!                     4   Temperature in degrees Kelvin            *
!                     5   Time observed in the sample beam for     *
!                         each half chop in milliseconds           *
!                                                                  *
!             autohs     - program automatic 60 character history  *
!                                                                  *
!             manhis     - manual history (4 lines of 74 charac-   *
!                        ters each                                 *
!                                                                  *
!*******************************************************************
!  Set variable type
      INTEGER*4 filtyp(12,5),ftptr(19),filreq(3),when(2,5),fiobox(3)
      REAL*4    dnscal(5)
      CHARACTER*24 titl3d(5)
      COMMON    /ioftyp/filtyp,ftptr,filreq,dnscal,when,titl3d,fiobox
!##### End   ../common/ioftyp
!##### Beginning   ../common/cmdarg
!  this common block holds the command line arguments submitted with
!  the job
!  ncmdarg is the number of command line arguments
!  charg1 is the actual argument 1
      common /cmdarg/ ncmdarg, charg1, charg2, charg3
      integer*4 ncmdarg
      character*80 charg1, charg2, charg3
!##### End   ../common/cmdarg
      integer		i
      integer*4	dataa
      integer*4	ier, idummy
      integer*4	ioftp(127)
      character*80    files(8)
      equivalence 	(files,lblvol)
      equivalence	(filtyp(1,1),ioftp(1))
      if (ic.eq.1) then
       write (ttyout,51)
!
!     store parameters for a future restart
!
       do  i=1,160
        dataa(i)=lblvol(i)
      end do 
       do i=1,6
        dataa(160+i)=lblprt(i)
      end do 
       do i=1,6
        dataa(166+i)=label3(i)
      end do 
       do i=1,11
        dataa(177+i)=labelf(i)
      end do 
       do i=1,5
        dataa(188+i)=lblg(i)
      end do 
       do i=1,12
        dataa(193+i)=info(i)
      end do 
       do i=1,2
        dataa(205+i)=icmd(i)
      end do 
       dataa(208)=igrmod
       do i=1,58
        dataa(208+i)=lbl4(i)  
! note: space for 73
      end do 
       do i=59,73
        dataa(208+i)=0.0 
! note: space holder for 73
      end do 
       do i=1,3
        dataa(281+i)=lbl7(22+i)
      end do 
       do i = 1, 127
        dataa(284+i)=ioftp(i)
      end do 
       write(rlun,rec=1,iostat=ier) dataa
       if (ier.ne.0) then
        goto 4321
      end if
       return
4321          continue
       write(ttyout,4322) ier
4322		format      ( 'write error on restart file, error=', i5, /,' Termi&
     &nating specpr')
       stop
      else
!
!
!----------------------------------------------------------------------
!
!     do a restart:
!
!     allocate the parameter file, recall parameters and allocate
!     and open all files and devices as appropriate.
!
       write (ttyout,50)
       close(rlun,iostat=idummy)
       if (ncmdarg .ge. 1) then
        files(8) = charg1
        write (ttyout, 30) files(8)
30			format      ('restart file= ', a)
      end if
       open(rlun,file=files(8),iostat=ier,access='direct',recl=2048,form&
     &='unformatted')
       if (ier .ne. 0) then
        goto 1234
      end if
       read(rlun,rec=1,iostat=ier)dataa
       if (ier.ne.0) then
        goto 1234
      end if
       do  i=1,160
        lblvol(i)=dataa(i)
      end do 
       do i=1,6
        lblprt(i)=dataa(160+i)
      end do 
       do i=1,6
        label3(i)=dataa(166+i)
      end do 
       do i=1,11
        labelf(i)=dataa(177+i)
      end do 
       do i=1,5
        lblg(i)=dataa(188+i)
      end do 
       do i=1,12
        info(i)=dataa(193+i)
      end do 
       do i=1,2
        icmd(i)=dataa(205+i)
      end do 
       igrmod=dataa(208)
       do i=1,58
        lbl4(i)=dataa(208+i)  
       ! note: space for 73
      end do 
       do i=1,3
        lbl7(22+i)=dataa(281+i)
      end do
       ! don't initialize 61 to 79 because that is ftptr which is set by blockdata
       do i = 1, 60
        ioftp(i)=dataa(284+i)
      end do 
       do i = 80, 127
        ioftp(i)=dataa(284+i)
      end do
       !------------------------------
      end if 
      if (ic.eq.3) then
       return
      end if
      open(cmdlun,file=COMMAND,iostat=idummy,access='direct',recl=80,for&
     &m='unformatted')
      if (idummy.ne.0) then
       write(ttyout,100)
       stop
      end if
      !
      !     assign title file
      !
      open(ttllun,file=TITLE,iostat=idummy,access='direct',recl=128,form&
     &='unformatted')
      if (idummy.ne.0) then
       write(ttyout,400)
       stop
      end if
      !
      !     assign addition scratch file
      !
      open(addlun,access='direct',recl=19456,form='unformatted',iostat=i&
     &dummy,status='scratch')
      if (idummy.ne.0) then
       write(ttyout,500)
       stop
      end if
      !
      !     assign plot scratch file
      !
      open(pltlun,access='direct',recl=80,form='unformatted',iostat=idum&
     &my,status='scratch')
      if (idummy.ne.0) then
       write(ttyout,600)
       stop
      end if
      !
      !     assign device v
      !
      inn = 4
      if (filtyp(1,inn) .eq. 3) then
      ! special 3D file
       irecl = filtyp(3,inn)
      else
      ! normal specpr file
       irecl = 1536
      end if 
      if (files(1).ne.NULL) then
       open(vlun,file=files(1),iostat=idummy,access='direct',recl=irecl,&
     &form='unformatted')
      end if
      if (idummy.ne.0) then
       write(ttyout,700) files(1)
       files(1) = NULL
      end if
       !
       !     assign device w
       !
      inn = 5
      if (filtyp(1,inn) .eq. 3) then
       ! special 3D file
       irecl = filtyp(3,inn)
      else
       ! normal specpr file
       irecl = 1536
      end if 
      if (files(2).ne.NULL) then
       open(wlun,file=files(2),iostat=idummy,access='direct',recl=irecl,&
     &form='unformatted')
      end if
      if (idummy.ne.0) then
       write(ttyout,700) files(2)
       files(2) = NULL
      end if
       !
       !     assign device d
       !
      inn = 3
      if (filtyp(1,inn) .eq. 3) then
       ! special 3D file
       irecl = filtyp(3,inn)
      else
       ! normal specpr file
       irecl = 1536
      end if 
      if (files(3).ne.NULL) then
       open(dlun,file=files(3),iostat=idummy,access='direct',recl=irecl,&
     &form='unformatted')
      end if
      if (idummy.ne.0) then
       write(ttyout,700) files(3)
       files(3) = NULL
      end if
       !
       !     assign starpack file =s
       !
      if (files(4).ne.NULL) then
       open(slun,file=files(4),iostat=idummy,access='direct',recl=77824,&
     &form='unformatted')
      end if
      if (idummy.ne.0) then
       write(ttyout,700) files(4)
       files(4) = NULL
      end if
       !
       !     assign device u
       !
      inn = 1
      if (filtyp(1,inn) .eq. 3) then
       ! special 3D file
       irecl = filtyp(3,inn)
      else
       ! normal specpr file
       irecl = 1536
      end if 
      if (files(5).ne.NULL) then
       open(ulun,file=files(5),iostat=idummy,access='direct',recl=irecl,&
     &form='unformatted')
      end if
      if (idummy.ne.0) then
       write(ttyout,700) files(5)
       files(5) = NULL
      end if
       !
       !     assign device y
       !
      inn = 2
      if (filtyp(1,inn) .eq. 3) then
       ! special 3D file
       irecl = filtyp(3,inn)
      else
       ! normal specpr file
       irecl = 1536
      end if 
      if (files(6).ne.NULL) then
       open(ylun,file=files(6),iostat=idummy,access='direct',recl=irecl,&
     &form='unformatted')
      end if
      if (idummy.ne.0) then
       write(ttyout,700) files(6)
       files(6) = NULL
      end if
       !
       !     assign listing device: lp or dummy
       !
      if (files(7).ne.NULL .and. files(7).ne.SPLFILE) then
       open(lstlun,file=files(7),iostat=idummy,form='formatted')
      end if
      if (idummy.ne.0) then
       write(ttyout,700) files(7)
       files(7) = NULL
      end if
       !----------------------------------------------------------------------
       !
      return
       !
1234          continue
      write(ttyout,"('read error on restart file ier=',i5)")ier
      stop
       !**********************************************************************
50	format(' Restarting ')
51	format(' Updating Restart File ')
100	format('Can''t open command history file (.cmd). exiting!!')
400	format('Can''t open title storage file (.spttl). exiting!!')
500	format('Can''t open addition scratch file. exiting!!')
600	format('Can''t open plot scratch file. exiting!!')
700	format('Can''t open data file ',a,/,'reseting it to /dev/null')
       !
      end 

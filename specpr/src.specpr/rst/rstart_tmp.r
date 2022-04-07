      subroutine rstart (ic)
# *********************************************************
#     SPECPR Version 2 restart ASCII read subroutine
#
#
#     this subroutine sets up the specpr files and protections for
#     restarting the program at the same condition as at the time of
#     the rstart call or it also stores the parameters for a future
#     restart.
#
#      ic= 1: store current parameters for a future restart.
#      ic= 2: restart= recall the parameters and assign and open all
#                      the files and devices.
#      ic= 3: recall the parameters but don't assign and open any files
#      ic= 4: open rlun (unit 18) only. (opens restart file)
#
#      modified to SPECPR Version 2 - Summer 2009  (K.E. Livo)
#
# *********************************************************

      implicit integer*4 (i-n)
	include "../common/spmaxes"   # max parameters, must be first

      integer*4 ic

#      common /blank/  dataa(512)
#      common /lblvol/ lblvol(160)
#      common /lblprt/ lblprt(6)
#      common /label3/ label3(6)
#      common /labelf/ labelf(11)
#      common /lblg/   lblg(5)
#      common /lbl4/     lbl4(58)  # note: until 5/3/1993 this was 73
#      common /info/   info(12)
#      common /cmd/    icmd(2)
#      common /lbl7/     lbl7(25)

      include "../common/hptrm"
      include "../common/lundefs"
      include "../common/filenames"
      include "../common/ioftyp"
      include "../common/cmdarg"

      include "../common/lblvol"
      include "../common/lblprt"
      include "../common/label3"
      include "../common/labelf"
      include "../common/lbl4"

#      integer*4 rlun, ier
#      integer*4 cmdlun, ttllun, addlun, pltlun
#      integer*4 vlun, wlun, dlun, slun, ulun, ylun, lstlun
#      integer*4 idummy

      integer*4 i
      integer*4 dataa
      integer*4 ier, idummy
      integer*4 ioftp(127)
      character*80 files(8)

      integer*4 il, inn, irecl
      real*4 x

#  ***   note: to do - put iopcon2 into common block ******
      character*120 iopcon2
      character*8 lnerr

#      equivalence (files,lblvol)
#      equivalence (filtyp(1,1),ioftp(1))

      if (ic==1) {
#        store current parameters for a future restart
#        version 2 - specpr ascii restart

      write (ttyout,51)
51    format(' Updating Restart File ')

      rewind(rlun)
      write(rlun,'("Specpr restart version=2",A24)')
      write(rlun,'("#",A1)')

#     lblvol.h
#     8 - 80 character filenames
      write(rlun,'("# file names",A12)')
      write(rlun,'("#",A1)')

      write(*,'(A)') files(1)
      write(*,'(A)') ivfl
#
      write(rlun,'("ivfl=",A80)',err=4321) ivfl
      write(rlun,'("iwfl=",A80)',err=4321) iwfl
      write(rlun,'("idfl=",A80)',err=4321) idfl
      write(rlun,'("iufl=",A80)',err=4321) iufl
      write(rlun,'("iyfl=",A80)',err=4321) iyfl
      write(rlun,'("isfl=",A80)',err=4321) isfl
      write(rlun,'("ilfl=",A80)',err=4321) ilfl
      write(rlun,'("irfl=",A80)',err=4321) irfl

#     lblprt.h
#     6 - protection # for open files (u,y,d,v,w,s)
      write(rlun,'("#",A1)')
#
      write(rlun,'("iprtv    =",I10,"  # device protection v")',err=4321) iprtv
      write(rlun,'("iprtw    =",I10,"  # device protection w")',err=4321) iprtw
      write(rlun,'("iprtd    =",I10,"  # device protection d")',err=4321) iprtd
      write(rlun,'("iprtu    =",I10,"  # device protection u")',err=4321) iprtu
      write(rlun,'("iprty    =",I10,"  # device protection y")',err=4321) iprty
      write(rlun,'("iprts    =",I10,"  # device protection s")',err=4321) iprts

#     label3.h
#     6 - observatory variables:
#     (latitude,right ascension, declination,hour angle, air mass,
#      wavelength calibration shift in angstroms)
      write(rlun,'("#",A1)')
#
      write(rlun,'("alat   =",F12.7,"  # observatory lat in rads")',err=4321) alat
      write(rlun,'("ra     =",F12.7,"  # right ascension in radians")',err=4321) ra
      write(rlun,'("dec    =",F12.7,"  # declination in radians")',err=4321) dec
      write(rlun,'("ha     =",F12.7,"  # hour angle in radians")',err=4321) ha
      write(rlun,'("airmas =",F12.6,"  # air mass of object")',err=4321) airmas
      write(rlun,'("iwch   =",I12,"  # wavelength calib shift")',err=4321) iwch

#     labelf.h
#     11 - file pointers and status
      write(rlun,'("#",A1)')
      write(rlun,'("# record pointers",A17)')
      write(rlun,'("#",A1)')
#
      write(rlun,'("mag0     =",I10,"  # mag tape drive 0")',err=4321) mag0
      write(rlun,'("mag1     =",I10,"  # mag tape drive 1")',err=4321) mag1
      write(rlun,'("isavf    =",I10,"  # file v")',err=4321) isavf
      write(rlun,'("iwjf     =",I10,"  # file w")',err=4321) iwjf
      write(rlun,'("iwrkf    =",I10,"  # file d")',err=4321) iwrkf
      write(rlun,'("isvcu    =",I10,"  # file u")',err=4321) isvcu
      write(rlun,'("iwjcy    =",I10,"  # file y")',err=4321) iwjcy
      write(rlun,'("istrf    =",I10,"  # file s")',err=4321) istrf
      write(rlun,'("ilpt     =",I10,"  # line printer")',err=4321) ilpt
      write(rlun,'("icdr     =",I10,"  # card reader")',err=4321) icdr
      write(rlun,'("ipp      =",I10,"  # printer/plotter")',err=4321) ipp

#     lblg.h  (not used in ASCII restart)
#     spectrum in use specifics

#     info.h  (not used in ASCII restart)
#     info print on crt control flags

#     icmd.h  (not used in ASCII restart)

#     igrmod.h  (not used in ASCII restart)

#     lbl4.h
#     graphics plot control values
      write(rlun,'("#",A1)')
#
      write(rlun,'("wmina=",E14.6,"  # plot min wavelength")',err=4321) wmina
      write(rlun,'("wmaxa=",E14.6,"  # plot max wavelength")',err=4321) wmaxa
      write(rlun,'("bbnd =",E14.6,"  # plot bottom")',err=4321) bbnd
      write(rlun,'("ubnd =",E14.6,"  # plot top")',err=4321) ubnd
      write(rlun,'("#",A1)')
      write(rlun,'("# names used for specpr history",A31)')
      write(rlun,'("#",A1)')
      write(rlun,'("isavt=",A8,"  # plot name v")',err=4321) isavt
      write(rlun,'("iwdgt=",A8,"  # plot name w")',err=4321) iwdgt
      write(rlun,'("iwrkt=",A8,"  # plot name d")',err=4321) iwrkt
      write(rlun,'("inmu =",A8,"  # plot name u")',err=4321) inmu
      write(rlun,'("inmy =",A8,"  # plot name y")',err=4321) inmy

#     lbl7.h  (not used in ASCII restart)
#     variables lbl(23) - lbl(25): itrol(1) - itrol(3)
#     wavelength file id, record in use, and
#     chan/wave/energy plot flag

#     ioftyp.h
#     3D file reading routine
      write(rlun,'("#",A1)')
      write(rlun,'("# parameters for 3D file I/O",A28)')
      write(rlun,'("#",A1)')
      write(rlun,'("filtyp(1,1)=",I10,"  # specpr file flag")',err=4321) filtyp(1,1)
      write(rlun,'("filtyp(2,1)=",I10,"  # file header lgth")',err=4321) filtyp(2,1)
      write(rlun,'("filtyp(3,1)=",I10,"  # record length")',err=4321) filtyp(3,1)
      write(rlun,'("filtyp(4,1)=",I10,"  # record hdr lgth")',err=4321) filtyp(4,1)
      write(rlun,'("filtyp(5,1)=",I10,"  # DN offset")',err=4321) filtyp(5,1)
      write(rlun,'("filtyp(6,1)=",I10,"  # x - dimension")',err=4321) filtyp(6,1)
      write(rlun,'("filtyp(7,1)=",I10,"  # y - dimension")',err=4321) filtyp(7,1)
      write(rlun,'("filtyp(8,1)=",I10,"  # z - dimension")',err=4321) filtyp(8,1)
      write(rlun,'("filtyp(9,1)=",I10,"  # data type")',err=4321) filtyp(9,1)
      write(rlun,'("filtyp(10,1)=",I10,"  # file order")',err=4321) filtyp(10,1)
      write(rlun,'("filtyp(11,1)=",I10,"  # point deletion")',err=4321) filtyp(11,1)
      write(rlun,'("filtyp(12,1)=",I10,"  # blank")',err=4321) filtyp(12,1)
#
      write(rlun,'("filtyp(1,2)=",I10,"  # specpr file flag")',err=4321) filtyp(1,2)
      write(rlun,'("filtyp(2,2)=",I10,"  # file header lgth")',err=4321) filtyp(2,2)
      write(rlun,'("filtyp(3,2)=",I10,"  # record length")',err=4321) filtyp(3,2)
      write(rlun,'("filtyp(4,2)=",I10,"  # record hdr lgth")',err=4321) filtyp(4,2)
      write(rlun,'("filtyp(5,2)=",I10,"  # DN offset")',err=4321) filtyp(5,2)
      write(rlun,'("filtyp(6,2)=",I10,"  # x - dimension")',err=4321) filtyp(6,2)
      write(rlun,'("filtyp(7,2)=",I10,"  # y - dimension")',err=4321) filtyp(7,2)
      write(rlun,'("filtyp(8,2)=",I10,"  # z - dimension")',err=4321) filtyp(8,2)
      write(rlun,'("filtyp(9,2)=",I10,"  # data type")',err=4321) filtyp(9,2)
      write(rlun,'("filtyp(10,2)=",I10,"  # file order")',err=4321) filtyp(10,2)
      write(rlun,'("filtyp(11,2)=",I10,"  # point deletion")',err=4321) filtyp(11,2)
      write(rlun,'("filtyp(12,2)=",I10,"  # blank")',err=4321) filtyp(12,2)
#
      write(rlun,'("filtyp(1,3)=",I10,"  # specpr file flag")',err=4321) filtyp(1,3)
      write(rlun,'("filtyp(2,3)=",I10,"  # file header lgth")',err=4321) filtyp(2,3)
      write(rlun,'("filtyp(3,3)=",I10,"  # record length")',err=4321) filtyp(3,3)
      write(rlun,'("filtyp(4,3)=",I10,"  # record hdr lgth")',err=4321) filtyp(4,3)
      write(rlun,'("filtyp(5,3)=",I10,"  # DN offset")',err=4321) filtyp(5,3)
      write(rlun,'("filtyp(6,3)=",I10,"  # x - dimension")',err=4321) filtyp(6,3)
      write(rlun,'("filtyp(7,3)=",I10,"  # y - dimension")',err=4321) filtyp(7,3)
      write(rlun,'("filtyp(8,3)=",I10,"  # z - dimension")',err=4321) filtyp(8,3)
      write(rlun,'("filtyp(9,3)=",I10,"  # data type")',err=4321) filtyp(9,3)
      write(rlun,'("filtyp(10,3)=",I10,"  # file order")',err=4321) filtyp(10,3)
      write(rlun,'("filtyp(11,3)=",I10,"  # point deletion")',err=4321) filtyp(11,3)
      write(rlun,'("filtyp(12,3)=",I10,"  # blank")',err=4321) filtyp(12,3)
#
      write(rlun,'("filtyp(1,4)=",I10,"  # specpr file flag")',err=4321) filtyp(1,4)
      write(rlun,'("filtyp(2,4)=",I10,"  # file header lgth")',err=4321) filtyp(2,4)
      write(rlun,'("filtyp(3,4)=",I10,"  # record length")',err=4321) filtyp(3,4)
      write(rlun,'("filtyp(4,4)=",I10,"  # record hdr lgth")',err=4321) filtyp(4,4)
      write(rlun,'("filtyp(5,4)=",I10,"  # DN offset")',err=4321) filtyp(5,4)
      write(rlun,'("filtyp(6,4)=",I10,"  # x - dimension")',err=4321) filtyp(6,4)
      write(rlun,'("filtyp(7,4)=",I10,"  # y - dimension")',err=4321) filtyp(7,4)
      write(rlun,'("filtyp(8,4)=",I10,"  # z - dimension")',err=4321) filtyp(8,4)
      write(rlun,'("filtyp(9,4)=",I10,"  # data type")',err=4321) filtyp(9,4)
      write(rlun,'("filtyp(10,4)=",I10,"  # file order")',err=4321) filtyp(10,4)
      write(rlun,'("filtyp(11,4)=",I10,"  # point deletion")',err=4321) filtyp(11,4)
      write(rlun,'("filtyp(12,4)=",I10,"  # blank")',err=4321) filtyp(12,4)
#
      write(rlun,'("filtyp(1,5)=",I10,"  # specpr file flag")',err=4321) filtyp(1,5)
      write(rlun,'("filtyp(2,5)=",I10,"  # file header lgth")',err=4321) filtyp(2,5)
      write(rlun,'("filtyp(3,5)=",I10,"  # record length")',err=4321) filtyp(3,5)
      write(rlun,'("filtyp(4,5)=",I10,"  # record hdr lgth")',err=4321) filtyp(4,5)
      write(rlun,'("filtyp(5,5)=",I10,"  # DN offset")',err=4321) filtyp(5,5)
      write(rlun,'("filtyp(6,5)=",I10,"  # x - dimension")',err=4321) filtyp(6,5)
      write(rlun,'("filtyp(7,5)=",I10,"  # y - dimension")',err=4321) filtyp(7,5)
      write(rlun,'("filtyp(8,5)=",I10,"  # z - dimension")',err=4321) filtyp(8,5)
      write(rlun,'("filtyp(9,5)=",I10,"  # data type")',err=4321) filtyp(9,5)
      write(rlun,'("filtyp(10,5)=",I10,"  # file order")',err=4321) filtyp(10,5)
      write(rlun,'("filtyp(11,5)=",I10,"  # point deletion")',err=4321) filtyp(11,5)
      write(rlun,'("filtyp(12,5)=",I10,"  # blank")',err=4321) filtyp(12,5)
#
      return
#
#----------------------------------------------------------------------
#     error handling routines
#
4321  write(ttyout,4322) ier
4322  format ( 'write error on restart file, error=', i5, /, 
               ' Terminating specpr')
      stop

      } else {

#
#----------------------------------------------------------------------
#        ic != 1: recall the parameters (restart ascii version 2)
#                 (Continue when ic = 2, ic = 3, or ic = 4)
#                 Do a Restart
#

#        just open restart file only when ic=4
         if (ic == 4) goto 33

#        initialize rstart variables

         ivfl = ""
         iwfl = ""
         idfl = ""
         iufl = ""
         iyfl = ""
         isfl = ""
         ilfl = ""
         irfl = ""

         iprtv = -1
         iprtw = -1
         iprtd = -1
         iprtu = -1
         iprty = -1
         iprts = -1

         alat = -1.0
         ra = -1.0
         dec = -1.0
         ha = -1.0
         airmas = -1.0
         iwch = -1

         mag0 = -1
         mag1 = -1
         isavf = -1
         iwjf = -1
         iwrkf = -1
         isvcu = -1
         iwjcy = -1
         istrf = -1
         ilpt = -1
         icdr = -1
         ipp = -1

         wmina = -1.0
         wmaxa = -1.0
         bbnd = -1.0
         ubnd = -1.0
         isavt = ""
         iwdgt = ""
         iwrkt = ""
         inmu = ""
         inmy = ""

#  ***   to do: zero out array filtyp(12,5) later  ***

#        restart rstart file

	   write (ttyout,50)
50       format(' Restarting ')
         close(rlun,iostat=idummy)

         if (ncmdarg >= 1) {
            files(8) = charg1
            write (ttyout, 30) files(8)
30          format ('restart file= ', a)
         }

33       continue

#  sole SPECPR Version 2 ascii restart file-open statement here
         open(rlun,file=files(8),iostat=ier,
            access='sequential',form='formatted')
         if (ier != 0) goto 1234

         if (ic == 4) return
#
#        start read loop  (statements 510-511)
#
510      read(rlun,'(A120)',end=511,iostat=ier) iopcon2
            if (ier != 0) goto 1234

         iopcon = iopcon2(1:80)

# start *****  read for loop code here *****

# ***** end file work *****

#
#        lblvol.h
#        8 - 80 character filenames
#          (filename must start at character position 1)
#
         if(iopcon2(1:5)=='ivfl=') {
            i = 5 + lnb(iopcon2(6:85))
            ivfl = iopcon2(6:i) }
         if(iopcon2(1:5)=='iwfl=') {
            i = 5 + lnb(iopcon2(6:85))
            iwfl = iopcon2(6:i) }
         if(iopcon2(1:5)=='idfl=') {
            i = 5 + lnb(iopcon2(6:85))
            idfl = iopcon2(6:i) }
         if(iopcon2(1:5)=='iufl=') {
            i = 5 + lnb(iopcon2(6:85))
            iufl = iopcon2(6:i) }
         if(iopcon2(1:5)=='iyfl=') {
            i = 5 + lnb(iopcon2(6:85))
            iyfl = iopcon2(6:i) }
         if(iopcon2(1:5)=='isfl=') {
            i = 5 + lnb(iopcon2(6:85))
            isfl = iopcon2(6:i) }
         if(iopcon2(1:5)=='ilfl=') {
            i = 5 + lnb(iopcon2(6:85))
            ilfl = iopcon2(6:i) }
         if(iopcon2(1:5)=='irfl=') {
            i = 5 + lnb(iopcon2(6:85))
            irfl = iopcon2(6:i) }

#        lblprt.h
#        6 - protection # for open files (u,y,d,v,w,s)
#
         if (iopcon(1:10)=='iprtv    =') {
            i = 11
            call wjfren(i,x,il)
            if(il!=0) {
               lnerr='iprtv'
               goto 1235
            }
            iprtv = int(x)
         }

         if (iopcon(1:10)=='iprtw    =') {
            i = 11
            call wjfren(i,x,il)
            if(il!=0) {
               lnerr='iprtw'
               goto 1235
            }
            iprtw = int(x)
         }

         if (iopcon(1:10)=='iprtd    =') {
            i = 11
            call wjfren(i,x,il)
            if(il!=0) {
               lnerr='iprtd'
               goto 1235
            }
            iprtd = int(x)
         }

         if (iopcon(1:10)=='iprtu    =') {
            i = 11
            call wjfren(i,x,il)
            if(il!=0) {
               lnerr='iprtu'
               goto 1235
            }
            iprtu = int(x)
         }

         if (iopcon(1:10)=='iprty    =') {
            i = 11
            call wjfren(i,x,il)
            if(il!=0) {
               lnerr='iprty'
               goto 1235
            }
            iprty = int(x)
         }

         if (iopcon(1:10)=='iprts    =') {
            i = 11
            call wjfren(i,x,il)
            if(il!=0) {
               lnerr='iprts'
               goto 1235
            }
            iprts = int(x)
         }

#     label3.h
#     6 - observatory variables:
#     (latitude,right ascension, declination,hour angle, air mass,
#      wavelength calibration shift in angstroms)
#
         if (iopcon(1:8)=='alat   =') {
            i = 9
            call wjfren(i,x,il)
            if(il!=0) {
               lnerr='alat'
               goto 1235
            }
            alat = x
         }

         if (iopcon(1:8)=='ra     =') {
            i = 9
            call wjfren(i,x,il)
            if(il!=0) {
               lnerr='ra'
               goto 1235
            }
            ra = x
         }

         if (iopcon(1:8)=='dec    =') {
            i = 9
            call wjfren(i,x,il)
            if(il!=0) {
               lnerr='dec'
               goto 1235
            }
            dec = x
         }

         if (iopcon(1:8)=='ha     =') {
            i = 9
            call wjfren(i,x,il)
            if(il!=0) {
               lnerr='ha'
               goto 1235
            }
            ha = x
         }

         if (iopcon(1:8)=='airmas =') {
            i = 9
            call wjfren(i,x,il)
            if(il!=0) {
               lnerr='airmas'
               goto 1235
            }
            airmas = x
         }

         if (iopcon(1:8)=='iwch   =') {
            i = 9
            call wjfren(i,x,il)
            if(il!=0) {
               lnerr='iwch'
               goto 1235
            }
            iwch = int(x)
         }

#     labelf.h
#     11 - file pointers and status
#
         if (iopcon(1:10)=='mag0     = ') {
            i = 11
            call wjfren(i,x,il)
            if(il!=0) {
               lnerr='mag0'
               goto 1235
            }
            mag0 = int(x)
         }

         if (iopcon(1:10)=='mag1     =') {
            i = 11
            call wjfren(i,x,il)
            if(il!=0) {
               lnerr='mag1'
               goto 1235
            }
            mag1 = int(x)
         }

         if (iopcon(1:10)=='isavf    =') {
            i = 11
            call wjfren(i,x,il)
            if(il!=0) {
               lnerr='isavf'
               goto 1235
            }
            isavf = int(x)
         }

         if (iopcon(1:10)=='iwjf     =') {
            i = 11
            call wjfren(i,x,il)
            if(il!=0) {
               lnerr='iwjf'
               goto 1235
            }
            iwjf = int(x)
         }

         if (iopcon(1:10)=='iwrkf    =') {
            i = 11
            call wjfren(i,x,il)
            if(il!=0) {
               lnerr='iwrkf'
               goto 1235
            }
            iwrkf = int(x)
         }

         if (iopcon(1:10)=='isvcu    =') {
            i = 11
            call wjfren(i,x,il)
            if(il!=0) {
               lnerr='isvcu'
               goto 1235
            }
            isvcu = int(x)
         }

         if (iopcon(1:10)=='iwjcy    =') {
            i = 11
            call wjfren(i,x,il)
            if(il!=0) {
               lnerr='iwjcy'
               goto 1235
            }
            iwjcy = int(x)
         }

         if (iopcon(1:10)=='istrf    =') {
            i = 11
            call wjfren(i,x,il)
            if(il!=0) {
               lnerr='istrf'
               goto 1235
            }
            istrf = int(x)
         }

         if (iopcon(1:10)=='ilpt     =') {
            i = 11
            call wjfren(i,x,il)
            if(il!=0) {
               lnerr='ilpt'
               goto 1235
            }
            ilpt = int(x)
         }

         if (iopcon(1:10)=='icdr     =') {
            i = 11
            call wjfren(i,x,il)
            if(il!=0) {
               lnerr='icdr'
               goto 1235
            }
            icdr = int(x)
         }

         if (iopcon(1:10)=='ipp      =') {
            i = 11
            call wjfren(i,x,il)
            if(il!=0) {
               lnerr='ipp'
               goto 1235
            }
            ipp = int(x)
         }

#     lbl4.h
#     graphics plot control values
#
         if (iopcon(1:6)=='wmina=') {
            i = 7
            call wjfren(i,x,il)
#            if(il!=0) {
#               lnerr='wmina'
#               goto 1235
#            }
#            wmina = x
         }

         if (iopcon(1:6)=='wmaxa=') {
            i = 7
            call wjfren(i,x,il)
#            if(il!=0) {
#               lnerr='wmaxa'
#               goto 1235
#            }
#            wmaxa = x
         }

         if (iopcon(1:6)=='bbnd =') {
            i = 7
            call wjfren(i,x,il)
#            if(il!=0) {
#               lnerr='bbnd'
#               goto 1235
#            }
#            bbnd = x
         }

         if (iopcon(1:6)=='ubnd =') {
            i = 7
            call wjfren(i,x,il)
#            if(il!=0) {
#               lnerr='ubnd'
#               goto 1235
#            }
#            ubnd = x
         }

         if(iopcon(1:6)=='isavt=') {
            i = 6 + lnb(iopcon(7:14))
            isavt = iopcon(7:i) }
         if(iopcon(1:6)=='iwdgt=') {
            i = 6 + lnb(iopcon(7:14))
            iwdgt = iopcon(7:i) }
         if(iopcon(1:6)=='iwrkt=') {
            i = 6 + lnb(iopcon(7:14))
            iwrkt = iopcon(7:i) }
         if(iopcon(1:6)=='inmu =') {
            i = 6 + lnb(iopcon(7:14))
            inmu = iopcon(7:i) }
         if(iopcon(1:6)=='inmy =') {
            i = 6 + lnb(iopcon(7:14))
            inmy = iopcon(7:i) }

#     lbl7.h  (not used in ASCII restart)
#     variables lbl(23) - lbl(25): itrol(1) - itrol(3)
#     wavelength file id, record in use, and
#     chan/wave/energy plot flag

#     ioftyp.h
#     3D file reading routine
#
         if (iopcon(1:12)=='filtyp(1,1)=') {
            i = 13
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(1,1) = int(x)
         }
         if (iopcon(1:12)=='filtyp(2,1)=') {
            i = 13
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(2,1) = int(x)
         }
         if (iopcon(1:12)=='filtyp(3,1)=') {
            i = 13
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(3,1) = int(x)
         }
         if (iopcon(1:12)=='filtyp(4,1)=') {
            i = 13
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(4,1) = int(x)
         }
         if (iopcon(1:12)=='filtyp(5,1)=') {
            i = 13
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(5,1) = int(x)
         }
         if (iopcon(1:12)=='filtyp(6,1)=') {
            i = 13
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(6,1) = int(x)
         }
         if (iopcon(1:12)=='filtyp(7,1)=') {
            i = 13
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(7,1) = int(x)
         }
         if (iopcon(1:12)=='filtyp(8,1)=') {
            i = 13
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(8,1) = int(x)
         }
         if (iopcon(1:12)=='filtyp(9,1)=') {
            i = 13
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(9,1) = int(x)
         }
         if (iopcon(1:13)=='filtyp(10,1)=') {
            i = 14
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(10,1) = int(x)
         }
         if (iopcon(1:13)=='filtyp(11,1)=') {
            i = 14
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(11,1) = int(x)
         }
         if (iopcon(1:13)=='filtyp(12,1)=') {
            i = 14
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(12,1) = int(x)
         }
#
         if (iopcon(1:12)=='filtyp(1,2)=') {
            i = 13
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(1,2) = int(x)
         }
         if (iopcon(1:12)=='filtyp(2,2)=') {
            i = 13
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(2,2) = int(x)
         }
         if (iopcon(1:12)=='filtyp(3,2)=') {
            i = 13
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(3,2) = int(x)
         }
         if (iopcon(1:12)=='filtyp(4,2)=') {
            i = 13
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(4,2) = int(x)
         }
         if (iopcon(1:12)=='filtyp(5,2)=') {
            i = 13
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(5,2) = int(x)
         }
         if (iopcon(1:12)=='filtyp(6,2)=') {
            i = 13
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(6,2) = int(x)
         }
         if (iopcon(1:12)=='filtyp(7,2)=') {
            i = 13
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(7,2) = int(x)
         }
         if (iopcon(1:12)=='filtyp(8,2)=') {
            i = 13
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(8,2) = int(x)
         }
         if (iopcon(1:12)=='filtyp(9,2)=') {
            i = 13
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(9,2) = int(x)
         }
         if (iopcon(1:13)=='filtyp(10,2)=') {
            i = 14
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(10,2) = int(x)
         }
         if (iopcon(1:13)=='filtyp(11,2)=') {
            i = 14
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(11,2) = int(x)
         }
         if (iopcon(1:13)=='filtyp(12,2)=') {
            i = 14
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(12,2) = int(x)
         }
#
         if (iopcon(1:12)=='filtyp(1,3)=') {
            i = 13
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(1,3) = int(x)
         }
         if (iopcon(1:12)=='filtyp(2,3)=') {
            i = 13
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(2,3) = int(x)
         }
         if (iopcon(1:12)=='filtyp(3,3)=') {
            i = 13
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(3,3) = int(x)
         }
         if (iopcon(1:12)=='filtyp(4,3)=') {
            i = 13
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(4,3) = int(x)
         }
         if (iopcon(1:12)=='filtyp(5,3)=') {
            i = 13
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(5,3) = int(x)
         }
         if (iopcon(1:12)=='filtyp(6,3)=') {
            i = 13
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(6,3) = int(x)
         }
         if (iopcon(1:12)=='filtyp(7,3)=') {
            i = 13
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(7,3) = int(x)
         }
         if (iopcon(1:12)=='filtyp(8,3)=') {
            i = 13
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(8,3) = int(x)
         }
         if (iopcon(1:12)=='filtyp(9,3)=') {
            i = 13
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(9,3) = int(x)
         }
         if (iopcon(1:13)=='filtyp(10,3)=') {
            i = 14
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(10,3) = int(x)
         }
         if (iopcon(1:13)=='filtyp(11,3)=') {
            i = 14
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(11,3) = int(x)
         }
         if (iopcon(1:13)=='filtyp(12,3)=') {
            i = 14
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(12,3) = int(x)
         }
#
         if (iopcon(1:12)=='filtyp(1,4)=') {
            i = 13
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(1,4) = int(x)
         }
         if (iopcon(1:12)=='filtyp(2,4)=') {
            i = 13
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(2,4) = int(x)
         }
         if (iopcon(1:12)=='filtyp(3,4)=') {
            i = 13
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(3,4) = int(x)
         }
         if (iopcon(1:12)=='filtyp(4,4)=') {
            i = 13
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(4,4) = int(x)
         }
         if (iopcon(1:12)=='filtyp(5,4)=') {
            i = 13
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(5,4) = int(x)
         }
         if (iopcon(1:12)=='filtyp(6,4)=') {
            i = 13
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(6,4) = int(x)
         }
         if (iopcon(1:12)=='filtyp(7,4)=') {
            i = 13
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(7,4) = int(x)
         }
         if (iopcon(1:12)=='filtyp(8,4)=') {
            i = 13
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(8,4) = int(x)
         }
         if (iopcon(1:12)=='filtyp(9,4)=') {
            i = 13
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(9,4) = int(x)
         }
         if (iopcon(1:13)=='filtyp(10,4)=') {
            i = 14
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(10,4) = int(x)
         }
         if (iopcon(1:13)=='filtyp(11,4)=') {
            i = 14
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(11,4) = int(x)
         }
         if (iopcon(1:13)=='filtyp(12,4)=') {
            i = 14
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(12,4) = int(x)
         }
#
         if (iopcon(1:12)=='filtyp(1,5)=') {
            i = 13
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(1,5) = int(x)
         }
         if (iopcon(1:12)=='filtyp(2,5)=') {
            i = 13
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(2,5) = int(x)
         }
         if (iopcon(1:12)=='filtyp(3,5)=') {
            i = 13
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(3,5) = int(x)
         }
         if (iopcon(1:12)=='filtyp(4,5)=') {
            i = 13
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(4,5) = int(x)
         }
         if (iopcon(1:12)=='filtyp(5,5)=') {
            i = 13
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(5,5) = int(x)
         }
         if (iopcon(1:12)=='filtyp(6,5)=') {
            i = 13
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(6,5) = int(x)
         }
         if (iopcon(1:12)=='filtyp(7,5)=') {
            i = 13
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(7,5) = int(x)
         }
         if (iopcon(1:12)=='filtyp(8,5)=') {
            i = 13
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(8,5) = int(x)
         }
         if (iopcon(1:12)=='filtyp(9,5)=') {
            i = 13
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(9,5) = int(x)
         }
         if (iopcon(1:13)=='filtyp(10,5)=') {
            i = 14
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(10,5) = int(x)
         }
         if (iopcon(1:13)=='filtyp(11,5)=') {
            i = 14
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(11,5) = int(x)
         }
         if (iopcon(1:13)=='filtyp(12,5)=') {
            i = 14
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(12,5) = int(x)
         }

#
#        loop read back up to label 500 until end-of-file
#
         goto 510
511      continue

      }
      if (ic==3) return

#
#----------------------------------------------------------------------
#     ic= 2: restart= assign and open all the files and devices.
#                 (Continue when ic = 2)
#                 open all files and devices as appropriate.
#

	open(cmdlun,file=COMMAND,iostat=idummy,
		access='direct',recl=80,form='unformatted')
	if (idummy!=0) {
		write(ttyout,100)
		stop
	}
#
#     assign title file
#
	open(ttllun,file=TITLE,iostat=idummy,
		access='direct',recl=128,form='unformatted')
	if (idummy!=0) {
		write(ttyout,400)
		stop
	}
#
#     assign addition scratch file
#
	open(addlun,access='direct',recl=19456,form='unformatted',
		iostat=idummy,status='scratch')
	if (idummy!=0) {
		write(ttyout,500)
		stop
	}
#
#     assign plot scratch file
#
	open(pltlun,access='direct',recl=80,form='unformatted',
		iostat=idummy,status='scratch')
	if (idummy!=0) {
		write(ttyout,600)
		stop
	}
#
#     assign device v
#
	inn = 4
	if (filtyp(1,inn) == 3) {   # special 3D file
		irecl = filtyp(3,inn)
	} else {                    # normal specpr file
		irecl = 1536
	}
	if (files(1)!=NULL) open(vlun,file=files(1),iostat=idummy,
		access='direct',recl=irecl,form='unformatted')
	if (idummy!=0) {
		write(ttyout,700) files(1)
		files(1) = NULL
	}
#
#     assign device w
#
	inn = 5
	if (filtyp(1,inn) == 3) {   # special 3D file
		irecl = filtyp(3,inn)
	} else {                    # normal specpr file
		irecl = 1536
	}
	if (files(2)!=NULL) open(wlun,file=files(2),iostat=idummy,
		access='direct',recl=irecl,form='unformatted')
	if (idummy!=0) {
		write(ttyout,700) files(2)
		files(2) = NULL
	}
#
#     assign device d
#
	inn = 3
	if (filtyp(1,inn) == 3) {   # special 3D file
		irecl = filtyp(3,inn)
	} else {                    # normal specpr file
		irecl = 1536
	}
	if (files(3)!=NULL) open(dlun,file=files(3),iostat=idummy,
		access='direct',recl=irecl,form='unformatted')
	if (idummy!=0) {
		write(ttyout,700) files(3)
		files(3) = NULL
	}
#
#     assign starpack file =s
#
	if (files(4)!=NULL) open(slun,file=files(4),iostat=idummy,
		access='direct',recl=77824,form='unformatted')
	if (idummy!=0) {
		write(ttyout,700) files(4)
		files(4) = NULL
	}
#
#     assign device u
#
	inn = 1
	if (filtyp(1,inn) == 3) {   # special 3D file
		irecl = filtyp(3,inn)
	} else {                    # normal specpr file
		irecl = 1536
	}
	if (files(5)!=NULL) open(ulun,file=files(5),iostat=idummy,
		access='direct',recl=irecl,form='unformatted')
	if (idummy!=0) {
		write(ttyout,700) files(5)
		files(5) = NULL
	}
#
#     assign device y
#
	inn = 2
	if (filtyp(1,inn) == 3) {   # special 3D file
		irecl = filtyp(3,inn)
	} else {                    # normal specpr file
		irecl = 1536
	}
	if (files(6)!=NULL) open(ylun,file=files(6),iostat=idummy,
		access='direct',recl=irecl,form='unformatted')
	if (idummy!=0) {
		write(ttyout,700) files(6)
		files(6) = NULL
	}
#
#     assign listing device: lp or dummy
#
	if (files(7)!=NULL && files(7)!=SPLFILE) open(lstlun,file=files(7),
		iostat=idummy,form='formatted')
	if (idummy!=0) {
		write(ttyout,700) files(7)
		files(7) = NULL
	}

#     warning errors
100	format('Can''t open command history file (.cmd). exiting!!')
400	format('Can''t open title storage file (.spttl). exiting!!')
500	format('Can''t open addition scratch file. exiting!!')
600	format('Can''t open plot scratch file. exiting!!')
700	format('Can''t open data file ',a,/,
			'reseting it to /dev/null')

      return
#
#----------------------------------------------------------------------
#     error handling routines
#
1234  write(ttyout,"('read error-restart file ier=',i5)")ier
      stop
1235  write(ttyout,"('parsing error-restart file value error=',a8)")lnerr
      stop
#
      end

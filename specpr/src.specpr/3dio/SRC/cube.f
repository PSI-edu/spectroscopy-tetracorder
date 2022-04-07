      subroutine cube
      implicit integer*4 (i-n)
c...................................................................
c   TITLE:                 CUBE PATTERN SUBROUTINE                 .
c...................................................................
c   DESCRIPTION:                                                   .
c               This portion of the program is designed to make    .
c               the way the data is read easier to visualize.  The .
c               user is provided with an echo on his choice of     .
c               spectral axis.  If the picture does not match the  .
c               user's idea of how the data needs to be read the   .
c               program is set up to loop back to correct the      .
c               choice.                                            .
c...................................................................
c   VARIABLES:                                                     .
c             flag      - indicates the extraction direction for   .
c                       the intended spectra                       .
c             ans       - check set up to catch user errors        .
c                                                                  .
c...................................................................
c  Set variable type
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
      integer*4 filtyp(12,5),ftptr(19),filreq(3),when(2,5)
      real*4 dnscal(5)
      character*24 titl3d(5)
      common /ioftyp/filtyp,ftptr,filreq,dnscal,when,titl3d
c  Print visual aids for user
1     write (ttyout,*)'                     ......................'
      write (ttyout,*)'                   .                    . .'
      write (ttyout,*)'  ................                    .   .'
      write (ttyout,*)'.....................................     .'
      write (ttyout,*)'. File header  .                    .     .'
      write (ttyout,*)'................                    .     .'
      write (ttyout,*)'               .                    .     .'
      write (ttyout,*)'            x  .       data         .     .'
      write (ttyout,*)'               .                    .     . '
      write (ttyout,*)'               .                    .   .'
      write (ttyout,*)'               .                    . . z'
      write (ttyout,*)'               ......................'
      write (ttyout,*)'                          y'
      write (ttyout,*)
      write (ttyout,*)'Choose extraction direction:'
      write (ttyout,*)'           1    x dim (skewer the y-z plane)'
      write (ttyout,*)'           2    y dim (skewer the x-z plane)'
      write (ttyout,*)'           3    z dim (skewer the x-y plane)'
      write (ttyout,*)'           e,x  to exit'
      write (ttyout,*)
      read (ttyin,*)flag
c  Decode input and check validity
      call crtin
      i=1
      call wjfren (i,x,il)
      if(.not.(il .ne. 0))goto 23000
         write (ttyout,*)'Improper decode, try again.'
         go to 1
23000 continue
      if(.not.(il .eq. ihe .or. il .eq. ihx))goto 23002
         return
23002 continue
      if(.not.(x .lt. 1 .or. il .gt. 3))goto 23004
         write (ttyout,*)'Invalid input.'
23004 continue
      go to 1
      flag=x
c  Indicate extraction direction chosen
      if(.not.(flag .eq. 1))goto 23006
         do 23008 i=1,10 
            write (ttyout,*)
23008       continue
         write (ttyout,*)'                    ......................'
         write (ttyout,*)'                   .                    ..'
         write (ttyout,*)'                  .- - - - - - *       . .'
         write (ttyout,*)'                 .            /*      .  .'
         write (ttyout,*)'                .            / *     .   .'
         write (ttyout,*)'               ............./..*.....    .'
         write (ttyout,*)'               .               *    .    .'
         write (ttyout,*)'               .               *    .    .'
         write (ttyout,*)'               .               *    .    .'
         write (ttyout,*)'            x  .               *    .   .'
         write (ttyout,*)'               .   - - - - - - *    .  . z'
         write (ttyout,*)'               .              /     . .'
         write (ttyout,*)'               .             /      ..'
         write (ttyout,*)'               ............ /........'
         write (ttyout,*)'                          y'
         goto 23007
c     else
23006    continue
         if(.not.(flag .eq. 2))goto 23010
            l=dy
            do 23012 i=1,10 
               write (ttyout,*)
23012          continue
            write (ttyout,*)
     %       '                    ......................'
            write (ttyout,*)
     %       '                   .                    ..'
            write (ttyout,*)
     %       '                  .                    . .'
            write (ttyout,*)
     %       '                 .                    .  .'
            write (ttyout,*)
     %       '                .                    .   .'
            write (ttyout,*)
     %       '               ......................    .'
            write (ttyout,*)
     %       '               .                    .    .'
            write (ttyout,*)
     %       '               .  ********************** .'
            write (ttyout,*)
     %       '               . /|                 . /| .'
            write (ttyout,*)
     %       '            x  ./ |                 ./ |.'
            write (ttyout,*)
     %       '               .  |                 .  | z'
            write (ttyout,*)'               .                    . . '
            write (ttyout,*)'               .                    ..  '
            write (ttyout,*)'               ...................... '
            write (ttyout,*)'                          y'
            goto 23011
c        else
23010       continue
            if(.not.(flag .eq. 3))goto 23014
               do 23016 i=1,10 
                  write (ttyout,*)
23016             continue
               write (ttyout,*)
     %          '                         ......................'
               write (ttyout,*)
     %          '                       .                    . .'
               write (ttyout,*)
     %          '                     .                    .   .'
               write (ttyout,*)
     %          '                   .                    .     .'
               write (ttyout,*)
     %          '                 .         - - - *    .       .'
               write (ttyout,*)
     %          '               ................*.|...         .'
               write (ttyout,*)
     %          '               .             *   |  .         .'
               write (ttyout,*)
     %          '               .           *     |  .         .'
               write (ttyout,*)
     %          '               .         *       |  .         .'
               write (ttyout,*)
     %          '            x  . - - - *            .       .'
               write (ttyout,*)
     %          '               .       |            .     . z'
               write (ttyout,*)
     %          '               .       |            .   .'
               write (ttyout,*)
     %          '               .       |            . .'
               write (ttyout,*)'               ........|.............'
               write (ttyout,*)'                          y'
23014       continue
23011    continue
23007 continue
      write (ttyout,*)
      write (ttyout,*)
c  Prompt for possible mistake
2     write (ttyout,*)
     % 'Is this the way you intended your data to be read?'
c  Decode input and check validity
      call crtin
      i=1
      call wjfren (i,x,il)
      if(.not.(il .ne. 0))goto 23018
         write (ttyout,*)'Improper decode, try again.'
         go to 2
23018 continue
      if(.not.(il .eq. ihe .or. il .eq. ihx))goto 23020
         return
23020 continue
      if(.not.(il.ne.'Y'.or.il.ne.'y'.or.il.ne.'N'.or.il.ne.'n'))goto 23
     % 022
         write (ttyout,*)'Invalid input.'
         go to 1
23022 continue
      flag=x
      if(.not.(ans .eq. 'Y' .or. ans .eq.'y'))goto 23024
         continue
         goto 23025
c     else
23024    continue
         write (ttyout,*)'OK, well try again.'
         go to 1
23025 continue
c  Back to main program
      return
      end

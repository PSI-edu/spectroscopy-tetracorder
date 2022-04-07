      subroutine grstart (ic)
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     This is the specpr restart routine for gfit.
! modified 12/11/81 by rkk
! modified 10/27/82 by jah to update common cmd
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      common /label1/ ititl(20),ilbl1(35)
      common /label1/ ihist(30),mhist(148)
      common /label1/ ilbl2(43)
      common /label1/ data(256)
      common /lbl3/ ictrl,idad,ibncon,ibncn2,ixit
      common /lbl7/ idevc(4),idvc(2,4),itl(20),itrol(3)
      common /lbl6/ idv1,idv2
      common /lbl4/ ixa(1),bbnd,ubnd,iopcon(80),ipc(10),iops(10)
      common /lbl4/ istarp(2),iauto,iwdgt(4),isavt(4),iwrkt(4)
      common /lbl4/ inmu(4),inmy(4),wmina,wmaxa
      common /label3/ iwch(11)
      common /labelf/ mag0(1),mag1,isavf,iwjf,iwrkf,istrf,ilpt,icdr
      common /labelf/ ipp,isvcu,iwjcy
      common /lblg/ nchans,ibnrm1,ibnrm2,nchsav,iline
      common /info/ info(12)
      common /lblvol/ ivfl(20),iwfl(20),idfl(20),isfl(20),iufl(20)
      common /lblvol/ iyfl(20),ilfl(20),ipfl(20)
      common /lblvol/ irfl(20),iwavfl(20),ipid
      common /lblprt/ iprtu,iprty,iprtd,iprtv,iprtw,iprts
      common /hptrm/ ihptrm(30)
      common /idummy/idummy(80)
      common /dafp/ ifv2,ifv03,ifv04,ifv07,ifv08,ifv09,ifv10,ifv11
      common /dafp/ ifv13,ifv14,ifv15,ifv17,ifv18,ifv16
      common /cmd/ icomand,iprom,cndx,redirect,inline,iend,icopy
      common /cmd/ cfile(40),infile(40),copcon(80)
      logical*1 copcon,cfile,infile
      integer*2 eopcon(40),einfile(40),ecfile(40)
      integer*2 icomand,iprom,cndx,redirect,inline,iend,icopy
      integer*2 istart(512)
      integer*2 cta,ctb,sta,stb
      integer*2 dateb,revs,filno
      integer*2 datea
      equivalence (eopcon,copcon),(cfile,ecfile),(infile,einfile)
      call seterr(7,0)
      call seterr(8,0)
      call seterr(9,-1)
      call seterr(1,-1)
      call seterr(2,-1)
      if (ic .ne. 1) then
       go to 1000
      end if
      do  i=1,148
       istart(i)=mhist(i)
      end do 
      do  i=149,178
       istart(i)=ihist(i-148)
      end do 
      do  i=179,208
       istart(i)= ihptrm(i-178)
      end do 
      do  i=209,243
       istart(i) = ilbl1(i-208)
      end do 
      do  i=244,286
       istart(i) = ilbl2(i-243)
      end do 
      call drite(18,3,1024,istart,ier)
      if (ier.ne.0) then
       goto 4321
      end if
      do  i= 1,35
       istart(i)=idevc(i)
      end do 
      istart(36) = icomand
      istart(37) = iprom
      istart(38) = cndx
      istart(39) = redirect
      istart(40) = inline
      istart(41) = icopy
      istart(44)=ixit
      istart(45)=idv1
      istart(46)=idv2
      do  i= 51,182
       istart(i)= ixa(i-50)
      end do 
      do  i=190,200
       istart(i)=iwch(i-189)
      end do 
      do  i= 201,211
       istart(i)=mag0(i-200)
      end do 
      istart(221)=nchans
      istart(222)=ibnrm1
      istart(223)=ibnrm2
      istart(224)=nchsav
      istart(225)=iline
      do  i= 241,252
       istart(i)= info(i-240)
      end do 
      do  i=1,20  
       istart(i+260) = ivfl(i)
       istart(i+280) = iwfl(i)
       istart(i+300) = idfl(i)
       istart(i+320) = isfl(i)
       istart(i+340) = iufl(i)
       istart(i+360) = iyfl(i)
       istart(i+380) = ilfl(i)
       istart(i+420) = ecfile(i)
      end do 
      istart(461) = ipid
      istart(462)= iprtu
      istart(463)= iprty
      istart(464)= iprtd
      istart(465)= iprtv
      istart(466)= iprtw
      istart(467)= iprts
      istart(468)=ictrl
      istart(469)=idad
      do  i=1,10
       istart(469+i) = idummy(i)
      end do 
      istart(480) = filno
      do  i=481,500
       istart(i) = ititl(i-480)
      end do 
      istart(501) = ifl1
      istart(502) = ibncn2
!
!     write parameters to the papameter file (lun 18), record 0
!
      call drite(18,0,1024,istart,ier)
      if (ier.ne.0) then
       goto 4321
      end if
3333          continue
      continue
      return
4321          continue
      write(6,4322)
4322         format('write error on restart file')
      stop
!
!
!----------------------------------------------------------------------
!
!     do a restart:
!
!     allocate the parameter file, recall parameters and allocate
!     and open all files and devices as appropriate.
!     restart file:  userid = spec, file name array
!
1000          continue
      continue
      endfile 18
      call setmod(6,6272)
      if (iargc(i).ge.2) then
       call argfil(18,2,20480)
       call getarg(2,irfl,40)
      else
       call setfil(18,irfl,20480)define file 18(1,1024,u,ifv18)
      end if 
      call dread(18,3,1024,istart,ier)
      if (ier.ne.0) then
       goto 1234
      end if
      do  i=1, 148
       mhist(i)=istart(i)
      end do 
      do  i=149,178
       ihist(i-148)=istart(i)
      end do 
      do i= 179,208
       ihptrm(i-178)= istart(i)
      end do 
      do  i=209,243
       ilbl1(i-208) = istart(i)
      end do 
      do  i = 244,286
       ilbl2(i-243) = istart(i)
      end do 
      call dread(18,0,1024,istart,ier)
      if (ier.ne.0) then
       goto 1234
      end if
      do  i=1,35
       idevc(i) = istart(i)
      end do 
      icomand = istart(36)
      iprom = istart(37)
      cndx = istart(38)
      redirect = istart(39)
      inline = istart(40)
      icopy = istart(41)
      ixit=istart(44)
      idv1= istart(45)
      idv2= istart(46)
      do i=51,182
       ixa(i-50)= istart(i)
      end do 
      do  i=190,200
       iwch(i-189)= istart(i)
      end do 
      do  i= 201,211
       mag0(i-200)= istart(i)
      end do 
      nchans= istart(221)
      ibnrm1= istart(222)
      ibnrm2= istart(223)
      nchsav= istart(224)
      iline=istart(225)
      do  i= 241,252
       info(i-240)= istart(i)
      end do 
      do  i=1,20  
       ivfl(i) = istart(i+260)
       iwfl(i) = istart(i+280)
       idfl(i) = istart(i+300)
       isfl(i) = istart(i+320)
       iufl(i) = istart(i+340)
       iyfl(i) = istart(i+360)
       ilfl(i) = istart(i+380)
       ecfile(i) = istart(i+420)
      end do 
      ipid = istart(461)
      iprtu= istart(462)
      iprty= istart(463)
      iprtd= istart(464)
      iprtv= istart(465)
      iprtw= istart(466)
      iprts= istart(467)
      ictrl=istart(468)
      idad=istart(469)
      do  i=1,10
       idummy(i)=istart(469+i)
      end do 
      filno = istart(480)
      do  i=481,500
       ititl(i-480) = istart(i)
      end do 
      ifl1 = istart(501)
      ibncn2 = istart(502)
!------------------------------
!
!     assign command file
!
      if (redirect.eq.1) then
       endfile 5
       call setfil(5,infile)
      i=1
      do
      if(.not. (i<inline)) then
        exit
      end if
        read(5,1) iopcon
        i=i+1
      end do
1                            format(80a1)
      end if
      if (icopy.eq.1) then
       endfile 99
       call setfil(99,cfile,4096)
      end if
      endfile 16
      call setfil(16,'.cmd',20480)define file 16(1,80,u,ifv16)
!
!     assign wavelength file
!
      call setfil(11,'.spwav',20480)define file 11(1,1024,u,ifv11)
!
!      assign wavelength header file
!
      call setfil(14,'.wvhed',20480)define file 14(1,514,u,ifv14)
!
!     assign title file
!
      call setfil(15,'.spttl',20480)define file 15(1,128,u,ifv15)
!
!     assign device v
!
      call setfil(8,ivfl,20480)define file 8(1,1536,u,ifv08)
!
!     assign device w
!
      call setfil(9,iwfl,20480)define file 9(1,1536,u,ifv09)
!
!     assign device d
!
      call setfil(7,idfl,20480)define file 7(1,1536,u,ifv07)
!
!     assign starpack file =s
!
      call setfil(17,isfl,20480)define file 17(1,2048,u,ifv17)
!
!     assign device u
!
      call setfil(3,iufl,20480)define file 3(1,1536,u,ifv03)
!
!     assign device y
!
      call setfil(4,iyfl,20480)define file 4(1,1536,u,ifv04)
!
!     assign listing device: lp or dummy
!
      call setfil(12,ilfl,6272)
!----------------------------------------------------------------------
!
      goto 3333
!
1234          continue
      write(6,1233)
1233         format('read error on restart file')
      stop
!**********************************************************************
!
      end 

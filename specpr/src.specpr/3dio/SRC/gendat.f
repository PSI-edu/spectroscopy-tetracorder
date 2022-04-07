      implicit integer*4 (i-n)
      common /test1/ image
      integer*2image(20),dark(20),labcal(20)
      integer*4 image4(10),dark4(10),lab4(10)
      integer*4 i,j,k,l,n,nascal(20),ns,nb,nl,imlun
      logicalonly
      equivalence (image,image4)
      equivalence (dark,dark4)
      equivalence (labcal,lab4)
      ns=20
      nb=20
      nl=20
      only=.true.
      open (unit=11,file='im.dat',access='direct',status='new',recl=40,
     % iostat=ioerr)
      open (unit=12,file='dk.dat',access='direct',status='new',recl=40,
     % iostat=ioerr)
      open (unit=13,file='ns.dat',access='direct',status='new',recl=80,
     % iostat=ioerr)
      open (unit=14,file='lb.dat',access='direct',status='new',recl=40,
     % iostat=ioerr)
      n=0
      l=0
      do 23000 i=1,nl 
         do 23002 j=1,nb 
            n=n+1
            labcal(j)=2
            dark(j)=1
            do 23004 k=1,ns 
               image(k)=2
               nascal(k)=3
23004          continue
            if(.not.(n.gt.224))goto 23006
               only=.false.
23006       continue
            if(.not.(only))goto 23008
               write(13,rec=j)nascal
23008       continue
            write(11,rec=n)image4
23002       continue
         if(.not.(i.eq.1))goto 23010
            write(14,rec=1) lab4
23010    continue
         write(12,rec=i) dark4
23000    continue
      end

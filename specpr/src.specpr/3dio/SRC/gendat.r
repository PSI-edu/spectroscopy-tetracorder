	implicit integer*4 (i-n)
	common /test1/ image
	integer*2	image(20),dark(20),labcal(20)
	integer*4       image4(10),dark4(10),lab4(10)
	integer*4 	i,j,k,l,n,nascal(20),ns,nb,nl,imlun
	logical		only
	equivalence     (image,image4)
	equivalence     (dark,dark4)
	equivalence     (labcal,lab4)
	ns=20
	nb=20
	nl=20
	only=.true.


	open (unit=11,file='im.dat',access='direct',status='new',recl=40,
		iostat=ioerr)
	open (unit=12,file='dk.dat',access='direct',status='new',recl=40,
		iostat=ioerr)
	open (unit=13,file='ns.dat',access='direct',status='new',recl=80,
		iostat=ioerr)
	open (unit=14,file='lb.dat',access='direct',status='new',recl=40,
		iostat=ioerr)
	n=0
	l=0
	do i=1,nl  {
	   do j=1,nb  {
	      n=n+1
	      labcal(j)=2
	      dark(j)=1

	      do k=1,ns  { 
	         image(k)=2
	         nascal(k)=3
	      }
	   if (n>224) only=.false.
	   if (only) write(13,rec=j)nascal
	   write(11,rec=n)image4
	   }
	   if (i==1) write(14,rec=1) lab4
	   write(12,rec=i) dark4
	}
	end

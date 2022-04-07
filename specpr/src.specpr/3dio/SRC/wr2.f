	implicit integer*4 (i-n)
	integer*4  	i4buff(112)
	integer*2	i2buff(224)
	equivalence     (i4buff,i2buff)
	open(11,file="lb.dat",recl=448,access='direct',status='new')
	do i=1,224
	   i2buff(i)=1
	end do
	   write (11,rec=1,iostat=ioerr) i4buff
	   if (iostat .ne. 0) then
	      write (6,*)'Houston... we have a problem.   ',ioerr
	      stop
	   end if
	   read(11,rec=1)i2buff
	   do i=1,224
	      write (6,*)i2buff(i)
	   end do
	end

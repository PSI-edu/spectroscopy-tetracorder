	implicit integer*4 (i-n)
	integer*4	i4buff(12)
	open(11,file="del.dat",recl=48,access='direct',status='old')
	write (6,*)'File opened'
	read (11,rec=1)i4buff
	do i=1,12
	   write (6,*)i4buff(i)
	end do
	end

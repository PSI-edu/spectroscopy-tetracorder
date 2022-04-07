	integer*2	reclen,i,j,k
	integer*4	i4buff(20)
	character       infile*13
	write (6,*)'input file name:'
	read (5,10)infile
10	format(a10)
	open(11,file=infile,recl=80,access='direct',status='old')
	do i=1,10  
	   read(11,rec=i)i4buff
	   write (6,*)i4buff
	end do	
	end

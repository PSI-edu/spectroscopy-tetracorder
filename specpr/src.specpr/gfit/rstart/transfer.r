	subroutine trnsfr(imode,error,b,xmean,sigma,gfile)
	implicit integer*4 (i-n)
#
#   common /dafp/ifv2
	dimension buf(192),error(66), b(66)
	character*40 gfile
	data luntr/2/
	if (imode == 1) {
		rewind 2
		do i = 1,66
			write(2,'(e16.6)') error(i)
		do i = 1,66
			write(2,'(e16.6)') b(i)
		write(2,'(e16.6)') xmean
		write(2,'(e16.6)') sigma
		write(2,'(a)') gfile
		if (ier != 0) {
			write(6,1234)
1234        format(1x,'write error...')
			return
		}

	} else {
		close(2,iostat=i)
		open(2,file='.trans',iostat=ier)
		rewind 2
		do i = 1,66
			read(2,'(e16.6)') error(i)
		do i = 1,66
			read(2,'(e16.6)') b(i)
		read(2,'(e16.6)') xmean
		read(2,'(e16.6)') sigma
		read(2,'(a)') gfile
		if (ier != 0) {
			write(6,4321)
4321		format(1x,'read error...')
			return
		}
#		for(i=1;i<=66;i=i+1) {
#			error(i) = buf(i)
#			b(i) = buf(i+66)
#		}
#		xmean = buf(133)
#		sigma = buf(134)
#		for(i=1;i<=10;i=i+1) gfile(i) = buf(i+134)
	}
	return
	end

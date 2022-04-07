	subroutine readhd(ikey,ichans)
    implicit integer*4 (i-n)
	dimension ibuf(257)
	if (ikey > 99 | ikey < 1) {
		write(6,10)
10      format(1x,'wavelength file number out of bounds...')
		return
	}

    read(14,rec=ikey,iostat=ier) ibuf
	if (ier == -1) {
        write(6,20)
	}
	ichans = ibuf(257)
   	return
20  format(1x,'readhd...read error wavelng header file')
	end

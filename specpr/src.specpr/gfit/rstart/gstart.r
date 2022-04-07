      subroutine gstart(ic,rfile)
	  implicit integer*4 (i-n)
      common/status/ ilabel(768)

      character*40 rfile

	if (ic == 2) {
		close(1,iostat=i)
		close(2,iostat=i)
        open(1,file=rfile,access='direct',form='unformatted',recl=1536,iostat=ier)
#	rewind 1
		open(2,file='.trans')
#	rewind 2
		read(1,rec=1,iostat=ier) ilabel
		if (ier != 0) goto 99
	} else {
		write(1,rec=1,iostat=ier) ilabel
		if (ier != 0.0) goto 99
		}

99      return
	end

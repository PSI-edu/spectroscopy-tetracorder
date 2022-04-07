      subroutine save(lungf,irec)
	  implicit integer*4 (i-n)
      common/status/ info(768)
#
      nchar = 1536
	  write(lungf,rec=irec+1,iostat=ier) info
      if (ier != 0) goto 10
      return
10    write(6,20)
20    format(1x,'save: write error...',/)
      iflg=1
      return
      end

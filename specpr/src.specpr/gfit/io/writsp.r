      subroutine writsp(lun,key)
	  implicit integer*4 (i-n)
      common /label1/a(384)

      write(lun,rec=key+1,iostat=ier)a
      if (ier!=0) {
          write(6,20)lun,key+1,ier
      }
      return
20    format(1x,'writsp: lun=',i4,' key=',i4,'ier=',i4)
      end

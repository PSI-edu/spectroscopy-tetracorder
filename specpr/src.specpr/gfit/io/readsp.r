#  6.8.80  dmc  spio  sfortran
#
#  these routines perform i/o to specpr files
#--------------------------------------------------------------
      subroutine readsp(lun,buf,key,iflg)
	  implicit integer*4 (i-n)
      dimension buf(384)
 
    read(lun,rec=key+1,iostat=ier) buf
	if (ier != 0) {
        write(6,20)lun,key,ier
        iflg = 1
	}
 
      return
20    format(' readsp: lun=',i4,' key=',i4,'error=',i4/)
      end

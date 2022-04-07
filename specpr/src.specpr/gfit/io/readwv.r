      subroutine readwv(wbuf,key,iflg)
	  implicit integer*4 (i-n)
#
      dimension wbuf(256)
#
    read(11,rec=key+1,iostat=ier) wbuf
	if (ier == -1) {
        write(6,20)key+1,ier
	    iflg = 1
	}
#
    return
20  format(1x,'readwv: read error...lun: 11 key: ',i4,' ier = ',i4/)
    end

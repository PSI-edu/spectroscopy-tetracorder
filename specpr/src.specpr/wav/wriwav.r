	subroutine wriwav(key)
	implicit integer*4 (i-n)

	integer*4 ier

	include "../common/spmaxes"   # max parameters, must be first

	include "../common/blank"
	include "../common/lblg"

	ier = 0
	if (key<=0||key>99) key = 1
	if (nchans>256) nchans = 256
	if (nchans<2) nchans = 2
	lun = 11
	write(lun,rec=key+1,iostat=ier)dataa
	call erored(icnt,ier,1024)
	return
	end

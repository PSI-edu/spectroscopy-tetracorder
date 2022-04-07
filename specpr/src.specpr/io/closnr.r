	subroutine closnr(lun)
	implicit integer*4 (i-n)

############################################################
#		close lun and reopen. used to write EOF on tapes
############################################################

	include "../common/spmaxes"   # max parameters, must be first

	include "../common/lblvol"
	include "../common/lundefs"
	include "../common/filenames"

	integer*4 ier

	character*40	filenm


	if (lun == 3) {
		filenm = iufl

	} else if (lun == 4) {
		filenm = iyfl

	} else if (lun == 7) {
		filenm = idfl

	} else if (lun == 8) {
		filenm = ivfl

	} else if (lun == 9) {
		filenm = iwfl

	} else {
		write(ttyout,10) lun
	}
	close (lun,iostat=ier)
	open (lun,file=filenm,access='direct',form='unformatted',
		  iostat=ier,recl=1536)
	if(ier!=0) write(ttyout,20) filenm

	return
10	format(' attempt to close & reopen lun',i7)
20	format("Can't reopen file ",a)
	end

   subroutine getwav(iwavd,iwavr,il)

	implicit integer*4 (i-n)

	include "../../src.specpr/common/spmaxes"
	include "../../src.specpr/common/label1"
	include "../../src.specpr/common/lundefs"
	include "../../src.specpr/common/alphabet"
	include "../../src.specpr/common/lblg"
	include "../../src.specpr/common/lblwav"
	include "../../src.specpr/common/lbl7"
	include "defs.h"
	include "lmrefl.h"

	integer*4 idlt(SPMAXCHAN)

40	write (ttyout, 45)
45	format (/,'============',/,
                ' Type in the file id and record number ',
		'of the wavelength set')
	call crtin
	i = 1
	call wjfren (i,x,iwavd)
	call wjfren (i,x,il)
	if (iwavd == ihx || iwavd == ihe) {
		il = iwavd
		go to 10000
	}
	if (il == ihe || il == ihx) go to 10000
	if (il != 0) {
		call what(i)
		go to 40
	}
	if (iwidok (iwavd) != 1) {  # check if wav id is OK
		go to 40
	}
	if (x < 1 || x > maxrec) {
		write (ttyout, 50) 
50		format (/,' ERROR record number out of range, reenter')
		go to 40
	}
	iwavr = x
	itmp = iwavr

#       set the default wavelength set

	itrol(1) = iwavd    # wave ID
	itrol(2) = iwavr    # wave record number

	call redwav(iwavd,itmp,ier)
	if (ier != 0) go to 40

#	nchsav = nchans
	nchans = iwtchn
	if (nchans < 1) {
		write (ttyout, 60) nchans
60		format (/,' TOO FEW channels:',i6, /, 'Redo',/)
		go to 40
	}
	if (nchans > SPMAXCHAN) {
		write (ttyout, 65) nchans, SPMAXCHAN
65		format(/,'Too Many Channels:',i6,/,
		        'Radtran cannot handle more than',i7,
			' channels',/)
		go to 40
	}
	do i = 1, nchans {
		if (wdata(i) < 0.00001) wdata(i) = -1.23e+34
		wav(i) = wdata(i)
	}

#
# channels to delete
#
	write (ttyout,70)
70	format (/,'To DELETE channels, type d and the channels to be',
		' deleted')
	call crtin
	i = 1
	call wjfren(i,x,il)
	if (il == ihe || il == ihx) go to 10000
	if (il == ihd) {
		call dltpts(i,j,idlt,nchans,ic)
		if (ic == ihe || ic == ihx) go to 10000
		if (j > 0) {
			do itemp = 1, j {
				wav(idlt(itemp)) = -1.23e+34
			}
		}
	}


10000	return
	end 

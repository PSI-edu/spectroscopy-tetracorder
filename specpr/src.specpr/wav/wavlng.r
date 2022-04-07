	subroutine wavlng (iwidfl, key, ier)
	implicit integer*4 (i-n)
#cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
#                                                               c
#       this routine reads the wavelength file into             c
#       dataa, and sets the number of channels.                 c
#                                                               c
#       arguments:                                              c
#	    iwidfl	the filid in uppercase			c
#			if the filid is a C, then key is taken  c
#			to be the number of channels, and 	c
#			number is assigned to wavelengths	c
#           key         the record number to be read.           c
#			(or the number of channels, see iwidfl	c
#	    ier		return error				c
#                                                               c
#cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
#
	include "../common/spmaxes"   # max parameters, must be first

	include "../common/alphabet"
	include "../common/blank"
	include "../common/lblg"
	include "../common/lundefs"
	include "../common/label1"
	include "../common/lblwav"
	include "../common/lbl7"

	integer*4 ier
#RED
	integer*4 iwidok      # function


         ier=0


	if (iwidok(iwidfl) == 0) {
		write (ttyout, 1) iwidfl
1		format (1x, ' ERROR: INVALID Wavelength ID: ',a1)
		ier = 1
		return
	}

	if (iwidfl == ihcc) {
#  assign wavelengths as channel numbers:
		if (key <= SPMAXCHAN) {
			do i = 1, SPMAXCHAN {
				wdata(i) = float(i)
			}
			iwtchn = key
			nchans = iwtchn
# set in use variables
			itrol(1) = ihcc
			itrol(2) = nchans
		} else {
			write (ttyout, 10) key
			ier = 1
			return
		}
	} else {

		call redwav (iwidfl, key, ier)
		nchans = iwtchn
		if (nchans > maxchn) {
			nchans = maxchn
			iwtchn = maxchn
		}
		if (ier == 0) {
			itrol(1) = iwidfl
			itrol(2) = key
		}
	}

	do i = 1, SPMAXCHAN {
		dataa(i) = wdata(i)
	}

10	format (' ERROR: Channels are specified as wavelengths, ',
		' but TOO MANY CHANNELS',
		' specified:', i7, /)
	return
	end

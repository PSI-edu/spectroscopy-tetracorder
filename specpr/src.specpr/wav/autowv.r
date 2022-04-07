	subroutine autowv (i)
	implicit integer*4 (i-n)

#ccc  name: autowav
#ccc  version date: 1/17/86
#ccc  author(s): Roger N. Clark
#ccc  language: ratfor
#ccc
#ccc  short description: set wavelength automatically from idwcon
#ccc			and wavelength pointer irwav.
#ccc
#ccc  algorithm description: none
#ccc  system requirements: none
#ccc  subroutines called: none
#ccc  argument list description: i: dummy variable
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced: none
#ccc  internal variables:
#ccc  file description: none
#ccc  user command lines: none
#ccc  update information:
#ccc  NOTES:
#ccc

	include "../common/spmaxes"   # max parameters, must be first

	include "../common/alphabet"
	include "../common/iocontrol"
	include "../common/label1"
	include "../common/lbl7"
	include "../common/lundefs"

	if (itrol(1) == ihandp) {
		if (idwcon != 0) {
			if (idwcon == ihv) idwcon = ihcv
			if (idwcon == ihw) idwcon = ihcw
			if (idwcon == ihd) idwcon = ihcd
			if (idwcon == ihu) idwcon = ihcu
			if (idwcon == ihy) idwcon = ihcy
			if (irwav > 0) {
				itrol(1) = idwcon
				itrol(2) = irwav
			} else {
				itrol(1) = ihcc
				itrol(2) = itchan
				if (itrol(2) <= 0) itrol(2) = 1
				write (ttyout, 10) itrol(2)
			}
		}
	}
	return

10	format ('WARNING: can not auto-set wavelengths: ',
		'invalid wavelength pointer',/, 9x,
		'setting wavelengths to channel number, channels=',
		i5, /)
	end

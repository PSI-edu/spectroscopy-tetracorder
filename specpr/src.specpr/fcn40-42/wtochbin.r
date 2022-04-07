	subroutine wtochbin (waves, ichans, w1, w2, ich1, ich2, ier)
	implicit integer*4 (i-n)

#ccc  name: wtochbin
#ccc  version date: 10/22/91
#ccc  author(s): Roger N. Clark
#ccc  language: Ratfor
#ccc
#ccc  short description: Given wavelength array "waves" of ichans
#ccc                     length, and wavelength interval w1 to w2, find
#ccc                     first set of channels (ich1, ich2) inside the
#ccc                     wavelength interval.
#ccc
#ccc  algorithm description:
#ccc  system requirements: none
#ccc  subroutines called: none
#ccc  argument list description: see short description
#ccc  parameter description: none
#ccc  common description: none
#ccc  message files referenced: none
#ccc  internal variables:
#ccc  file description: none
#ccc  user command lines: none
#ccc  update information:
#ccc  NOTES:
#ccc


	include "../common/lundefs"
	real*4 waves(ichans), w1, w2
	integer*4 ich1, ich2, ier, i, imod12

	imod12 = 0 # issue error messages
	if (ier == 12) {
		imod12=12  # do not issue errors for out of range wavelengths
				# used for tetracorder
	} 
		

	ich1 = 0
	ich2 = 0
	ier = 0

#DEBUG:
#	write (ttyout,*) 'DEBUG: number of channels= ',ichans
#	write (ttyout,*) 'DEBUG: waves 52, 53, 60, 61:',waves(52),waves(53),
#				waves(60),waves(61)
#	write (ttyout,*) 'DEBUG: wavelength interval=', w1, w2
#END_DEBUG

	if (w2 < w1) {
		ier = 1
		write (ttyout,1) w1, w2
1		format ('ERROR in wavelength to channel conversion:',/,
			'      first wavelength (',f12.5,
			') is less than the second (',f12.5,')',/)
		return
	}

	do i = 1, ichans {

		if (w1 <= waves(i)) {

			ich1 = i
			ich2 = ich1

			do j = i, ichans {

				if (w2 < waves(j)) go to 100
				ich2 = j
			}
			go to 100
		}
	}

	if (imod12 != 12) {      # w1 never found and mode is to issue error messages
		write (ttyout,2) w1
2		format ('ERROR: wavelength ',f7.5,' not in range',/)
		ier=1
		return
	} else {
		ier = 12   # w1 never found but no error messages
		return
	}

100	if ((ich1 == ich2) & (w1 <= waves(ich1)) & (w2 >= waves(ich2))) {

#                        wavelength range bounds the channel found,
#                        so dont move it.

		return

	} else if (ich1 == ich2) {   # if one channel found, check if it is
					# the closest channel
					# if above condition doesn't
					# hold, which means the range does
					# not actually encompas the channel.

		if ((ich1 > 1) & (ich1 <= ichans)) {
			if (abs(waves(ich1)-w1) > abs(waves(ich1-1)-w1)) {
				ich1 = ich1 - 1
				ich2 = ich1
				return
			}
		}
		if ((ich1 > 0) & (ich1 < ichans)) {
			if (abs(waves(ich1)-w1) > abs(waves(ich1+1)-w1)) {
				ich1 = ich1 + 1
				ich2 = ich1
				return
			}
		}
	}
	return
	end

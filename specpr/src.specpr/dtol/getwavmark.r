	subroutine getwavmark (i,iwm,ierw)
	implicit none

# read wavelength markers for plots

	include "../common/spmaxes"   # max parameters, must be first

        include "../common/lbl4"
        include "../common/alphabet"
        include "../common/wavemarks"

# the wavelength marks common block has:
#       wmrflg  = -1 not defined  
#               = 0 do not show marks
#               = 1 show marks
#       wmrflgb = stored setting of wmrflg when the mark gets turned off
#                 when turned back on, wmrflg(n) = wmrflgb(n)
#       wmrclr  =     color (to be defined, future)
#       wmrwav  = wavelength markers (up to 9)
#       wmnum   = number of wavelength markers (up to 9)
#       wmops   = mark options

	integer*4 i, iwm, ierw, j, il
	real*4    x

	ierw=0   # error value

	#write (*,*) "DEBUG: getwavmarkers pos1"

#       i = index in the iopcon command line array
#       iwm = which array to put specturm into (1 to 9)

	if ( iwm > 9 ) {
		write (*,*) "ERROR, wavelength marker set out of range:", iwm
		call what(i)
		ierw=1
		return
	}

	if ( iwm > 0 ) {

		wmnum(iwm)=0       # no wavelength markers yet
		do j = 1, 9 {
			call wjfren(i,x,il)  # get wavelength mark value
			if (il != 0) {       # unexpected character found
				call what(i)
				ierw=1
				return
			}
			if (i < 80 ) {   # wavelength marker
				wmrwav(j,iwm) = x
				wmnum(iwm)=j          # now have j wavelength marks
			}
			if (i == 80 ) break   # end loop at end of input line
		}
		

		wmrflg(iwm) = 1
		wmrflgb(iwm) = 1

		# now check for options (future)

		wmops(iwm) = '    '

	}

	#call crtin   # DEBUG
	return
	end

	subroutine setwav
	implicit integer*4 (i-n)

#ccc	Version date: @(#)setwav.r	2.3 08/08/85 12:49:31


	include "../common/spmaxes"   # max parameters, must be first

	include "../common/pltcnt"

	real*4	temp

	if (iptype == 1 | iptype == -1) {
		if (xlabel==' ') 
#OLD way		xlabel = 'Wavelength (!m!m)'
			xlabel = 'WAVELENGTH (!m!m)'

	} else if (iptype == 2) {
		if (wminp==0.0) {                      # case inverse waves
			if (wmaxp<0.0) wminp= -1.0e-30
			else wminp = 1.0e-30
		}
		if (wmaxp==0.0) {
			if (wminp<0.0) wmaxp= -1.0e-30
			else wmaxp = 1.0e-30
		}
		if (wmaxp < wminp) {
			temp = wminp
			wminp = wmaxp
			wmaxp = temp
		}
		if (xlabel==' ')
#OLD way		xlabel = 'Wavenumbers (cm}-1@)'
			xlabel = 'WAVENUMBERS (cm}-1@)'

	} else if (iptype == 3) {
		if (wminp==0.0) {                   # case reverse inverse waves
			if (wmaxp<0.0) wminp= -1.0e-30
			else wminp = 1.0e-30
		}
		if (wmaxp==0.0) {
			if (wminp<0.0) wmaxp= -1.0e-30
			else wmaxp = 1.0e-30
		}
		if (wmaxp > wminp) {       # make sure range is in reverse order
			temp = wminp
			wminp = wmaxp
			wmaxp = temp
		}
		if (xlabel==' ')
#OLD way		xlabel = 'Wavenumbers (cm}-1@)'
			xlabel = 'WAVENUMBERS (cm}-1@)'
	}
	if (!nhscle) {
		if (wmaxp > wminp) {
			temp = (wmaxp-wminp)*0.02
			wmaxp = wmaxp+temp
			wminp = wminp-temp
		}
		if (wmaxp < wminp) {              # reverse wavenumber case
			temp = (wminp-wmaxp)*0.02
			wmaxp = wmaxp-temp
			wminp = wminp+temp
		}
	}
	return
	end

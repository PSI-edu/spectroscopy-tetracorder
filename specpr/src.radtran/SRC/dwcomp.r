	subroutine dwcomp (ddflt, df, w0, w, d)
	implicit integer*4 (i-n)
#
# compute a new grain size (d) based on the wavelength offset (w)
#         from a given wavelength (w0) with a slope (df) and the 
#         initial grain size (ddflt).

	real*4 ddflt, df, w0, w, d

	d = ddflt/(1.0 + df *(w0 - w))

	return
	end

	subroutine wav150
	implicit integer*4 (i-n)

	include "../common/spmaxes"   # max parameters, must be first

	include "../common/blank"
	include "../common/lbl7"
	include "../common/lbl4"
	include "../common/label1"
	include "../common/label3"
	include "../common/lblg"
	include "../common/lundefs"


	real*4 apds(25)


	data    apds/   0.325,  0.350,  0.375,  0.400,  0.433,
			0.466,  0.500,  0.533,  0.566,  0.600,
			0.633,  0.666,  0.700,  0.733,  0.766,
			0.800,  0.833,  0.866,  0.900,  0.933,
			0.966,  1.000,  1.033,  1.066,  1.100/

	write(ttyout,10)
	nchans = 25
	iwch = 0
	do i = 1,25
		dataa(i) = apds(i)
	do i = 26,256
		dataa(i) = 0.0

	call wavfil(iwavfl)
	call wriwav(iwavfl)

	ititl = 'UH PGD pds photometer wavelengths'
	ihist = ititl
	call wrihed(iwavfl)
	itrol(2) = iwavfl
	return

10      format(' UH PGD wavelength assignment= pds, 25 channels',/)

	end

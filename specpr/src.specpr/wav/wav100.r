	subroutine wav100
	implicit integer*4 (i-n)
	
	include "../common/spmaxes"   # max parameters, must be first

	include "../common/blank"
	include "../common/lbl7"
	include "../common/lbl4"
	include "../common/label1"
	include "../common/label3"
	include "../common/lblg"
	include "../common/lundefs"


	real*4 ay(120)


	data    ay/     0.6203, 0.6332, 0.6441, 0.6560, 0.6679, 0.6799,
			0.6917, 0.7047, 0.7164, 0.7289, 0.7398, 0.7519,
			0.7636, 0.7756, 0.7863, 0.7990, 0.8117, 0.8224,
			0.8371, 0.8498, 0.8625, 0.8758, 0.8873, 0.8996,
			0.9118, 0.9240, 0.9362, 0.9484, 0.9613, 0.9735,
			0.9863, 0.9978, 1.0108, 1.0218, 1.0345, 1.0471,
			1.0598, 1.0725, 1.0852, 1.0978, 1.1097, 1.1227,
			1.1352, 1.1477, 1.1604, 1.1730, 1.1855, 1.1981,
			1.2106, 1.2232, 1.2357, 1.2481, 1.2615, 1.2745,
			1.2878, 1.3000, 1.3130, 1.3288, 1.3408, 1.3530,
			1.3110, 1.3322, 1.3534, 1.3754, 1.3979, 1.4199,
			1.4419, 1.4637, 1.4864, 1.5067, 1.5254, 1.5487,
			1.5689, 1.5904, 1.6119, 1.6329, 1.6552, 1.6775,
			1.6998, 1.7221, 1.7443, 1.7647, 1.7867, 1.8085,
			1.8312, 1.8532, 1.8762, 1.8992, 1.9222, 1.9450,
			1.9680, 1.9909, 2.0124, 2.0349, 2.0579, 2.0809,
			2.1041, 2.1272, 2.1504, 2.1736, 2.1970, 2.2188,
			2.2418, 2.2639, 2.2858, 2.3063, 2.3278, 2.3489,
			2.3701, 2.3916, 2.4126, 2.4346, 2.4546, 2.4746,
			2.4951, 2.5156, 2.5361, 2.5566, 2.5771, 2.5976/

	repeat {
		write(ttyout,10)
		call crtin
		i = 1
		call wjfren(i,x,il)
	} until(il==0)
	iwch = x+0.5
	nchans = 120
	do i = 1,120 {
		diwch = iwch
		dataa(i) = ay(i)+diwch*0.0001
		if (i>60) dataa(i) = ay(i)+diwch*0.0001*(2.047/1.128)
	}
	do i = 121,256
		dataa(i) = 0.00
	call wavfil(iwavfl)
	write(ititl,112)iwch
	write(ititl,112)iwch
	call wriwav(iwavfl)
	call wrihed(iwavfl)
	itrol(2) = iwavfl
	return

10      format (' type in the wavelength calibration shift in angstroms',/,
		'     (positive if further to the ir)',/,
		'     the number of channels is automatically set to 120',/)

112     format('ir cvf wavelengths shifted',i4,' angstroms')

	end

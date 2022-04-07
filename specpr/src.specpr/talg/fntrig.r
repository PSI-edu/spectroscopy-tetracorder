subroutine fntrig(m)

implicit integer*4(i-n)
	include "../common/spmaxes"   # max parameters, must be first

include "../common/blank"
include "../common/label1"
include "../common/lblg"

pi2 = 1.570796
dr = 57.29578
im = 0

	if (m == 1 | m == 2 | m == 3 | m == 4 | m == 5 | m == 6 | m == 7 | m == 8) {
		m = -2
		return
	} else if (m == 9) {
		go to 50
	} else if (m == 10) {
		go to 40
	} else if (m == 11) {
		go to 30
	} else if (m == 12) {
		go to 20
	} else if (m == 13) {
		go to 10
	} else if (m == 14) {
		xjunk=0
	} else if (m == 15) {
		im = 1
		go to 50
	} else if (m == 16) {
		im = 1
		go to 40
	} else if (m == 17) {
		im = 1
		go to 30
	} else if (m == 18) {
		im = 1
		go to 20
	} else if (m == 19) {
		im = 1
		go to 10
	} else if (m == 20) {
		im = 1
	}
#
#	 arc tan
#
	do j = 1,nchans {
		datac(j) = atan(dataa(j))
	}
	return
#
#	 arc sin
#
10  do j = 1,nchans {
		x = dataa(j)
		if (x>1.0)
		 	x = 1.0
		if (x<(-1.0))
			x = -1.0
		if (abs(x)<1.0e-18)
			x = 1.0e-18
		a = (1.0-x**2)**0.5
		if (abs(a)<1.0e-30)
			a = 1.0e-30
		c = x/a
		datac(j) = atan(c)
		if (im==1)
			datac(j) = datac(j)*dr
	}
	return
#
#	 arc cosine
#
20  do j = 1,nchans {
		x = dataa(j)
		if (x>1.0)
			x = 1.0
		if (x<(-1.0))
			x = -1.0
		if (abs(x)<1.0e-18)
			x = 1.0e-18
		a = (1.0-x**2)**0.5
		c = a/x
		datac(j) = atan(c)
		if (im==1)
			datac(j) = datac(j)*dr
	}
	return
30  do j = 1,nchans {
		x = dataa(j)
		if (im==1)
			x = x/dr
		a = sin(x)
		b = cos(x)
		if (abs(b)>=1.0e-18)
			datac(j) = a/b
		else {
			x = 1.0
			if (b<0.0)
				x = -1.0
			datac(j) = 1.0e+18*x
		}
	}
	return
40  do j = 1,nchans {
		x = dataa(j)
		if (im==1)
			x = x/dr
		datac(j) = cos(x)
	}
	return
50  do j = 1,nchans {
		x = dataa(j)
		if (im==1)
			x = x/dr
		datac(j) = sin(x)
	}
	return
	end




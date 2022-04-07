	subroutine frjuld (iy,imon,iday,julday)
	implicit integer*4 (i-n)

#ccc  name: frjuld
#ccc  version date: 5/3/85
#ccc  author(s): Rob Burtzlaff
#ccc  language: Ratfor
#ccc
#ccc  short description:
#ccc         This subroutine converts Julian day numbers to the Gregorian date
#ccc         following an algorithm in May 1984 Sky & Telescope, p. 455
#ccc
#ccc  algorithm description: see above reference.
#ccc  system requirements: none
#ccc  subroutines called: none
#ccc  argument list description:
#ccc  parameter description: none
#ccc  common description: none
#ccc  message files referenced: none
#ccc  internal variables:
#ccc  file description: none
#ccc  user command lines: none
#ccc  update information: none
#ccc  NOTES:
#ccc
#

	real*8 ab,a,b,c,d,e,m,y,da,f

	jday = julday
	da = real(jday)/10
	idb = int(jday/10)
	jday = idb
	f = da - real(idb) + 0.5
	if (f >= 1.0){
		jday = jday + 1
	}
	ab = int((jday/36524.25)-51.12264)
	a = jday + 1 + ab - int(ab/4)
	b = a + 1524
	c = int((b/365.25)-0.3343)
	d = int(365.25*c)
	e = int((b-d)/30.61)
	d = b-d-int(30.61*e)
	m = e-1
	y = c-4716
	if (e>13.5){
		m=m-12
	}
	if (m<2.5) {
		y=y+1
	}
	iy = int(y)
	imon = int(m)
	iday = int(d)
	return
	end

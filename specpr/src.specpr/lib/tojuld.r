	subroutine tojuld (iy,imon,iday,jday)
	implicit integer*4 (i-n)

#ccc  name: tojuld
#ccc  version date: 5/3/85
#ccc  author(s): Rob Burtzlaff and Roger Clark
#ccc  language: Ratfor
#ccc
#ccc  short description:
#ccc        A subroutine to convert the Gregorian date to a Julian day number
#ccc        that only works for A.D. years and is based on a algorithm found
#ccc        in the May 1984 issue of Sky & Telescope on pg. 454.
#ccc        jday is the Julian Day times ten and must have a ones digit of 5
#ccc        For example JD 2446192.5 is 5/7/1985 0hrs UT; jday=24461925
#ccc
#ccc  algorithm description:
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

	real*8 f,da

#	da = dble(iday)/10
#	idb = int(iday/10)
# this is what I think it should be (rclark)

	da = dble(iday)
	idb= iday

	f = da-dble(idb)-0.5
	itempa = 7*(iy+(imon+9)/12)
	itempb = 3*((iy+(imon-9)/7)/100 + 1)
	jday=367*iy-itempa /4- itempb / 4+275*imon/9+idb +1721029

	if (f>=0){
		jday = jday 
		return
	} else {
		jday = jday -1 
	}

# scale jday by ten and add the half day

	jday = jday *10 +5

	return
	end

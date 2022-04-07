      real function step(xmin,xmax)
	  implicit integer*4 (i-n)
      dimension is(9)
      data is/1,2,2,5,5,5,10,10,10/
#
         step1 = abs(xmax - xmin) / 8.
      stepl = alog10(step1)
#
      if (stepl .lt. 0.) {
      stepl = -stepl + 1
      istepl = stepl
     } else {
      stepl = -stepl
      istepl = stepl
      }
#
      mistpl = - istepl
      pten = 10. ** istepl
      pmten = 10. ** mistpl
#
      istep = ifix (pten * step1)
      if (istep .le. 0) istep = 1
      if (istep .gt. 9) istep = 9
#
      istep = is(istep)
      step = pmten * float(istep)
#
      return
      end

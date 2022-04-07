      subroutine shuffl(ig,a)
	  implicit integer*4 (i-n)
#
#  this routine shuffles the values in the params array used by gfit
#  control software developed at psd.  on the first call to shuffle
#  the array is made to look the way that the fitting package (harvard's
#  algoritm. on the next call it is changed back to the ti format.
#  this format allows changing the number of gaussian parameters
#  without hanging the memory location of the continuum coefficients.
#-----------------------------------------------------------------------
      dimension a(66)
#
      icount = ig * 3 + 6
      nc = 66
      if (ig .lt. 20) {
      while (nc .gt. 60) {
#
      temp = a(nc)
      a(nc) = a(icount)
      a(icount) = temp
#
      icount = icount - 1
      nc = nc - 1
      }
#
      }
      return
      end

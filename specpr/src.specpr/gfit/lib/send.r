      subroutine send(ia,ib)
	  implicit integer*4 (i-n)
      common/status/ info(252,3),idum(12)
#
      do i = 1,252 {
      info(i,ib) = info(i,ia)
      }
      return
      end

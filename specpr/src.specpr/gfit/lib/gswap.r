      subroutine gswap(ia,ib)
	  implicit integer*4 (i-n)
      common/status/ info(252,3),idum(12)
#
      do i = 1,252 {
      itemp = info(i,ia)
      info(i,ia) = info(i,ib)
      info(i,ib) = itemp
      }
      return
      end

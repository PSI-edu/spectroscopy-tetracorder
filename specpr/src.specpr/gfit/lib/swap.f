      subroutine swap(ia,ib)
      implicit integer*2 (i-n)
      common/status/ info(252,3),idum(12)
      do 23000 i = 1,252 
      itemp = info(i,ia)
      info(i,ia) = info(i,ib)
      info(i,ib) = itemp
23000 continue
      return
      end

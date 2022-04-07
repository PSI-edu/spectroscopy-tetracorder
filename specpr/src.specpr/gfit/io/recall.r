      subroutine recall(lungf,irec,ia)
	  implicit integer*4 (i-n)
      common/status/ info(252,3),idum(12)
#
      dimension ibuff(768), inf(252,3), id(12)
      equivalence (inf(1,1),ibuff(1)), (id(1),ibuff(742))
#
      nchar = 1536
	  read(1,rec=irec+1,iostat=ier) ibuff
      if (ierr != 0) go to 99
#
      if (ia .eq. 1  .or.  ia .eq. 2) {
      do i = 1,252 {
      info(i,ia) = inf(i,ia)
      }
#
      } else if (ia .eq. 3)  {
      do k = 1,3 {
      do i = 1,252 {
      info(i,k) = inf(i,k)
      }
      }
      }
#
	 return
99    if(ierr != -1)  {
          write(6,20)
20        format(1x,'recall: read error...',/)
          iflg=1
      }
101   irec= -1
      return
      end

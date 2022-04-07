#  2.26.80  dmc  wrtred  sfortran  wrtred, prtfit
#
      subroutine wrtred(x)
	  implicit integer*4 (i-n)
#
      write(6,10) x
10    format(' old value:',f15.7,/,' new value ?'/)
#
      repeat {
      call crtin
#
      ichar = 1
	call wjfren(ichar,xnew,il)
       } until (ichar .ge. 0)
      if (ichar .ne. 80) x = xnew
      return
      end

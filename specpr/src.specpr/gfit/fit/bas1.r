      real*4 function bas1(xdata,codeb,codeg,param)
#
#  writes messages on dev0 and dev1  2.15.79  dmc
#
#  subroutines from harvard's gaussian fitting program.
#  used basically unmodified.
#----------------------------------------------------------------
      implicit integer*4 (i-n)
      integer*2 codeb,codeg
      real param(66)
      bas1=0.0
      if(codeb.eq.0) return
      noff=3*codeg
      bas1=param(noff+1)
      if(codeb.eq.1) return
      do l=2,codeb {
      index=noff+l
100   bas1=bas1+param(index)*xdata**(l-1)  }
      return
      end

      subroutine prtfit(ilun)
	  implicit integer*4 (i-n)
#     common/status/ label(20),idr, ifiler, ierror, iwavel, idw,
# ifilew, itnamr(4), ititlr(20), ifilnr, itnamw(4), ititlw(20),
# ifilnw, iwprot, iwvlen, igaus, icont, iter, acc, invx, log,
# iptdel(2,10), params(66), xmin, xmax,ymin, ymax, invxp, logp,
# ifit(252), iscrat(252),  idum(12)
#
#
    include "../status"
#
      dimension gaus(3,20),ord(5)
	equivalence (gaus(1,1),params(1))
	equivalence (const,params(61))
	equivalence (ord(1),params(62))
#
#
#
      if (ilun == 12) call setspo
      write(ilun,5) const,(ord(n),n=1,5)
5     format(6x,'const',10x,'x',12x,'x ** 2',7x,'x ** 3',7x,
	 'x ** 4',7x,'x ** 5'/,
	 1x,6f13.6)
#
      write(ilun,10)
10    format(/5x,'gaus #',7x,'center',19x,'height',18x,'width')
#
      do j = 1,igaus {
      write(ilun,20) j,gaus(2,j),gaus(1,j),gaus(3,j)
20    format(8x,i2,4x,2(f15.7,10x),f15.7)
      }
#
#
      if (ilun == 12) call dumpsp
      return
      end

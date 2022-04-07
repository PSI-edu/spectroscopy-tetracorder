#  2.26.80  dmc  delpt  sfortran
#
      subroutine delpt
	  implicit integer*4 (i-n)
#
#  this routine gets the points to be deleted.
#
#--- notice that in this routine iptdel is given only 1 dim.
#
#     common/status/ label(20), idr, ifiler, ierror, iwavel, idw,
# ifilew, itnamr(4), ititlr(20), ifilnr, itnamw(4), ititlw(20),
# ifilnw, iwprot, iwvlen, igaus, icont, iter, acc, invx, log,
# iptdel(20), params(66), xmin, xmax,ymin, ymax, invxp, logp,
# ifit(252), iscrat(252),idum(12)
    include "../status"

#
	integer*2 del(20)
	equivalence (del,iptdel)

	data ihc/'c'/
	data ihe/'e'/

#
	ij=1
1	write(6,10)
10	format(////,1x,'Current Deleted Points...')
	write(6,20) del	
20	format(10i5)
	write(6,50)
50	format(//,1x,'Enter Points to be Deleted...',/)
	call crtin
	i=1
	call wjfren(i,x,il)
	if (il == ihc) {
		do j=1,20 
			del(j) = 0
		ij=1
		goto 1
	}
	if (x != 0.) {
		do k =ij,20 {
			del(k) = x
			ij = ij+1
			call wjfren(i,x,il)
			if (il !=0 | i >= 80) goto 1
			if (del(k)<0 && x<0.) {
				write(6,70) k,del(k),x
				goto 1
			}
		}
	}
	if (i >= 80 | il == ihe) return
	goto 1
70	format(/,1x,'Invalid Input...Negative number not valid here',/,
		'k=',i4,' del(k-1)=',i4,' x=',f10.2)
	return
	end

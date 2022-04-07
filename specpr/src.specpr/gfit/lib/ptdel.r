#  6.10.80  dmc  ptdel  sfortran
#
#  deletes points in the spectrum after reading them in
# xd - the input array which is one specpr data record
# out - the returned array after the points have been deleted
# nc - the number of points in out ( modified by this routine !)
#
#------------------------------------------------------------
	subroutine ptdel(xd,out,wav,nc,ip)
		implicit integer*4 (i-n)
#------------------------------------------------------
#  this routine takes the nc values in xd and copies
#  them to out.  if ip .ne. 0 then the data is compressed.
#  this means that the points with a value of delete are
#  completely dropped from the array out.  nc is updated
#  to be the actual number of data points returned.
#---------------------------------------------------------------
#
#     common/status/ label(20), idr, ifiler, ierror, iwavel, idw,
# ifilew, itnamr(4), ititlr(20), ifilnr, itnamw(4), ititlw(20),
# ifilnw, iwprot, iwvlen, igaus, icont, iter, acc, invx, log,
# iptdel(2,10), params(66), xmin, xmax,ymin, ymax, invxp, logp,
# ifit(252), iscrat(252), idum(12)
	include "../status"
#
#     dimension out(1), xd(1)
#timj -> calling routine had these dimensions
	dimension out(256),xd(384),wav(256)
	integer*2 del(20)
	equivalence (del,iptdel)
	logical range

	data delete/-1.23e+34/

	in = 1
	j = 1
	while (in<=nc && j<=10) {

		ip1 = del(j)
		ip2 = del(j+1)
		range = .false.

		if (ip2 < 0) range = .true.

		if (range) {
			ip2 = -ip2
			do k = ip1,ip2 {
				xd(128+k) = delete
#				write(6,10)k
#10				format('deleteing channel',i4)
			}
			in = ip2 + 1
		} else {
			if (ip1 > 0) {
				xd(128+ip1) = delete
#				write(6,10)ip1
				in = ip1 + 1
		
				if (ip2 > 0) {
					xd(128+ip2) = delete
#					write(6,10)ip2
					in = ip2 + 1
				}
			}
		}
		j = j + 1
	}

	nc1 = nc + 1
	do j = nc1,256 {
		xd(128+j) = delete
#		write(6,10)j
	}

	do j=1,nc
		if (wav(j)==delete) xd(128+j) = delete

	nc = 1
	if (ip!=0) {
		do j = 1,256 {
			if (xd(128+j)!=delete) {
				out(nc) = xd(128+j)
				nc = nc + 1
			}
		}
	} else {
		do j = 1,256 {
			out(nc) = xd(128 + j)
			nc = nc + 1
		}
	}

	if (nc!=257) {
		do j = nc,256 
			out(j) = delete
	}

	nc = nc - 1

	return
	end

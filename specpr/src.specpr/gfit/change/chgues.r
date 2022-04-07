      subroutine chgues
	  implicit integer*4 (i-n)
#     common/status/ label(20),idr, ifiler, ierror, iwavel, idw,
# ifilew, itnamr(4), ititlr(20), ifilnr, itnamw(4), ititlw(20),
# ifilnw, iwprot, iwvlen, igaus, icont, iter, acc, invx, log,
# iptdel(2,10), params(66), xmin, xmax,ymin, ymax, invxp, logp,
# ifit(252), iscrat(252),idum(12)
#
#
    include "../status"
#
	dimension xin(4), irv(2),iv(2)
	dimension gaus(3,20),ord(5)
	equivalence (gaus(1,1),params(1))
	equivalence (const,params(61))
	equivalence (ord(1),params(62))
#
      logical noerr, quit
      data ihh,ihc,ihw,iha,ihl,ihe,ihx/'h','c','w','a','l','e','x'/
#
	repeat {
		write(6,5) const,(ord(n),n=1,5)
5               format(6x,'const',10x,'x',12x,'x ** 2',7x,'x ** 3',7x,
	             'x ** 4',7x,'x ** 5'/,
	             1x,6f13.6)
		write(6,10)
10              format(5x,'gaus #',7x,'center',19x,'height',18x,'width')
      		for (j=1;j<=igaus;j=j+1) {
      			write(6,20) j,gaus(2,j),gaus(1,j),gaus(3,j)
20              format(8x,i2,4x,2(f15.7,10x),f15.7)
	}

	write(6,50)
50      format('  guesses to change ?'/)
        call crtin
	nchar = 1
#--------------------------------------------------------
#  quit = t means that on the first request for a change
#           after printing the table a cr will cause a return
#           to the calling program
#----------------------------------------------------------------
	quit = .true.
	repeat {
		call wjfren(nchar,x,il)
		if (nchar .ge. 80  .and. quit) break
		if(il == ihe | il == ihx) return
		quit = .false.
		if (il .ne. 0) {
			if (il .eq. ihc) k = 2
			if (il .eq. ihh) k = 1
			if (il .eq. ihw) k = 3
			if (il .eq. iha) k = 4
			if (il == ihl) goto 500
			call wjfren(nchar,x,il2)
			if (il2 .ne. 0) {
				break
			}
			ig = x
			if (ig .eq. 1) {
				write(6,210) ig
				call wrtred(gaus(1,ig))
			} else if (ig .eq. 2) {
				write(6,200) ig
				call wrtred(gaus(2,ig))
			} else if (ig .eq. 3) {
				write(6,220) ig
				call wrtred(gaus(3,ig))
			} else if (ig .eq. 4) {
				write(6,200) ig
				call wrtred(gaus(2,ig))
				write(6,210) ig
				call wrtred(gaus(1,ig))
				write(6,220) ig
				call wrtred(gaus(3,ig))
			} else if (nchar .lt. 80)  {
				write(6,100)
100                             format('  enter x1,y1,x2,y2 and gfit calculates the line : '/)
				call crtin
				ichar=1
				for (ii=1;ii<=4;ii=ii+1) call wjfren(ichar,xin(ii),il)
				if (ichar .lt. 80) {
#------------------------------------------------------
#  calculate straight line
#------------------------------------------------------
					if (xin(3) .ne. xin(1)) {
						slope = (xin(4) - xin(2)) / (xin(3) - xin(1))
					} else {
						slope = 0.
					}
					cept = xin(4) - slope * xin(3)
					const = cept
					ord(1) = slope
					ord(2) = 0.
					ord(3) = 0.
					ord(4) = 0.
					ord(5) = 0.
				}
				nchar = 81
#
			}
		} else if (nchar .lt. 80)  {
			write(6,150)
150                     format('  '//)
		}
       } until (nchar .ge. 80)

500    if (il == ihl) {
	 write(6,501)
501     format(1x,'enter value for const. ?',/)
	call crtin
	i = 1
	call wjfren(i,x1,il)
	if(i >= 80) next
	const = x1
	write(6,502)
502     format(1x,'enter x, x**2, x**3, x**4, x**5',/)
	call crtin
	i = 1
	for (ii=1;ii<=5;ii=ii+1) {
		call wjfren(i,x1,il)
		if (i >= 80) break
		ord(ii) = x1
	}
      }
#
200   format(1x,'   center:',i2,/)
210   format(1x,'   height:',i2,/)
220   format(1x,'    width:',i2,/)
#
       } until (quit)
      return
      end

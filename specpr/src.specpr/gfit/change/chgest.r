	subroutine chgest
	implicit integer*4 (i-n)
#
#     common/status/ label(40), idr, ifiler, ierror, iwavel, idw,
# ifilew, itnamr(4), ititlr(20), ifilnr, itnamw(4), ititlw(20),
# ifilnw, iwprot, iwvlen, igaus, icont, iter, acc, invx, log,
# iptdel(2,10), params(66), xmin, xmax,ymin, ymax, invxp, logp,
# ifit(252), iscrat(252), idum(12)
#
	include "../status"
	include "../../common/lbl4"
	include "../../common/alphabet"
	logical more
#data iha,ihb,ihc,ihd,ihe,ihf/'a','b','c','d','e','f'/
#data ihg,ihh,ihi,ihj,ihk,ihw,ihx/'g','h','i','j','k','w','x'/
#
#
	more = .true.
	repeat {
		call gpar

5		write(6,10)
10		format(' changes?  '/)

		call crtin
		i=1
20		call wjfren(i,x,il)
		if (i >= 80) next
		if (il == 0) goto 5
		if ( il == ihx | il==ihe) {
			more = .false.
			next
		}
		if (il == iha) {
#JAH		call getstr(i,label,ier)
			label = iopcon(i:80)
			goto 5

		} else if (il == ihb) {
			call wjfren(i,x1,il2)
			if (i >= 80) goto 5
			if (il2 == 0) goto 5
			idr = il2
			call devlun(4,idr,lunin)
			call wjfren(i,x1,il2)
			if (x1 == 0.0) goto 5
			if (i >= 80) next
			ifiler = int(x1)
			call rdtitl(lunin)
			goto 20

		} else if (il == ihw) {
			call wjfren(i,x1,il2)
			if (i >= 80) goto 5
			iwavel = int(x1)
			call readhd(iwavel,iwvlen)
			goto 20

		} else if (il == ihd) {
			call wjfren(i,x1,il2)
			if (i >= 80) goto 5
			igaus = int(x1)
			goto 20

		} else if (il == ihc) {
			call wjfren(i,x1,il2)
			if (i >= 80) go to 5
			icont = int(x1)
			goto 20

		} else if (il == ihf) {
			call wjfren (i,x1,il2)
			if (i >= 80) goto 5
			iter = int(x1)
			goto 20

		} else if (il == ihg) {
			call wjfren(i,x1,il2)
			if (i >= 80) goto 5
			acc = x1
			goto 20

		} else if (il == ihh) {
			ierror = .not.ierror
			goto 20

		} else if (il == ihi) {
			invx = .not.invx
			goto 20

		} else if (il == ihj) {
			log = .not.log
			goto 20

		} else if (il == ihk) {
			call delpt
			goto 20

		}



	} until (!more)
	return
	end

      subroutine rdspec(wavel,data,errors,nwv,ipack)
	  implicit integer*4 (i-n)

#     common/status/ label(20),idr, ifiler, ierror, iwavel, idw,
# ifilew, itnamr(4), ititlr(20), ifilnr, itnamw(4), ititlw(20),
# ifilnw, iwprot, iwvlen, igaus, icont, iter, acc, invx, log,
# iptdel(2,10), params(66), xmin, xmax,ymin, ymax, invxp, logp,
# ifit(252), iscrat(252),  idum(12)

      include "../status"
      common/label1/ buf(384)
	dimension wavel(256),data(256),errors(256)
	real buf

	call readwv(wavel,iwavel,iflg)
	call devlun(4,idr,lunin)
	call readsp(lunin,buf,ifiler,iflg)
	nwv = iwvlen
	call ptdel(buf,data,wavel,nwv,ipack)
	if(ipack == 1) {
		for(i=1;i<=256;i=i+1) errors(i) = wavel(i)
		j=1
		for(i=1;i<=iwvlen;i=i+1) {
			if (buf(i+128) != -1.23e34) {
				wavel(j) = errors(i)
				j=j+1
			}
		}
	}

	if (ierror)  {
		call readsp(lunin,buf,ifiler+1,iflg)
		nwv = iwvlen
		call ptdel(buf,data,wavel,nwv,ipack)
	}
	return
	end

#  2.26.80  dmc  pgaus  sfortran
#
#  this plots gaussian # ng  from the 'estimate' part
#  of the common area.
#--------------------------------------------------------
#
      subroutine pgaus(ng,nfull,wavel,data)
	  implicit integer*4 (i-n)
#
#     common/status/ label(20), idr, ifiler, ierror, iwavel, idw,
# ifilew, itnamr(4), ititlr(20), ifilnr, itnamw(4), ititlw(20),
# ifilnw, iwprot, iwvlen, igaus, icont, iter, acc, invx, log,
# iptdel(2,10), params(66), xmin, xmax,ymin, ymax, invxp, logp,
# ifit(252), iscrat(252), idum(12)
      include "../status"
#RED
	real*4 bas1    # function
#
      dimension wavel(256), data(256)
#
      data delete/-1.23e+34/, iestar/6954/, fudj/2.7725887/
#-----------------------------------------------------
#  write individual gaussians
#-----------------------------------------------------
      ig = iabs(ng)

      do i = 1,256 {
      data(i) = 0.
      }
#
      if (ig .gt. 0 .and.  ig .le. 20) {
#--------------------------------------------------
#  no need to call shuffle since fitting package routines
#  are not referenced
#------------------------------------------------------------
      index = 3 * ig
      height = params(index-2)
      center = params(index-1)
      width = params(index)
      write(6,1550) ig,center,height,width
1550  format(1x,'g#',i3,' c,h,w:',3f15.6/)
#
      do ix = 1,nfull {
      xdata = wavel(ix)
      temp = (xdata - center) / width
      temp = -( temp * temp)
	if ((temp*fudj) < -85) t= -85
	else { t = temp * fudj }
      tt=t
      if (t < -30) tt= -30
      temp = height * exp(tt)
#
      data(ix) = temp
      }
#
      }
      if (ig .eq. 0  .or. ng .lt. 0) {
#------------------------------------------------------------------
#  shuffle params array to be in the format of the fitting package
#  routines.  unshuffle at the end of the do . {
#-------------------------------------------------------------------
      call shuffl(igaus,params)
#
      do ix = 1,nfull {
	 xdata = wavel(ix)
	 data(ix) = data(ix) + bas1(xdata,icont,igaus,params)
#
      }
      call shuffl(igaus,params)
#
#---------------------------------------------
#     calculate fit and write it to disk
#  shuffle is necessary here also
#-----------------------------------------------
      } else if (ig .gt. 20)  {
      nwh = icont + 3 * igaus + 1
      call shuffl(igaus,params)
#
      do ix = 1,nfull {
      data(ix) = f(nwh,wavel(ix),params,icont,igaus)
#
      }
      call shuffl(igaus,params)
#
      }
#

      return
      end

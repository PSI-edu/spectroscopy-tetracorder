	subroutine getcor(ifd,xmin,xmax,ymin,ymax,xstp)
	implicit integer*4 (i-n)

#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc                   This subroutine accepts the coordinates of the
#ccc                   corners of the plot.
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc                    crtin,wjfren,getpix
#ccc  argument list description:
#ccc     arguments: ifd,xmin,xmax,ymin,ymax,xstp
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc
	include "../common/tablet"
	include "../common/lundefs"
	include "../common/alphabet"

	integer*4 iix(3),iiy(3)

#
1       write(ttyout,10)
	for(i=1;i<=3;i=i+1) {
		call getpix(ifd,iix(i),iiy(i))
		write(ttyout,100) iix(i),iiy(i)
	}
#
	theta = atan(float(iiy(3)-iiy(2)) / float(iix(3)-iix(2)))
	theta2 = atan(float(iix(1)-iix(2)) / float(iiy(1)-iiy(2)))
	rad = 1.74532e-2
	write(ttyout,45) iix(1)-iix(2),iiy(3)-iiy(2),90.0-(theta*rad+theta2*rad)
	write(ttyout,46)
	call crtin
	ichar = 1
	call wjfren(ichar,x,il)
	if (il == ihn) goto 1

	if (iix(2) == iix(1)) {
		theta = 0.0
	} else {
		theta = atan(float(iiy(3)-iiy(2)) / float(iix(3)-iix(2)))
	}
	costh = cos(theta)
	sinth = sin(theta)
	xzero = iix(2)
	yzero = iiy(2)
#
3   write(ttyout,20)
	call crtin
	ichar = 1
	call wjfren(ichar,x,il)
	if (il != 0) {
		write(ttyout,40)
		goto 3
	}
	if (ichar >= 80) goto 3
	xxmin = x
	call wjfren(ichar,x,il)
	if (il != 0 | ichar >= 80) {
		write(ttyout,40)
		goto 3
	}
	ymax = x
	call wjfren(ichar,x,il)
	if (il != 0) {
		write(ttyout,40)
		goto 3
	}
	if (ichar >= 80) goto 3
	xmin = x
	call wjfren(ichar,x,il)
	if (il != 0 | ichar >= 80) {
		write(ttyout,40)
		goto 3
	}
	ymin = x
	call wjfren(ichar,x,il)
	if (il!=0 | ichar >= 80) {
		write(ttyout,40)
		goto 3
	}
	xmax = x
	call wjfren(ichar,x,il)
	if (il != 0 | ichar >= 80) {
		write(ttyout,40)
		goto 3
	}
	yymin = x
#
	if (xxmin != xmin | yymin !=ymin) {
		write(ttyout,899)
		goto 3
	}

	write(ttyout,898)
	call crtin
	ichar = 1
	call wjfren(ichar,x,il)
	xstp = x
	if (il != 0 | ichar >= 80) xstp = xmin
	xscale = (float(iix(3) - iix(2)) / costh) / (xmax - xmin)
	yscale = (float(iiy(1) - iiy(2)) / costh) / (ymax - ymin)
	return
10      format(1x,'Enter coordinates of the upper left, lower left, ',
		/,1x,'and lower right corners of the plot:')
20      format(1x,'enter the coordinates of those points in order...',/)
40              format(1x,'--- INVALID INPUT ---')
45      format(/,' y axis off vertical: ',i7,/,
		' x axis off horizontal: ',i7,/,
		' angle between axis: ',f15.6)
46      format(' Are values acceptable? ',/)
100     format(1x,'x = ',i5,'  y = ',i5)
898     format(' enter maximum value of X...',/)
899     format(' coordinates not linear... retype')
	end

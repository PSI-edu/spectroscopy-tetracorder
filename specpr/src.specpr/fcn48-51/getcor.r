	subroutine getcor(ichk,xmin,xmax,ymin,ymax,xstp,xlow,ystp,ylow)
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
#ccc                    crtin,wjfren,tabpos
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

	dimension iix(3),iiy(3)
	integer*4 jchar

#
1       write(ttyout,10)
	do i=1,3 {
		ichk=0
		call tabpos(ichk,iix(i),iiy(i))
		if (ichk==ihx || ichk==ihe) {
			return
		}
		if (ichk==ihd) {
			write (ttyout,911)
			go to 1
		}
		write(ttyout,100) iix(i),iiy(i)
	}
	if (iix(3)-iix(2)==0 || iiy(1)-iiy(2)==0) {
		write (ttyout,914)
		go to 1
	}
#
	theta = atan(float(iiy(3)-iiy(2)) / float(iix(3)-iix(2)))
	theta2 = atan(float(iix(1)-iix(2)) / float(iiy(1)-iiy(2)))
	rad = 1.74532e-2
	write(ttyout,45) iix(1)-iix(2),iiy(3)-iiy(2),90.0-(theta*rad+theta2*rad)
	write(ttyout,46)
	call crtin
	jchar = 1
	call wjfren(jchar,x,il)
	if (il==ihe || il==ihx) {
		ichk=il
		return
	}
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
	jchar = 1
	call wjfren(jchar,x,il)
	if (il==ihe || il==ihx) {
		ichk=il
		return
	}
	if (il != 0) {
		write(ttyout,40)
		goto 3
	}
	if (jchar >= 80) goto 3
	xxmin = x
	call wjfren(jchar,x,il)
	if (il != 0 | jchar >= 80) {
		write(ttyout,40)
		goto 3
	}
	ymax = x
	call wjfren(jchar,x,il)
	if (il != 0) {
		write(ttyout,40)
		goto 3
	}
	if (jchar >= 80) goto 3
	xmin = x
	call wjfren(jchar,x,il)
	if (il != 0 | jchar >= 80) {
		write(ttyout,40)
		goto 3
	}
	ymin = x
	call wjfren(jchar,x,il)
	if (il!=0 | jchar >= 80) {
		write(ttyout,40)
		goto 3
	}
	xmax = x
	call wjfren(jchar,x,il)
	if (il != 0 | jchar >= 80) {
		write(ttyout,40)
		goto 3
	}
	yymin = x
#
	if (xxmin != xmin | yymin !=ymin) {
		write(ttyout,899)
		goto 3
	}

6	write(ttyout,897)
	call crtin
	jchar = 1
	call wjfren(jchar,x,il)
	if (il==ihe || il==ihx) {
		ichk=il
		return
	}
	if (il != 0 | jchar >= 80) {
		write(ttyout,40)
		goto 6
	}
	xlow = x
	call wjfren(jchar,x,il)
	if (il != 0 | jchar >= 80) {
		write(ttyout,40)
		goto 6
	}
	xstp = x
	if (xstp<=xlow) {
		write (ttyout,912)
		go to 6
	}
7	write(ttyout,898)
	call crtin
	jchar = 1
	if (il==ihe || il==ihx) {
		ichk=il
		return
	}
	call wjfren(jchar,x,il)
	if (il != 0 | jchar >= 80) {
		write(ttyout,40)
		goto 7
	}
	ylow = x
	call wjfren(jchar,x,il)
	if (il != 0 | jchar >= 80) {
		write(ttyout,40)
		goto 7
	}
	ystp = x
	if (ystp<=ylow) {
		write (ttyout,912)
		go to 7
	}
	
	
	if (il != 0 | jchar >= 80) xstp = xmin
	xscale = (float(iix(3) - iix(2)) / costh) / (xmax - xmin)
	yscale = (float(iiy(1) - iiy(2)) / costh) / (ymax - ymin)
	return
10      format(1x,'Digitize coordinates of the upper left, lower left, ',
		/,1x,'and lower right corners of the plot:')
20      format(1x,'Type in the x,y coordinates of upper left, lower left, ',
	      /,1x,'and lower right corners of the plot, seperated by spaces'/)
40              format(1x,'--- INVALID INPUT ---')
45      format(/,' y axis off vertical: ',i7,/,
		' x axis off horizontal: ',i7,/,
		' angle between axis: ',f15.6)
46      format(' Are values acceptable? ',/)
100     format(1x,'x = ',i5,'  y = ',i5)
897     format(' enter Minimum and Maximum value of X',/)
898     format(' enter Minimum and Maximum value of Y',/)
899     format(' coordinates not linear... retype')
911	format(' Starting over...')
912	format(' ERROR **** Maximum value is less than Minimum Value...',//)
914	format(' Incorrect Values!!! Try again...'//)
	end

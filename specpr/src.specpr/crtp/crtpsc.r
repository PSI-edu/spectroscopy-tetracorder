	subroutine crtpsc (igo,bbnd,ubnd,nchans,datac,error,wmina,wmaxa,i,dataa,iscale)
	implicit integer*4 (i-n)

#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc                   This subroutine allows the user to set the
#ccc                   horizontal and vertical scales for the crt
#ccc                   plotting routine.
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc                    crtin,rlchng,wjfren,er,eralph
#ccc  argument list description:
#ccc     arguments: igo,bbnd,ubnd,nchans,datac,error,wmina,wmaxa,i
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc
#################################################################
#                                                               #
#       this routine allows the user to set the horizontal      #
#       and vertical scale for the crt plotting routine.        #
#                                                               #
#       Author: Roger N. Clark                                  #
#       Modified:       JAH 03-01-83                            #
#                                                               #
#################################################################

	include "../common/spmaxes"   # max parameters, must be first

	include "../common/hptrm"
	include "../common/lundefs"
        include "../common/alphabet"
        include "../common/lbl7"

	real*4 datac(SPMAXCHAN),error(SPMAXCHAN),dataa(SPMAXCHAN)
	character*1 iichar
	integer*4 iscale   # = a autoscale, A= autoscale +2% range, B= autoscale, keep 0
#
	inflag=0  # inverse wavelength mode
	izflag=0  # bbnd = 0 autoscale mode izflag=1
                  # izflag = 2 scale 2% around min, max

#
#     this subroutine determines the crt plot scale (horizontal and
#     vertical axes).
#
	if (igo == 6) call eralph
	if (igo == 6) go to 6
	if (igo == 8) go to 8

	if (iscale == ihcb) izflag = 1   # autoscale with bbnd = 0 
	if (iscale == ihca) izflag = 2   # autoscale 2% around min, max

#
#     auto scale if desired
#
9	bbnd= 0.9e+30
	ubnd= -0.9e+30
	jb=nchans
	ja=1
	icount=0    # count of number of valid channels in the range
	xmax = wmaxa
	xmin = wmina
	if (xmax < xmin) {
		xmax = wmina
		xmin = wmaxa
		#write (ttyout,*) 'DEBUG: xmax < xmin',xmax, xmin
	}
	if (itrol(3)==ihn) {
		#write (ttyout,*) 'DEBUG: converting xmin, xmax to wavenumber'
		if (abs(xmax) < 0.1e-25) xmax = 0.1e-25
		if (abs(xmin) < 0.1e-25) xmin = 0.1e-25
		xtmp = xmin
		xmin = 10000.0/xmax
		xmax = 10000.0/xtmp
	}
	#write (ttyout,*) 'DEBUG: xmin= ',xmin,'  xmax=', xmax, '  x(3654)=',dataa(3654)
	do jmn= ja, jb  {
		if (datac(jmn) == -1.23e34) next
		if (dataa(jmn)>xmax || dataa(jmn)<xmin) next
		xx= datac(jmn)+ error(jmn)
		yy= datac(jmn)- error(jmn)
		if (yy < bbnd) bbnd= yy
		if (xx > ubnd) ubnd= xx
		icount = icount+1
	}
	if (icount < 1) {  # no valid channels found, so set range 0 to 2
		bbnd = 0.0
		ubnd = 2.0
	}
	if (izflag == 1) bbnd = 0.0

	if (izflag == 2) {   # autoscale range is 2% more than data range

		diff= abs(ubnd-bbnd)
		ubnd = ubnd + 0.02*diff
		bbnd = bbnd - 0.02*diff

	}
#
#     if difference between upper and lower bound is too small, default
#     to 0.0 to 2.0
#
	diff= ubnd-bbnd
	if (diff < 0.0) go to 6
	if (ubnd > 0.0) {
		ubnd= ubnd*1.02
	} else {
		ubnd= ubnd*0.98
	}

	if (bbnd > 0.0) {
		bbnd= bbnd*0.98
	} else {
		bbnd= bbnd*1.02
	}
	call er
8	if (i != ihc) go to 7
#
6	if (wmaxa <= wmina) write (ttyout,62) bbnd,ubnd
	else write (ttyout,61) bbnd,ubnd,wmina,wmaxa
	write (ttyout, 3)
	call crtin
	j= 1
#
#     decode lower and upper bound
#
	call rlchng(j, xx, il)
	inflag=0
	izflag=0
	if (j.ge.80) go to 4
#	if (il == ihn) go to 4
	if (il == ihw) go to 19
	if (il == ihn) {
		inflag=1
		go to 19
	}
	if (il == ihca) {
		call getiopconchar(j,iichar)
		if(iichar=='0') izflag=1
		if(iichar=='z') izflag=1
		if(iichar=='2') izflag=2
#		write (*,*) 'DEBUG: autoscale zero flag= ',iichar
		i= 0
		call hreset(1)
		go to 9
	}
	if (il != 0) {
		call what(j)  # error encountered
		go to 16
	}
	bbnd=xx
	call rlchng(j, xx, il)
	if (j.ge.80) go to 4
	if (il != 0) {
		call what(j)  # error encountered
		go to 16
	}
	ubnd=xx
#
	call er
	if (igrmod >= 50 & igrmod <= 53) { # X-windows case only
		call eralph
	}
	igo=0
	return
#
19	call rlchng(j,xx,il)
	if (il != 0) {
		call what(j)  # error encountered
		go to 16
	}
	call rlchng(j,yy,il)
	if (il != 0) {
		call what(j)  # error encountered
		go to 16
	}
	if (yy < xx) go to 16
	if ((yy == xx) & (xx != 0.)) go to 16
	if (inflag==1) {
		if (xx!=0) {
			xx=10000.0/xx
		} else {
			xx=0
		}
		if (yy!=0) {
			yy=10000.0/yy
		} else {
			yy=0
		}
		if (xx>yy) {
			xtmp=xx
			xx=yy
			yy=xtmp
		}
	}
	wmina=xx
	wmaxa=yy
	go to 6
4	igo=4
	call er
	if (igrmod >= 50 & igrmod <= 53) { # X-windows case only
		call eralph
	}
	return
7	igo=7
	if (igrmod >= 50 & igrmod <= 53) { # X-windows case only
		call eralph
	}
	return
16	write (ttyout,17)
	go to 6

17      format (' ** ERROR: reenter')
62      format (' Current Scale: VERTICAL=', 1pe11.4,'  ', 1pe11.4, /,
		'                HORIZ.  = Automatic')
# RED Added comma before first 1pe11.4 in second line
61      format (' Current Scale: VERTICAL=',1pe11.4, '  ', 1pe11.4, /,
		'                HORIZ.  =',1pe11.4, '  ', 1pe11.4)
3       format( /,
' To scale the plot, type in the mode (n or w) and ',
			'horizontal axis limits first.',/,
' When the vertical scale is entered, ',
			'the routine will exit to the plot.',//,
' HORIZONTAL:',/,
' type  n  and left and right hand limits in ',
				'INVERSE WAVELENGTH, or:',/,
' type  w  and left and right hand WAVELENGTH limits', /,
'          (if you type  w  only,  the program will ',
				'AUTOSCALE the limits ', /,
'          from the current wavelength set)', //,
' VERTICAL:',/,
' Type lower bound and upper bound values for ',
				'the VERTICAL AXIS, or:',/,
' type  A  to AUTO SCALE (the VERTICAL AXIS), or:',/)
      end

	subroutine doplot(pltnum)
#ccc  name: doplot
#ccc  version date: 
#ccc  author(s): Roger N. Clark and Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc
#ccc  algorithm description:
#ccc  system requirements:
#ccc  subroutines called:
#ccc  argument list description:
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc
	implicit integer*4 (i-n)

#ccc	Version:	@(#)doplot.r	2.33 6/19/87 12:12:15

	include "../common/spmaxes"   # max parameters, must be first

	include "../common/blank"
	include "../common/label1"
	include "../common/pltcnt"
	include "../common/lbl4"
	include "../common/alphabet"
	include "../common/lundefs"

	integer*4		pltnum,iplsym(15),errlin
        integer*4		sympen,icenabl
	integer*4	iy,iday,imon,ihour,ihmin,iss
	character*1		filnms(20)
	character*8     iname
	character*30	temp
	character*17	temp17
	real*4		xdown,bar,oldx,oldy,x,y,top,bottom,wtemp
	real*4          vdelta,wdelta,delta,ss

#					0...1...2...3...4...5...6...7...8...9
	data	filnms/' ',' ',' ','u','y',' ',' ','d','v','w',
				   ' ',' ',' ',' ',' ',' ',' ',' ',' ',' '/
#				   10..11..12..13..14..15..16..17..18..19

        data   iplsym/0,2,3,7,6,8,1,11,4,9,10,12,13,14,5/

#    determine sympen
        if (linetp < 100) {
	        sympen = (linetp - (linetp/10) * 10)
        }else{
	        linetp=linetp-101
	        sympen = 1
        }

                 
#
# print title, file #, and plotting options at bottom of plot
#
	if (ptsize < 0) {
		if (chans < 30) ptsize = 3
		else if (chans > 100) ptsize = 1
		else ptsize = 2
	}
	xdown = down * (3.5 + 18.5*vscale)/22.0
	if (ptsize!=0) {
		if (symtyp==0) symtyp = pltnum
                call pltsym(xdown-0.08,1.5,-ptsize,iplsym(symtyp),sympen)
	}
#   if no point put a " - " in front of title instead of a symbol
	if (ptsize==0) {
	 	call line(xdown-0.08,0.0,xdown-0.08,2.10,-linetp)	
        }
#
# print title
#
	call symbol (xdown,2.,-10,ititl,90.)
#
# get date and time and print it
#
	call frjuld (iy,imon,iday,jdateb)
	call todms(isctb,24000,ihour,ihmin,ss)
	write (temp17,103) imon,iday,iy,ihour,ihmin
	do itemp = 1, 10 {
		if (temp17(itemp:itemp) == ' ') temp17(itemp:itemp) = '0'
	}
	do itemp = 12, 16 {
		if (temp17(itemp:itemp) == ' ') temp17(itemp:itemp) = '0'
	}
	call symbol (xdown,13.2,-7,temp17,90.)
#
# print file id and rec number
#
    if (filnms(filid+1) == 'v') iname=isavt
    else if (filnms(filid+1) == 'u') iname=inmu
    else if (filnms(filid+1) == 'y') iname=inmy
    else if (filnms(filid+1) == 'd') iname=iwrkt
    else if (filnms(filid+1) == 'w') iname=iwdgt
    write(temp,100) iname,recno
	call symbol (xdown,16.7,-10,temp,90.)
#
# print options
#
	call symbol (xdown,20.7,-10,pltopt,90.)
#
# increment text position pointer
#
	down = down + 0.35/((3.5+18.5*vscale)/22.0)
#
# insert text and symbols
#
	if (txtflg) {
		for (i=1; i<=15; i = i + 1) {
	               if (textx(i)!=0.0) {	
			   if (texts(i)==-1) {
                                 call pltsym(18.0*vscale-texty(i),4.0+textx(i),
                                        -texte(i),iplsym(texta(i)),sympen)
			   }else{
				call symbol(18.0*vscale-texty(i),3.0+textx(i),
					-16,text(texts(i):texte(i)),90.0*(2-texta(i)))
			   }
		       }else i=16
		}
	}
#
# setup error bar sizes
#
	bar = ptsize
	if (bar <1.0) bar = 1.0
	bar = 240.0/bar
	if (bar>500) bar=500
	if (bar<100) bar=100
	bar = (wmaxp-wminp)/bar
#
# plot data (lines and points)
#
# find first non-deleted point

	for (i=1; i<chans && (dataa(i)==-1.23e34 || data(i)==-1.23e34);
					i = i+1)   ;  
	if (i > chans) {
		write (ttyout,101) iname,recno,'ALL POINTS DELETED'
		call crtin
		return
	}
# find first wavelength within bounds
	itmp = i
	if (iptype == 2) {  # Wavenumber mode
		write (ttyout,102) 'Wavenumber', wminp, wmaxp
		do i = itmp,chans {
			x = dataa(i)
			if (abs(x) < 0.1e-30) x = 0.1e-30
			x = 10000.0/x
			if (x >= wminp && x <= wmaxp) break
		}
	} else if (iptype == 3) {  # REVERSE Wavenumber mode
                                   # (wminp should really be a larger number)
		if (wminp < wmaxp) {
			wtemp = wminp
			wminp = wmaxp
			wmaxp = wtemp
		}
		write (ttyout,102) 'Reverse Wavenumber', wminp, wmaxp
		do i = itmp,chans {
			x = dataa(i)
			if (abs(x) < 0.1e-30) x = 0.1e-30
			x = 10000.0/x
			if (x <= wminp && x >= wmaxp) break
		}
	} else {
		do i = itmp,chans {
			if (dataa(i) >= wminp && dataa(i) <= wmaxp) break
		}
	}
	if (i > chans) {
		write (ttyout,101) iname,recno,
			'ALL POINTS DELETED or OUT OF HORIZONTAL RANGE'
		call crtin
		return
	}
# find first vertical-axis value within bounds
	itmp = i
	do i = itmp,chans {
		if (data(i)  >= vminp && data(i)  <= vmaxp) break
	}
	if (i > chans) {
		write (ttyout,101) iname,recno,
			'ALL POINTS DELETED or OUT OF HORIZ. or VERT. RANGE'
		call crtin
		return
	}
	oldx = dataa(i)
	oldy = data(i)

	if (iptype==2 || iptype==3) oldx = 10000.0/oldx

	if (errbar != 0 && linetp==0) errlin = 1
	else errlin = linetp

# check wmin < wmax
	if (iptype==3) {
		if (wminp > wmaxp) {
			wtemp = wminp
			wminp = wmaxp
			wmaxp = wtemp
		}
	}
# connect point count
	icount = 0
#
# find data point range to use to compute the minimum data point
#      separation to consider connecting lines
#
	wdelta = abs(wmaxp - wminp)
	vdelta = abs(vmaxp - vminp)
	delta = 0.0008
#
	istart = i
	do i=istart,chans {
		y = data(i)
		x = dataa(i)

		if (-1.231e34 < x && -1.229e34 > x) x = -1.23e34
		if (-1.231e34 < y && -1.229e34 > y) y = -1.23e34

		if (iptype==2 || iptype==3 && x != -1.23e34) x = 10000.0/x

		if (nolim) {
			if (y>vmaxp) y = -1.23e34
			if (y<vminp) y = -1.23e34
		}

		if (y!=-1.23e34 && x!=-1.23e34) {
			
			if (y>vmaxp) y = vmaxp
			if (y<vminp) y = vminp
			if (!(x<wminp || x>wmaxp)) {
				if (ptsize>0) {
				call pltsym(y,x,ptsize,iplsym(symtyp),sympen)
				}
				icount = icount + 1
				if (errbar != 0) {
					top = y+datab(i)
					bottom = y-datab(i)
					if (top>vmaxp) top = vmaxp
					if (bottom<vminp) bottom = vminp
					if (top<vminp) top = vminp
					if (bottom>vmaxp) bottom = vmaxp
					if (errbar == 1) {
					  call line(top,x-bar,top,x+bar,errlin)
					  call line(top,x,bottom,x,errlin)
				       	  call line(bottom,x-bar,bottom,x+bar,errlin)
					}else{
					  if ((top-bottom) >
                                              ((vmaxp-vminp)*.015/vscale)){
					  call line(top,x-bar,top,x+bar,errlin)
					  call line(top,x,bottom,x,errlin)
					  call line(bottom,x-bar,bottom,                                                                     x+bar,errlin)
					  }
					}

				}
# icenabl =1 enables connect points
# this check is used for high density plots.  The value (delta) is a
# small fraction of the plot size (especially compared to the pen size)
# so plotting points this close just waers out the pen tip, so skip it!

				icenabl = 1
				if (abs(x-oldx)/wdelta < delta &&
					abs(y-oldy)/vdelta < delta) icenabl = 0

				if (icount>=2 && conct==1 && icenabl == 1) {
							     # connect points
							     # when x>oldx
							     # & no dltd pts
					if (x>=oldx) {
						call line(oldy,oldx,y,x,linetp)
					} else {
						icount = 0
					}
				} else if (conct==2 && i > istart &&
								icenabl == 1) {
							# connect point always
					call line(oldy,oldx,y,x,linetp)
				}
				if (icenabl == 1) {
					oldy = y
					oldx = x
				}
			}
		} else {
			icount = 0
		}
	}
	return

100	format(a,' r',i5)
101	format (' WARNING: NO DATA POINTS TO PLOT!',/,
		'          data set: ',a,' r',i6,/,
                '          Reason: ',a,'.',/,
		' Press RETURN to continue.')
102	format (' ',a,' MODE:  Range =',f15.6,' to ',f15.6)
103	format (i2,'/',i2,'/',i4,' ',i2,':',i2,' ')

	end

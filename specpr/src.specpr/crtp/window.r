	subroutine window(lbnd,ubnd,xmin,xmax,diff)
	implicit integer*4 (i-q)

	include "../common/spmaxes"   # max parameters, must be first

	include "../common/hptrm"
	include "../common/lundefs"

	real*4 lbnd,ubnd,xmin,xmax,diff
	character*1 escape
	character*80 iopcon,outline
	
	escape = char(27)

# RED Initialize to 0 the following 4 vars
	x1 = 0
	x2 = 0
	y1 = 0
	y2 = 0
	icheck=0

#	axl = 56.   # original standard size
#	axh = 500.
#	ayl = 46.
#	ayh = 276.

	axl = 112.   # 2x size
	axh = 1000.
	ayl = 92.
	ayh = 552.
#
#     determine constants to scale data
#
	if (diff == 0.) diff = 0.1e-10
	dy = (ayh-ayl)/diff
	an = xmax - xmin
	if (an <= 0) an = 0.1e-10
	dx = (axh-axl)/an
	
1	call serase(0,620,1022,778)
	call movabs (0,718)
	call sb(0)
	write (outline,72) char(0)
	call gwrite(outline)
	if (icheck==0) {
		if (igrmod < 50 | igrmod > 53) {
			write (outline,172) char(0)
		} else {
			write(outline,74) char(0)
		}
		call gwrite(outline)
		write (outline,272) char(0)
		call gwrite(outline)
	} else {
		if (igrmod < 50 | igrmod > 53) {
			write (outline,73) char(0)
		} else {
			write(outline,75) char(0)
		}
		call gwrite(outline)
		write (outline,272) char(0)
		call gwrite(outline)
		call movabs(ix,iy)
	}
	call sb(0)

	
#
# Get position, and draw cross hairs
#
	if (icheck==0) {
		call gcrpos(ix,iy,xpos,ypos,xmax,xmin,lbnd,diff,iopcon,ier)
		call movabs (int(axl),iy)
		call drwabs(int(axh),iy)
		call movabs(ix,int(ayl))
		call drwabs(ix,int(ayh))
	} else {
		call gcrpos(ix2,iy2,xpos,ypos,xmax,xmin,lbnd,diff,iopcon,ier)
		call movabs (int(axl),iy2)
		call drwabs(int(axh),iy2)
		call movabs(ix2,int(ayl))
		call drwabs(ix2,int(ayh))
		call sb(0)

		ix=ix2
		iy=iy2
	}

#
# calculate x and y postion in data space
#
	if (icheck==0) {
		icheck = 1
		x1=xpos
		y1=ypos
		goto 1
	} else {
		x2=xpos
		y2=ypos
	}

	if (x2>x1) {
		xmax=x2
		xmin=x1
	} else {
		xmax=x1
		xmin=x2
	}
	if (y2>y1) {
		lbnd=y1
		ubnd=y2
	} else {
		lbnd=y2
		ubnd=y1
	}
	diff = ubnd-lbnd


72  format(26x,'Graphic Plot Scaling Routine',a1)
172 format(12x,'<cr> to enter Graphics Cursor Position as 1st corner,',a1)
74  format(12x,'use left mousebutton to enter Position as 1st corner,',a1)
272 format(12x,'                  or e to exit',a1)
73  format(12x,'<cr> to enter Graphics Cursor Position as 2nd corner,',a1)
75  format(12x,'use left mousebutton to enter Position as 2nd corner,',a1)
	return
	end

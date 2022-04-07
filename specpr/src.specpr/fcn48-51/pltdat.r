	subroutine pltdat (xmax,xmin,lbnd,diff,y,x,ylast,xlast)
	implicit integer*4 (i-n)
	include "../common/lbl5"

#ccc  version date: 08/06/87
#ccc  author(s): Noel Gorelick
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc           This subroutine plots the points 
#ccc           for the crt plotting package.
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc                    movabs,drwabs
#ccc  argument list description:
#ccc    arguments: nchans,xmax,xmin,lbnd,diff,y,x,iline
#ccc    nchans  input   number of channels to be plotted
#ccc    xmax    input   maximum x-axis value to plot
#ccc    xmin    input   minimum x-axis value to plot
#ccc    lbnd    input   lower bound of y-axis plot
#ccc    diff    input   vertical axis scale factor
#ccc    y       input   y value to plot
#ccc    x       input   x value to plot
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc


	real x, y, lbnd, xmax, xmin, diff, xlast, ylast
#
#     graph within limits (x,y) = (56,46),(56,276),(500,276),(500,46)
#
#     x and y limits of box =

	axl = 56.
	axh = 500.
	ayl = 46.
	ayh = 276.

#
#     determine constants to scale data
#
	if (diff == 0.) diff = 0.1e-36
	dy = (ayh-ayl)/diff
	an = xmax - xmin
	if (an <= 0) an = 0.1e-36
	dx = (axh-axl)/an
#
#     add 0.5 to reduce round off error
#
	aylr = ayl + 0.5
	axlr = axl + 0.5
#
#     find last point coordinate
#
#     if xlast = -9999.0 then this is the first point. 

		if (xlast == -9999.) {
			xlast = x
			ylast = y
		}

			
		yylast= ylast
		if (xlast<xmin || xlast>xmax || yylast==-1.23e34) return
		yiilst = (yylast - lbnd) * dy + aylr
		xiilst = (xlast - xmin) * dx + axlr
#
#     check to see if point is out of bounds
#
		if (yiilst > ayh) yiilst= ayh
		if (yiilst < ayl) yiilst= ayl
		if (xiilst > axh) xiilst= axh
		if (xiilst < axl) xiilst= axl
		iixlst= xiilst
		iiylst= yiilst
#
#     determine coordinate for current point
#
		yy= y
		if (x<xmin || x>xmax || yy==-1.23e34) return
		yii = (yy - lbnd) * dy + aylr
		xii = (x - xmin) * dx + axlr
#
#     check to see if point is out of bounds
#
		if (yii > ayh) yii= ayh
		if (yii < ayl) yii= ayl
		if (xii > axh) xii= axh
		if (xii < axl) xii= axl
		iix= xii
		iiy= yii
#
#     move to last point then draw vector to current point
#

	call movabs(iixlst,iiylst)
	call drwabs(iix,iiy)
#
# set xlast and ylast
#
	xlast = x
	ylast = y

	return
	end

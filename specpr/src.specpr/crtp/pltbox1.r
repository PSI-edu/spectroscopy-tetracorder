	subroutine pltbox1 (x1, y1, x2, y2, xmax, xmin, lbnd, diff)
	implicit none

#ccc  version date: 11/23/2018
#ccc  author(s): Roger Clark
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc           This subroutine plots a box using data coordinates
#ccc
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc                    movabs,drwabs,hpline
#ccc  argument list description:
#ccc    arguments:
#ccc    xmax    input   maximum x-axis value to plot
#ccc    xmin    input   minimum x-axis value to plot
#ccc    lbnd    input   lower bound of y-axis plot
#ccc    diff    input   vertical axis scale factor
#ccc    x1      input   upper left  x value of box
#ccc    y1      input   upper left  y value of box
#ccc    x2      input   lower right x value of box
#ccc    y2      input   lower right y value of box 
#ccc
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc

	include "../common/spmaxes"   # max parameters, must be first

	real*4 x1, y1, x2, y2, xmax, xmin, lbnd, diff
	real*4 axl, axh, ayl, ayh, an, dx, dy
	real*4 yii1, xii1, yii2, xii2, axlr, aylr

	integer*4 iline, iix1, iiy1, iix2, iiy2

#
#     graph within limits (x,y) = (56,46),(56,276),(500,276),(500,46)
#
#     x and y limits of box =

#       axl = 56.   # original standard size
#       axh = 500.
#       ayl = 46.
#       ayh = 276.

        axl = 112.   # 2x size
        axh = 1000.
        ayl = 92.
        ayh = 552.

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
#     set line type
#
	iline = 2  # lines only
        call hpline(iline)

#
#           x1, y1  --------- x2, y1
#                   |       |
#                   |       |
#                   |       |
#           x1, y2  --------- x2, y2
#

	# upper left corner
	yii1 = (y1 - lbnd) *dy + aylr
	xii1 = (x1 - xmin) *dx + axlr

	if (yii1 > ayh) yii1 = ayh   # checking limits
	if (yii1 < ayl) yii1 = ayl

	if (xii1 > axh) xii1 = axh
	if (xii1 < axl) xii1 = axl

	iix1 = xii1
	iiy1 = yii1

	# lower right corner
	yii2 = (y2 - lbnd) *dy + aylr
	xii2 = (x2 - xmin) *dx + axlr

	if (yii2 > ayh) yii2 = ayh   # checking limits
	if (yii2 < ayl) yii2 = ayl

	if (xii2 > axh) xii2 = axh
	if (xii2 < axl) xii2 = axl

	iix2 = xii2
	iiy2 = yii2

	call movabs(iix1, iiy1)   # move to upper left corner
	call drwabs(iix2, iiy1)
	call drwabs(iix2, iiy2)
	call drwabs(iix1, iiy2)
	call drwabs(iix1, iiy1)

	return
	end

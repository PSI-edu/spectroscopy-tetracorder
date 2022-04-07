subroutine ptplot(nchans,xmax,xmin,lbnd,diff,y,x,iline)
implicit integer*4 (i-n)
#common/lbl3/ictrl,error(256),idad,bndnrm,ixit
include "../../common/lbl3"
dimension x(256),y(256)
real lbnd
#
#     graph within limits (x,y) = (64,46),64,276),(597,276),(597,46)
#
#       iline - 0 = crosses only (default)
#               1 = line and crosses
#               2 = line only
#               3 = points only, no errors
#
#
#     x and y limits of box =
axl = 56.
axh = 500.
ayl = 46.
ayh = 276.
i = 1
#
#     determine first point to plot ignore deleted points =
#       (=-1.23e34)
#
#if (iline==0)
#    pause"iline = 0"
repeat {
    yy = y(i)
    if (yy!=-1.23e34)
        break
    i = i+1
    if (i>nchans)
        return
    }
#
#     determine constants to scale data
#
if (diff==0.)
    diff = 0.1e-36
dy = (ayh-ayl)/diff
an = xmax-xmin
if (an<=0)
    an = 0.1e-36
dx = (axh-axl)/an
#
#     determine cross size on each point = +ibar to -ibar
#
ibar = 2
if (nchans<=60)
    ibar = 3
if (nchans<=40)
    ibar = 4
if (nchans<=30)
    ibar = 6
#
#     add 0.5 to reduce round off error
#
aylr = ayl+0.5
axlr = axl+0.5
#
#     do loop: plot points
#
j = 0
itmp = i
do i = itmp,nchans {
    yy = y(i)
    if (x(i)>=xmin)
        if (x(i)<=xmax)
            if (yy!=-1.23e34) {
                yii = (yy-lbnd)*dy+aylr
                xii = (x(i)-xmin)*dx+axlr
#
#     check to see if point is out of bounds
#
                if (yii>ayh)
                    yii = ayh
                if (yii<ayl)
                    yii = ayl
                if (xii>axh)
                    xii = axh
                if (xii<axl)
                    xii = axl
                iix = xii
                iiy = yii
                j = j+1
                if (j==1)
                    call movabs(iix,iiy)
#
#     if nchans.le.30 do not connect points
#     if iline.eq.3 draw just points
#     if iline.eq.2  draw lines
#
                if (iline==3) {
                    call movabs(iix,iiy)
                    call drwabs(iix,iiy)
                    write(6,20)
#                    pause
                    }
                else {
                    if (iline==0)
                        call movabs(iix-ibar,iiy)
                    else {
#                        if (iline==1)
#                            pause"drawing lines & cross"
#                        if (iline==2)
#                            pause"drawing just lines"
                        call drwabs(iix,iiy)
                        if (iline==2)
                            next
#
#     check for the out of bounds in the y direction
#
                        if (iiy==ayl)
                            go to 10
                        if (iiy==ayh)
                            go to 10
                        call drwabs(iix-ibar,iiy)
                        }
                    call drwabs(iix+ibar,iiy)
#
                    10  call movabs(iix,iiy+ibar)
                    call drwabs(iix,iiy-ibar)
#     return to center of point if going to draw line
#     connecting points
#
                    if (iline==1)
                        call movabs(iix,iiy)
                    }
                }
    }
return
20  format(1x,"----- drawing just points")
end




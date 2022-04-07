	subroutine curves(yarray,xarray,npts,ncurve,ntype)
	implicit integer*4 (i-n)
#ccc    name:
#ccc    version date:
#ccc    author(s):
#ccc    language:
#ccc
#ccc    short description:
#ccc
#ccc    algorithm description:
#ccc    system requirements:
#ccc    subroutines called:
#ccc    argument list description:
#ccc    parameter description:
#ccc    common description:
#ccc    message files referenced:
#ccc    internal variables:
#ccc    file description:
#ccc    user command lines:
#ccc    update information:
#ccc    NOTES:
#ccc
	real*4    xarray(npts),yarray(npts,ncurve)

	if (ntype>=0) {
		do i=1,ncurve
			do j=2,npts
				call line(yarray(j,i),xarray(j),yarray(j-1,i),
					xarray(j-1),3)
	}
	if (ntype==0) return

	nt = iabs(ntype)


	do i=1,ncurve
		do j=nt,npts,nt
			if (yarray(j,i)!=-1.23e34
				 &&  xarray(j)!=-1.23e34)
					call pltsym(yarray(j,i),xarray(j),1,i-1)
	return
	end

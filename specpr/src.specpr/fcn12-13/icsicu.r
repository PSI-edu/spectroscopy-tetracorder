	subroutine icsicu (x,y,nx,bpar,c,ic,ier)
	implicit integer*4 (i-n)
#ccc  name:
#ccc  version date:
#ccc  author(s):
#ccc  language:
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
#
#-icsicu--------s/d-----library 1---------------------------------------
#
#   function            - interpolatory approximation by cubic splines
#                           with arbitrary second derivative end
#                           conditions. (modified slightly for specpr
#                           by omitting a routine which print error)
#   usage               - call icsicu(x,y,nx,bpar,c,ic,ier)
#   parameters   x      - vector of length nx containing the abscissae
#                           of the nx data points (x(i),y(i)) i=1,...,
#                           nx (input). x must be ordered so that
#                           x(i) < x(i+1).
#                y      - vector of length nx containing the ordinates
#                           (or function values) of the nx data points
#                           (input).
#                nx     - number of elements in x and y (input). nx
#                           must be >= 2.
#                bpar   - vector of length 4 containing the end
#                           condition parameters (input).
#                           2.0*spp(1)+bpar(1)*spp(2) = bpar(2),
#                           bpar(3)*spp(nx-1)+2.0*spp(nx) = bpar(4),
#                           where spp(i) = second derivative of the
#                           cubic spline function s evaluated at x(i).
#                c      - spline coefficients (output). c is an nx-1 by
#                           3 matrix. the value of the spline
#                           approximation at t is
#                           s(t) = ((c(i,3)*d+c(i,2))*d+c(i,1))*d+y(i)
#                           where x(i) <= t < x(i+1) and
#                           d = t-x(i).
#                ic     - row dimension of matrix c in the calling
#                           program (input). ic must be >= nx-1.
#                ier    - error parameter.
#                         terminal error
#                           ier = 129, ic is less than nx-1.
#                           ier = 130, nx is less than 2.
#                           ier = 131, input abscissa are not ordered
#                             so that x(1) < x(2) ... < x(nx).
#   precision           - single/double
#   language            - fortran
#-----------------------------------------------------------------------
#   latest revision     - july 29, 1974
#
	real*4          x(nx),y(nx),bpar(4),c(ic,3)
	equivalence        (dxj,yppb),(pj,sixi),(dxjp1,yppa)
	data               zero/0.0/,half/0.5/,one/1.0/,
			   two/2.0/,six/6.0/
#                                  check error conditions
	ier = 0
	nxm1 = nx-1
	if (ic < nxm1) go to 30
	if (nx < 2) go to 35
	if (nx == 2) go to 10
#                                  compute coefficients and right
#                                  hand side of the tridiagonal
#                                  system defining the second
#                                  derivatives of the spline
#                                  interpolant for (x,y)
#                                  c(j,1) = lambda(j)
#                                  c(j,2) = mu(j)
#                                  c(j,3) = d(j)
	dxj = x(2)-x(1)
	if (dxj <= zero) go to 40
	dyj = y(2)-y(1)
        do j=2,nxm1 {
		dxjp1 = x(j+1)-x(j)
		if (dxjp1 <= zero) go to 40
		dyjp1 = y(j+1)-y(j)
		dxp = dxj+dxjp1
		c(j,1) = dxjp1/dxp
		c(j,2) = one-c(j,1)
		c(j,3) = six*(dyjp1/dxjp1-dyj/dxj)/dxp
		dxj = dxjp1
		dyj = dyjp1
	}
#                                  factor the tridiagonal matrix
#                                  and solve for u
#                                  c(j,2) = u(j)
#                                  c(j,1) = q(j)
#                                  bpar(1) = lambda(1)
#                                  bpar(2) = d(1)
#                                  bpar(3) = mu(nx)
#                                  bpar(4) = d(nx)
10      c(1,1) = -bpar(1)*half
	c(1,2) = bpar(2)*half
	if (nx == 2) {
		go to 20
	}
	xjunk=0  # filler for ratfor conversion error

        do j=2,nxm1 {
		pj = c(j,2)*c(j-1,1)+two
		c(j,1) = -c(j,1)/pj
		c(j,2) = (c(j,3)-c(j,2)*c(j-1,2))/pj
	}
#                                  solve for cubic coefficients
#                                  of spline interpolant
#                                  c(j,1), c(j,2), and c(j,3)
20      yppb = (bpar(4)-bpar(3)*c(nxm1,2))/(bpar(3)*c(nxm1,1)+two)
	sixi = one/six
        do i=1,nxm1 {
		j = nx-i
		yppa = c(j,1)*yppb+c(j,2)
		dx = x(j+1)-x(j)
		c(j,3) = sixi*(yppb-yppa)/dx
		c(j,2) = half*yppa
		c(j,1) = (y(j+1)-y(j))/dx-(c(j,2)+c(j,3)*dx)*dx
		yppb = yppa
	}
	go to 9005
30      ier = 129
	go to 9005
35      ier = 130
	go to 9005
40      ier = 131
9005    return
	end

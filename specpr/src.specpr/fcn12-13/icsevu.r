	subroutine icsevu(x,y,nx,c,ic,u,s,m,ier,ideriv)
	implicit integer*4(i-n)
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
#ccc  notes:
#ccc
#
#-icsevu--------s/d-----library 1---------------------------------------
#
#   function            - evaluation of a cubic spline or the derivitive
#                           of the cubic spline.
#                           (modified for specpr by omitting routine
#                           which prints the error)
#   usage               - call icsevu(x,y,nx,c,ic,u,s,m,ier)
#   parameters   x      - vector of length nx containing the abscissae
#                           of the nx data points (x(i),y(i)) i=1,...,
#                           nx (input). x must be ordered so that
#                           x(i) < x(i+1).
#                y      - vector of length nx containing the ordinates
#                           (or function values) of the nx data points
#                           (input).
#                nx     - number of elements in x and y (input).
#                           nx must be >= 2.
#                c      - spline coefficients (input). c is an nx-1 by
#                           3 matrix.
#                ic     - row dimension of matrix c in the calling
#                           program (input). ic must be >= nx-1.
#                u      - vector of length m containing the abcissae
#                           of the m points at which the cubic spline
#                           is to be evaluated (input).
#                s      - vector of length m (output).
#                           the value of the spline approximation at
#                           u(i) is
#                           s(i) = ((c(j,3)*d+c(j,2))*d+c(j,1))*d+y(j)
#                           where x(j) <= u(i) < x(j+1) and
#                           d = u(i)-x(j).
#                m      - number of elements in u and s (input).
#                ier    - error parameter.
#                         warning error
#                           ier = 33, u(i) is less than x(1).
#                           ier = 34, u(i) is greater than x(nx).
#                ideriv - flag to calculate the derivitive.
#   precision           - single/double
#   language            - fortran
#-----------------------------------------------------------------------
#   latest revision     - august 9, 1974
#   add check for deleted output wavelength, then set output to deleted point value
#             - RNC 03/06/2015
#
    include "../common/alphabet"

	real*4          x(nx),y(nx),c(ic,3),u(m),s(m)
	data i/1/,zero/0.0/

#   initialize error parameters
	jer = 0
	ker = 0
	if (m>0) {
		nxm1 = nx-1
		if (i>nxm1) i = 1
		do k = 1,m {

		   if (u(k) < -1.0e30) {  # delete this point

				s(k) = -1.23e34
		   } else {
###################################find the proper interval
			d = u(k)-x(i)
			if (d<0) {
				while (i!=1) {
					i = i-1
					d = u(k)-x(i)
					if (d>=0) go to 10
				}
###################################warning - u(i) < x(1)
				jer = 33
				go to 30
10				if (d==0) go to 40
			} else {
				if (d==0) go to 40
				while (i<nx) {
					dd = u(k)-x(i+1)
					if (dd<zero) go to 20
					i = i+1
					d = dd
				}
###################################if u(i) > x(nx) - warning
				if (dd>zero) ker = 34
				d = u(k)-x(nxm1)
				i = nxm1
				go to 30
20				if (d==zero) go to 40
			}
30			if (ideriv!=ihd) {
				s(k) = ((c(i,3)*d+c(i,2))*d+c(i,1))*d+y(i)
			} else {
				s(k) = (3*c(i,3)*(d**2))+(2*c(i,2)*d)+c(i,1)
			}
			next 1
40			if (ideriv!=ihd) {
				s(k) = y(i)
			} else {
				s(k) = c(i,1)
			}
		   }
		}
		ier = max0(jer,ker)
	}
	return
	end




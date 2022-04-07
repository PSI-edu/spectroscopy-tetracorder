	subroutine polfit (x,y,sigmay,npts,nterms,mode,a,chisqr,array)
	implicit integer*4 (i-n)

#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc         This subroutine makes a least square fit to
#ccc         data with a polynomial curve
#ccc           y=a(1)+a(2)*x+a(3)*x**2+a(4)*x**3+..
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc          none
#ccc  argument list description:
#ccc     arguments: x,y,sigmay,npts,nterms,mode,a,chisqr
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc
#********************************************************************
# purpose:                                                          *
#     make a least-squares fit to data with a polynomial curve      *
#     y=a(1) + a(2)*x + a(3)*x**2 + a(4)*x**3 + . . .               *
#                                                                   *
#                                                                   *
# usage:                                                            *
#     call polfit (x, y, sigmay, npts, nterms, mode, a, chisqr,array)*
#                                                                   *
# description of parameters:                                        *
#  x    -array of data points for independent variable              *
#  y    -array of data points for dependent varaible                *
# sigmay-array of standard deviation for y points                   *
#  npts -number of pairs of data points                             *
# nterms-number of coefficients (degree of polynomial +1 )          *
#  mode -determines method of weighting least squares fit :         *
#        mode=1 (    errors )                                       *
#        mode=0 ( no errors )                                       *
#  a    -array of coefficients of polynomial                        *
# chisqr -reduced chi square for fit                                *
# array - 2d working array for determ.
#                                                                   *
# subroutines and function subprograms required:                    *
#     determ (array,norder)                                         *
#      evaluates the determinant of a symmetric two-dimensional     *
#      matrix of order norder for an array of nterms,nterms         *
#                                                                   *
# comments:                                                         *
#     dimension statement valid for nterms up to 10                 *
#                                                                   *
#********************************************************************

	include "../common/spmaxes"   # max parameters, must be first

	include "../common/lundefs"
	include "../common/dscrch"

#RED
	real*4 determ     # function determ
	real*4 delta

	real*8 xterm,yterm,chisq

	real*4 x(npts),y(npts),a(10),sigmay(npts)
	real*8 sumx(19),sumy(10)
	real*8 array(nterms,nterms)

	equivalence (datsc3(1),sumx)
	equivalence (datsc4(1),sumy)

	integer*4 nterms,mode,npts,nmax

#
#     *** accumulate weighted sums ***
#
	nmax=2*nterms-1

	do  n=1,nmax
		sumx(n)=0.0

	do j=1,nterms
		sumy(j)=0.0

	chisq=0.0
	ndel = 0
	do  i=1,npts {
		x1=x(i)
		y1=y(i)

		if (x1==-1.23e34 || y1==-1.23e34) {
			ndel = ndel + 1
			next
		}

		if (mode!=1)
			weight=1.0
		else {
			if (sigmay(i)==0.0) {
				sigmay(i)=1.0e18
				write(ttyout,215)
				write(ttyout,40)
			}
			weight=1.0/sigmay(i)**2
		}
		xterm=weight
		do n=1,nmax {
			sumx(n)=sumx(n) + xterm
			xterm=xterm * x1
		}
		yterm=weight * y1
		do n=1,nterms {
			sumy(n)=sumy(n) + yterm
			yterm=yterm * x1
		}
		chisq=chisq + weight * y1 **2
	}
#
#     *** construct matrices and calculate coefficients ***
#
	do j=1,nterms
		do k=1,nterms {
			n=j+k-1
			array (j,k)=sumx(n)
		}

	delta=determ (array,nterms)
	if (delta==0.0) {
		chisqr=0.0
		do j=1,nterms
			a(j)=0.0
		return
	}
	do l=1,nterms {
		do j=1,nterms {
			do k=1,nterms  {
				n=j+k-1
				array (j,k)=sumx(n)
			}
			array (j,l)=sumy(j)
		}
		if (delta==0.0) {
			delta=-1.23e34
			write(ttyout,215)
			write(ttyout,67)
		}

		a(l)=determ (array,nterms) / delta
	}
#
#     *** calculate chi square ***
#
	do j=1,nterms {
		chisq=chisq-2. *a(j) * sumy(j)
		do k=1,nterms {
			n=j+k-1
			chisq=chisq + a(j) * a(k) *sumx(n)
		}
	}
	free=npts-nterms-ndel
	if (free==0.0) {
		free=-1.23e34
		write(ttyout,215)
		write(ttyout,74)
	}
	chisqr = chisq / free
#
#     *** program end ***
	return
40	format (' error #1',/)
67	format (' error #2',/)
74	format (' error #3',/)
215	format (' *** error *** : division by zero has ocurred.',/,
	'variable reset to -1.23e34.',/)
	end

subroutine quad(a,b,c,minchn,chan1,chan2,xdata,ydata)

#
# Note: This routine originally returned coefficients
# a,b,c which were for a parabola passing through the 
# points (xdata(chanel-1),ydata(chanel-1)), (xdata(chanel),
# ydata(chanel)), and (xdata(chanel+1),ydata(chanel+1) but
# it no longer does such a sensible thing 
# It instead calculates the coefficients of the parabola
# which passes through the points (xdata(chanel-1),ydata(chanel-1)-1), 
# (xdata(chanel),ydata(chanel)-1), and (xdata(chanel+1),ydata(chanel+1)-1)
# This is done since it was discovered that for fairly shallow bands
# where the coefficient of the x squared term in the fit (i.e. a)
# gets small there was a significant amout of round off error in the
# calculation of the center wavelength of the parabola 
# (i.e. sqrt(c/a) ). The calculation then was down in double
# precison and this did not improve anything so 1.0 was finally
# subtracted from each of the continum corrected reflectances
# so that the calculations would be performed about 0.0 this seems
# to have corrected the problem  

#ccc  name:
#ccc  version date:
#ccc  author(s): M. Klejwa
#ccc  language:
#ccc
#ccc  short description:
# This routine returns the coefficients of the quadratic 
# equation which using the bottom three points in the band 
# the equations are of the form y=a*x^2+b*x+c
#
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
	include "../common/spmaxes"   # max parameters, must be first

	include "../common/lundefs"

	integer*4 minchn,chanel,chan1,chan2,chanlf,chanrt
	integer*4 i,j
	real*8 array(3,3)
	real*4  x1,x2,x3,x1sqrd,x2sqrd,x3sqrd
	real*4  y1,y2,y3
	real*4 a,b,c
	real*4 xdata(SPMAXCHAN),ydata(SPMAXCHAN)

#RED
	real*4 determ      # function determ
	real*4 coeff
	real*4 aspace
	real*4 bspace
	real*4 cspace



	x2=xdata(minchn)

	chanel=minchn-1
	while ((ydata(chanel)==-1.23e34 |
		xdata(chanel)==-1.23e34) & chanel>chan1) {
		chanel=chanel-1
	}
	chanlf=chanel
	x1=xdata(chanlf)

	chanel=minchn+1
	while ((ydata(chanel)==-1.23e34 |
		xdata(chanel)==-1.23e34) & chanel<chan2) {
		chanel=chanel+1
	}
	chanrt=chanel
	x3=xdata(chanrt)

	y1=ydata(chanlf)-1.0
	y2=ydata(minchn)-1.0
	y3=ydata(chanrt)-1.0
	
	x1sqrd=x1**2
	x2sqrd=x2**2
	x3sqrd=x3**2

# Form the matrix of the coefficients 

	array(1,1)=x1sqrd
	array(1,2)=x1
	array(1,3)=1

	array(2,1)=x2sqrd
	array(2,2)=x2
	array(2,3)=1


	array(3,1)=x3sqrd
	array(3,2)=x3
	array(3,3)=1

# Find the determinant of the coefficient matrix
	 
	coeff=determ(array,3)


# Form the matrix for the solution space of a 
# imput matrix columns 2 and 3 must be changed 
# ( Note: determ destroys its imput matrix )
	


	array(1,1)=y1
	array(1,2)=x1
	array(1,3)=1

	array(2,1)=y2
	array(2,2)=x2
	array(2,3)=1


	array(3,1)=y3
	array(3,2)=x3
	array(3,3)=1

	aspace=determ(array,3)


# Form the matrix for the solution space of b 


	array(1,1)=x1sqrd
	array(1,2)=y1
	array(1,3)=1

	array(2,1)=x2sqrd
	array(2,2)=y2
	array(2,3)=1


	array(3,1)=x3sqrd
	array(3,2)=y3
	array(3,3)=1

	bspace=determ(array,3)


# Form the matrix for the solution space of c




	array(1,1)=x1sqrd
	array(1,2)=x1
	array(1,3)=y1

	array(2,1)=x2sqrd
	array(2,2)=x2
	array(2,3)=y2


	array(3,1)=x3sqrd
	array(3,2)=x3
	array(3,3)=y3

	cspace=determ(array,3)

# Calculate the coefficients

	if (coeff != 0) {
		
		a=aspace/coeff 
		b=bspace/coeff
		c=cspace/coeff
	} else {
		write(ttyout,200)
200		format('error in calculating coefficients of',
		       ' the quadratic fit ')  
	}

return
end

subroutine dconvt(datax,datay,graphx,graphy,lbnd,diff,xmin,xmax)


#ccc  name:
#ccc  version date:
#ccc  author(s): R. Clark and M. Klejwa
#ccc  language:
#ccc
#ccc  short description:
#ccc
#
# This subroutine converts from a given set of graphics coordinates to 
# a set of data coordinates
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

#
	include "../common/alphabet"
	include "../common/hptrm"
	include "../common/lbl3"
	include "../common/lundefs"
  
  integer*4 graphx,graphy
  real*4 datax, datay, lbnd
  real*4 axl,axh,ayl,ayh
  real*4 grealx,grealy

  axl = 56.*2.0
  axh = 500.*2.0
  ayl = 46.*2.0
  ayh = 276.*2.0


#
#     determine constants to scale data
#
	if (diff == 0.) diff = 0.1e-36
	dy = (ayh-ayl)/diff
	an = xmax - xmin
	if (an <= 0) an = 0.1e-36
	dx = (axh-axl)/an
#
	grealx=graphx
	grealy=graphy
	datax=(grealx-axl)/dx+xmin
	datay=(grealy-ayl)/dy+lbnd

return
end

	subroutine planck (wav,therm,t,ier)
	implicit integer*4 (i-n)

#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc                   This subroutine computes the planck black body
#ccc                   value for wavelength (wav) and temperature (t).
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc                    none
#ccc  argument list description:
#ccc     arguments: wav,therm,t,ier
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
#     this subroutine computes the planck black body value for
#     wavelength (wav) and temperature (t).
#
#     errors=
#            -1: underflow
#             1: overflow
#             2: temp. out of range
#             3: wav out of range
#
#
# The following notes added by R. Clark 1/2/2009:
#
# basic Planck function: I = (2hc^2)/{(w^5) * [e^(hc/wkT)-1]}
#
#                      # where:  w = wavelength         meters
#                      #         T = temperature        Kelvin
# h= 6.626068*10.^-34  #         h = Planck constant    J s
# c= 299792458.        #         c = speed of light     m/s
# k= 1.38065 * 10.^-23 #         k = Boltzmann constant J/K
# e= 2.718281          #         e = base of natural log 2.718281
#
# a = h*c/k (*10^6 for microns)
#
# bt = 2*h*c*c (again length in microns) ???
#
# 2.0*h*c*c       # = J*s*(m/s)*(m/s) = Watts/m^3
#                 # divide by 1^6 for W*M^2 micron
#
# result is in watts per sq meter per micron per steradian

	bt= 1.1909e+08
	a = 1.4388e+04
	if (t<0.01 || t>1.e+12) {
		ier = 2
		return
	}
	if (wav<1.e-12 || wav>1.e+12) {
		ier = 3
		return
	}
	x= a/(wav*t)
	if (x>81) go to 999
	x= (wav**5)*(exp(x)-1)
	if (x>1.e36) go to 999
	if (x<1.e-36) go to 1001
	therm= bt/x
	if (therm>1.e36) go to 1001
	if (therm<1.e-36) go to 999
	ier=0
	return
999     ier=-1
	return
1001    ier=1
	return
	end

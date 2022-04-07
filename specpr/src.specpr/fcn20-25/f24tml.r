	subroutine f24tml(wmin,wmax)
	implicit integer*4 (i-n)

#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc                   This subroutine does actual thermal removal
#ccc                   for f24.
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc                    planck
#ccc  argument list description:
#ccc     arguments: wmin,wmax
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc

#     ***********************************************
#     *
#     * routine does actual thermal removal for f24
#     *
#     ***********************************************

	include "../common/spmaxes"   # max parameters, must be first

	include "../common/blank"
	include "../common/label1"
	include "../common/lbl3"
	include "../common/lblg"
	include "../common/therml"
	include "../common/lbl7"
	include "../common/alphabet"
	include "../common/dscrch"

	real*4 roarry(SPMAXCHAN)
	equivalence (roarry,datsc1)

	real*4 rsarry(SPMAXCHAN),result(SPMAXCHAN)
	equivalence (rsarry,datab),(result,datac),(y,rsnorm)


#     ************************************************************
#     * loop to remove thermal component; put answer into <result>
#     ************************************************************

	wmin = 1.0e36
	wmax = -1.0e36
	call wavlng(itrol(1),itrol(2),ier)
	do  i = 1,nchans {

# *** calculate planck function of object and standard ***
		call planck(dataa(i),po,potemp,ierpo)

# *** find the min and max wavelength value ***
		if (dataa(i)!=-1.23e34) {
			if (dataa(i)<wmin) wmin = dataa(i)
			if (dataa(i)>wmax) wmax = dataa(i)
		}

# *** check for deleted input points
# *** and divide by zero in numerator expression ***
# *** and if denominator is zero
		if ((data(i)==-1.23e34) | (ierpo>=1) |
		    (dataa(i)==-1.23e34) | (rsarry(i)==-1.23e34) |
		     (roarry(i)==-1.23e34)) {
			result(i) = -1.23e34
			if (ictrl==ihe) error(i) = 0.0
			next
		}

		if (data(i)*rsarry(i) < 1.0e-36) {
			result(i) = -1.23e34
			if (ictrl==ihe) error(i) = 0.0
			next
		}
		therm =  ((dist**2 * po * (1-rsarry(i))) / (data(i)*rsarry(i)))

		result(i) = roarry(i) * (therm +1.0)

		if (ictrl == ihe) error(i) = error(i) * (therm + 1.0)
	}
	return
	end

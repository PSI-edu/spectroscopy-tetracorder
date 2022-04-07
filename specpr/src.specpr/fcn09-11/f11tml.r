	subroutine f11tml(wmin,wmax)
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

#     ***********************************************
#     *
#     * routine does actual thermal removal for f11
#     *
#     ***********************************************

	include "../common/spmaxes"   # max parameters, must be first

	include "../common/blank"
	include "../common/label1"
	include "../common/lbl3"
	include "../common/lblg"
	include "../common/therml"
	include "../common/lbl7"
	include "../common/lundefs"
	include "../common/alphabet"
	include "../common/dscrch"

	real*4 roarry(SPMAXCHAN)
	equivalence (roarry,datsc1)

	real*4 rsarry(SPMAXCHAN),result(SPMAXCHAN)
	equivalence (rsarry,datab),(result,datac)

#     ************************************************************
#     * loop to remove thermal component; put ancer into <result>
#     ************************************************************

	wmin = 1.0e36
	wmax = -1.0e36
	distsq = dist * dist
	call wavlng(itrol(1),itrol(2),ier)
	do  i = 1,nchans {

# *** calculate planck function of object and standard ***

		call planck(dataa(i),po,potemp,ierpo)
		call planck(dataa(i),ps,pstemp,ierps)

# *** find the min and max wavelength value ***
		if (dataa(i)!=-1.23e34) {
			if (dataa(i)<wmin) wmin = dataa(i)
			if (dataa(i)>wmax) wmax = dataa(i)
		}
# *** normalize ro and rs ***
		if (roarry(i)!=-1.23e34) ro = roarry(i) * ronorm
		else ro=-1.23e34
		if (rsarry(i)!=-1.23e34) rs = rsarry(i) * rsnorm
		else rs=-1.23e34

# *** compute f ***
		if (data(i)!=0.0 & data(i)!=-1.23e34) f = data(i) / distsq
		else f=-1.23e34

# *** check for deleted input points
# *** and divide by zero in numerator expression ***
# *** and if denominator is zero
		if ((data(i)==-1.23e34) | (rs<=1.0e-36) | (f<=1.0e-36) |
		    (ierpo>=1) | (ierps>=1) | ((1.0-po/f)<=1.0e-36) |
		    (ro==-1.23e34) | (rs==-1.23e34) | (f==-1.23e34)) {
			result(i) = -1.23e34
			if (ictrl==ihe) error(i) = 0.0
			next
		}
# *** check if po is zero (to simplify equation) ***

		rs1 = 1.0 - rs
		if (ierpo == -1) {
			xnumer = ro
			denom = 1.0
		} else {
			pof = po / f
			xnumer = ro - pof
			denom = 1.0 - pof
		}

# *** check if 0 < (1 - rs) < 1 ***
		if (rs1 <= 1.0e-36) {
			write(ttyout,10) i
			rs1 = 0.0
		} else if (rs1 > 1) {
			write(ttyout,20) i
			rs1 = 1.0
		}

#  *** check if have zero in product term in numerator ***
		if ((ro<=1.0e-36) | (ierps==-1)) {
			result(i) = xnumer / denom

# *** end of finding special cases; plug in numbers ***
		} else {
			result(i) = (xnumer+((ro/rs)*rs1*(ps/f)))/denom
		}
	}
	return
10      format (' *** error. channel',i4,' : reflectance',
		' of standard is greater than one ***'/)
20      format (' *** error. channel',i4,' : reflectance',
		' of standard is negative ***'/)
	end

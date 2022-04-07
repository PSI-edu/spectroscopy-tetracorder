	subroutine stdev(isumav,idltz,inumd,error2,js)
	implicit integer*4 (i-n)
#cc  version date: 06/01/83
#cc  author(s): Roger Clark & Jeff Hoover
#cc  language:  Fortran
#cc
#cc  short description:
#cc                   This subroutine calculates the standard deviation
#cc                   if the errors were not included. Includes the
#cc                   errors for average as well as the sum.
#cc  algorithm description: none
#cc  system requirements:   none
#cc  subroutines called:
#cc                    ertyp
#cc  argument list description:
#cc     arguments: isumav,idltz,inumd,error2,js
#cc  parameter description:
#cc  common description:
#cc  message files referenced:
#cc  internal variables:
#cc  file description:
#cc  user command lines:
#cc  update information:
#cc  NOTES:
#cc

	include "../common/spmaxes"   # max parameters, must be first

	include "../common/lundefs"
	include "../common/blank"
	include "../common/lbl3"
	include "../common/lblg"
	include "../common/alphabet"

	integer*4 ier

	real*4       data(SPMAXCHAN), error2(SPMAXCHAN)
	integer*4   inumd(SPMAXCHAN)

	do j= 1, js {
		read (addlun,rec=j+1,iostat=ier)data
		call ertyp('stdev',idev,ier)
		do i= 1, nchans {
			if (idltz==1 && data(i)==0.0) next
			if (data(i)==-1.23e34) next
			xnum= float(inumd(i))
			if (xnum < 0.1e-36) xnum= 0.1e+36
			sum= datab(i)/xnum
			if (abs(sum) < 0.1e-30) sum = 0.0
			if (abs(data(i)) < 0.1e-30) data(i) = 0.0
			s= sum- data(i)
			if(s > 0.1e+16)  s= 0.1e+16
			if(s < -0.1e+16)  s= -0.1e+16
			if (abs(s) < 0.1e-16) s= 0.0
			dataa(i)= dataa(i) + s*s
		}
	}
#
#     if errors were not included from each file, calculate the
#     standard deviation of the summed data as the error
#
	if (idad==2) go to 290
	do i=1,nchans {
		sig = dataa(i)
		xn = float(inumd(i))
		xn1 = xn-1.
		if (xn1 <= 0.1e-36) xn1= 0.1e+36
		error(i) = sig/(xn1)
		if (xn <= 0.1e-36) xn=0.1e+36
		error(i) = (error(i)**0.5)/(xn**0.5)
		error2(i) = error(i)
		if (isumav==ihs) error(i) = error(i)*xn
		if (xn==0.1e+36) error(i) = 0.0
	}
   290	return
	end

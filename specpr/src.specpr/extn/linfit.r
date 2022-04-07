	subroutine linfit(m)
	implicit integer*4(i-n)
#ccc  version date: 06/01/83
#ccc  author(s): roger clark & jeff hoover
#ccc  language:  fortran
#ccc
#ccc  short description:
#ccc                   this subroutine calculates the fitted line to the
#ccc                   airmass versus log intensity data.
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc                    dread,erored
#ccc  argument list description:
#ccc     arguments: m
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  notes:
#ccc

	include "../common/spmaxes"   # max parameters, must be first

	include "../common/blank"
	include "../common/label1"
	include "../common/xtnct"
	include "../common/lundefs"

	integer*4 ier

double precision sum,sumx,sumy,sumx2,sumxy,sumy2,xi,yi
double precision weight,delta,x
#
#     this subroutine calculates the fitted line to the airmass versus
#     log intensity data
#
sum = 0.
sumx = 0.
sumy = 0.
sumx2 = 0.
sumxy = 0.
sumy2 = 0.
do i = 1,nfile {
    read(addlun,rec=i+1,iostat=ier)data
    call ertyp('linfit',addlun,ier)
    xi = air(i)
    if (data(m)!=100000.) {
        if (data(m)<(-37.0)) data(m) = -37.0
        if (data(m)>37.0) data(m) = 37.0
        yi = data(m)
	    do j = 2,11
	        if (idel(i,j)==m) go to 900
	    if (idel(i,1)!=-1) {
	        weight = 1.
	        sum = sum+weight
	        sumx = sumx+weight*xi
	        sumy = sumy+weight*yi
	        sumx2 = sumx2+weight*xi*xi
	        sumxy = sumxy+weight*xi*yi
	        sumy2 = sumy2+weight*yi*yi
	    }
    }
900   continue
}
delta = sum*sumx2-sumx*sumx
dlta = delta
if (abs(dlta)<=0.1e-30) {
    datab(m) = -0.00001
    write(ttyout,10)m
    datac(m) = data(m)
    dataa(m) = 0.
} else {
#
#     datac= log intensity at zero airmass
#     datab= slope: attenuation (factors of 10 per airmass)
#     dataa= goodness of fit: 0= no fit, -1= perfrct fit
#
    datac(m) = (sumx2*sumy-sumx*sumxy)/delta
    datab(m) = (sumxy*sum-sumx*sumy)/delta
    x = dsqrt(delta*(sum*sumy2-sumy*sumy))
    xx = x
    if (abs(xx)<0.1e-30) x = 0.1e-30
    dataa(m) = (sum*sumxy-sumx*sumy)/x
    if (abs(dataa(m))>1.0) dataa(m) = 0.0
    if (datac(m)>36.) datac(m) = 36.
    if (datac(m)<(-36.)) datac(m) = -36.
    if (abs(datab(m))<0.00001) datab(m) = -0.00001
    if (datab(m)>10.) datab(m) = 10.
    if (datab(m)<(-10.)) datab(m) = -10.
}
return

10  format('least squares analysis failed on channel',i4)

end

	subroutine fnalg(i)
	implicit integer*4 (i-n)

	include "../common/spmaxes"   # max parameters, must be first

	include "../common/blank"
	include "../common/lbl3"
	include "../common/lbl6"
	include "../common/alphabet"
	include "../common/label1"
	include "../common/lblg"

#
#     absolute value
#
    if (i==24){
        do j = 1,nchans
        	datac(j) = abs(dataa(j))
	}
#
#     integer value
#
    else if (i==25) {
        do j = 1,nchans
        	datac(j) = aint(dataa(j))
	}
#
#     fractional value
#
    else if (i==26) {
        do j = 1,nchans
        	datac(j) = dataa(j)-aint(dataa(j))
	}
    else if (i==27) {
        ictrl = ihe
        call devlun(4,idv1,elun)
        call finfil(ifl1+1,elun,2,ier)
        do j = 1,nchans {
	        if (dataa(j)==(-1.23e34)) datac(j) = -1.23e34
	        if (datab(j)==(-1.23e34)) error(j) = -1.23e34
        	if (datac(j)!=-1.23e34) {
	            if (abs(dataa(j))!=0.0)
		            datac(j) = 1.0/dataa(j)
	            else
	               	datac(j) = 0.0

	            if (abs(datab(j))!=0.0)
		            error(j) = datab(j)/(dataa(j)*dataa(j))
	            else
		            error(j) = 0.0
	        }
        }
    }
#
#     1/x
#
    else if (i==6) {
        do j = 1,nchans {
        	if (abs(dataa(j))<1.e-36)
        	    datac(j) = 1.e36
        	else
        	    datac(j) = 1.0/dataa(j)
		}
	}
    else {
        i = -2
	}
    return
    end

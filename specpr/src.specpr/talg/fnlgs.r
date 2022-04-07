	subroutine fnlgs(i)
	implicit integer*4 (i-n)

	include "../common/spmaxes"   # max parameters, must be first

	include "../common/blank"
	include "../common/label1"
	include "../common/lblg"

#
#     exp (dataa)
#
    if (i==2) {
        do j = 1,nchans {
        	if (dataa(j)==-1.23e34) datac(j) = -1.23e34
        	else if (dataa(j)>82.893) datac(j) = -1.23e34
        	else if (dataa(j)<(-82.893)) datac(j) = -1.23e34
        	else datac(j) = exp(dataa(j))
        }			 
	}
#
#     natural ln (dataa)
#
    else if (i==3) {
        do j = 1,nchans {
        	if (dataa(j)==-1.23e34) datac(j)=-1.23e34
	        else if (dataa(j)<1.0e-36) datac(j) = -1.23e34
	        else datac(j) = alog(dataa(j))
		}
	}
#
#     log base 10 (dataa)
#
    else if (i==4) {
        do j = 1,nchans {
        	if (dataa(j)==-1.23e34) datac(j)=-1.23e34
        	else if (dataa(j)<1.0e-36) datac(j) = -1.23e34
        	else datac(j) = alog10(dataa(j))
		}
	}
#
#     10**(dataa)
#
    else if (i==5) {
        do j = 1,nchans {
    	    if (dataa(j)==-1.23e34) datac(j)=-1.23e34
    	    else if (dataa(j)>36.0) datac(j) = -1.23e34
    	    else if (dataa(j)<(-36.0)) datac(j) = -1.23e34
    	    else if (abs(dataa(j))<1.0e-36) dataa(j) = 0.0
    	    else datac(j) = 10.0**(dataa(j))
    	}
    }
    else {
        i = -2
	}
    return
    end

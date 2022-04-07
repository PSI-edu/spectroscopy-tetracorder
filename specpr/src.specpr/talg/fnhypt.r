	subroutine fnhypt(i)
	implicit integer*4 (i-n)

	include "../common/spmaxes"   # max parameters, must be first

	include "../common/blank"
	include "../common/label1"
	include "../common/lblg"


    do j = 1,nchans {
        x = dataa(j)
        if (x==-1.23e34) datac(i) = x
        else {
	        if (x>80.0) x = 80.0
        	if (x<(-80.0)) x = -80.0
        	if (i==21) datac(j) = (exp(x)+exp(-x))/2.0
        	else if (i==22) datac(j) = (exp(x)-exp(-x))/2.0
        	else if (i==23) datac(j) = (exp(x)-exp(-x))/(exp(x)+exp(-x))
        }
    }
    return
    end

	subroutine fnpowr(i,x)
	implicit integer*4 (i-n)

	include "../common/spmaxes"   # max parameters, must be first

	include "../common/blank"
	include "../common/label1"
	include "../common/lblg"


#
#     x**dataa
#
    if (i==8) {
        do j = 1,nchans {
    	    if (x>=1.0e-36) {
    	        yx = x
    	        cx = abs(dataa(j))
    	        a = abs(cx*alog(yx))
    	        if (a<=82.893) {
    	        	datac(j) = x**dataa(j)
    		        next 1
    	        }
    	        else if (dataa(j)>=0.0) {
    		        datac(j) = 1.0e+36
    		        next 1
    	        }
    	    }
    	    datac(j) = 0.0
        }    
    }
#
#     dataa**x
#    
    else if (i==7) {
        do j = 1,nchans {
	        if (dataa(j)>=1.0e-36) {
	            yx = dataa(j)
	            cx = abs(x)
	            a = abs(cx*alog(yx))
    	        if (a<=82.893) {
    	        	datac(j) = dataa(j)**x
    	        	next 1
    	        }
    	        else if (x>=0.0) {
    	        	datac(j) = 1.0e+36
    	        	next 1
    	        }
    	    }
    	    datac(j) = 0.0
        }
    }
    else { 
        i = -2
    }
    return
    end

	function determ10(array,norder)
	implicit integer*4 (i-n)
	real*8 save
	integer*4 norder

	include "../common/spmaxes"   # max parameters, must be first

	include "../common/lundefs"
	include "../common/dscrch"

	real*8 array(10,10)
#
#
#     *** calculate the determinant of a square matrix ***
#     *** this routine destroys the input matrix .  dimension ***
#     *** statement valid for norder up to ten. ***
#

	determ10 = 1
	do k = 1,norder {
#        *** interchange columns if diagonal element is zero ***
		if (array(k,k)==0) {
			do j = k,norder
				if (array(k,j)!=0)
					go to 10
			go to 20
10			do i = k,norder {
				save = array(i,j)
				array(i,j) = array(i,k)
				array(i,k) = save
			}
			determ10 = -determ10
		}
#
#     ***  subtract row k from lower rows to get diagonal matrix ***
#
		determ10 = determ10*array(k,k)
		if (k<norder) {
			k1 = k+1
			do i = k1,norder
				do j = k1,norder {
					if (array(k,k)==0) {
						array(k,k) = -1.23e34
						write(ttyout,40)
						write(ttyout,30)
					}
#
					array(i,j) = array(i,j)-array(i,k)*array(k,j)/array(k,k)
				}
		}
	}
	return
20  determ10 = 0
	return
30  format(1x," error #4",/)
40  format(1x,"*** error *** - division by zero has occured.",/,
		   5x,"variable has been reset to -1.23e34.",/)
end




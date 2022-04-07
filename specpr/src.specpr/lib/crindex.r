	integer*4 function crindex(str,charr)
#ccc
#ccc	Author JAH
#ccc    modified to not address str(0). R. Clark 4/1/85
#ccc	
	character*(*) 	str
	character*1		charr

	for (i=len(str); i>0; i=i-1) 
		if (str(i:i)==charr || i==1) {
			crindex=i
			return
		}

	end

	subroutine comprs (ia)
	implicit integer*4 (i-n)
#######################################################

#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc                   This subroutine compresses the blanks
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc                    none
#ccc  argument list description:
#ccc     arguments: ia,n.ifl
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc
##############################################################
	character*(*) ia
	integer*2 nblnks

	nblnks = 0
	n = len(ia)

	for(i=1;i<=n;i=i+1) {
		if (ia(i:n) == ' ') break;
		if (ia(i:i) == ' ') {
		    nblnks = nblnks + 1
		    if (i==n) break 
		    do j = i,n - 1
		    	ia(j:j) = ia(j+1:j+1)
			i=i-1;
		}
	}
	if (nblnks==0) return

	ia(n-nblnks+1:n) = ' '

	return
	end

	subroutine catn(dest,src1,n)
	implicit integer*4 (i-n)
#ccc    name: catn
#ccc    version date: 8/2/83
#ccc    author(s): Jeff Hoover
#ccc    language: RATFOR
#ccc
#ccc    short description: This routine concatenates an exponential
#ccc            scale factor of the form '(10}n@)' to the string src1
#ccc            and places the resulting string in dest.
#ccc
#ccc    algorithm description:
#ccc    system requirements:
#ccc    subroutines called:
#ccc    argument list description:
#ccc    parameter description:
#ccc    common description:
#ccc    message files referenced:
#ccc    internal variables:
#ccc    file description:
#ccc    user command lines:
#ccc    update information:
#ccc    NOTES:
#ccc

#RED
	integer*4 lnb    # function lnb

	character*(*)   dest,src1
	character*10    expt

	if (n!=0) {
		if (n<-10) {
			 write(expt,"('(10}',i3,'@)')") n
			 n=9
		} else if (n>=1&n<=9) {
			 write(expt,"('(10}',i1,'@)')") n
			 n=7
		} else {
			write(expt,"('(10}',i2,'@)')") n

			n=8
		}
		dest = src1(1:lnb(src1)) // expt
	} else
		dest = src1(1:lnb(src1))


	return
	end

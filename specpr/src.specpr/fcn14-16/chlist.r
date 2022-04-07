	subroutine chlist(i,x,ic)
	implicit integer*4 (i-n)

#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc                   This subroutine lists channels for f14.
#ccc  algorithm description: none

#ccc  system requirements:   none
#ccc  subroutines called:
#ccc                    wjfren
#ccc  argument list description:
#ccc     arguments: i,x,ic
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc

#     ************************************
#     *
#     * routine for f14 to list channels
#     *
#     ************************************

	include "../common/spmaxes"   # max parameters, must be first

	include "../common/blank"
	include "../common/lbl4"
	include "../common/lbl3"
	include "../common/lundefs"
	include "../common/alphabet"
	include "../common/label1"


	call wjfren(i,x1,ic1)
	if (i >= 80) return
	call wjfren(i,x2,ic2)

#     *** check for soft or hard exit ***
	if (ic1==ihe || ic2==ihe) {
		ic = ihe
		return
	} else if (ic1==ihx || ic2==ihx) {
		ic = ihx
		return

#     *** check for illegal entry ***
	} else if ((ic1 != 0) | (ic2 != 0) |
	    (x1 <= 0.0) | (x1 > maxchn) |
	    ((x2 < x1) & (i < 80)) |
	    ((x2 <= 0.0) & (i < 80)) |
	    (x2 > maxchn)) {
		write(ttyout,30)
		return
	}

#     *** list data ***
	i1 = x1
	i2 = x2
	if (i2 == 0) i2 = i1
	write(ttyout,50)
	write(ttyout,60) (i,datac(i),i=i1,i2)

#     *** list errors ***
	if (ictrl == ihe) {
		write(ttyout,90)
		write(ttyout,60) (i,error(i),i=i1,i2)
	}
	return
30      format(' illegal entry. reenter last entry.'/)
50      format(' * data *')
60      format(64(4(i5,1x,1pe11.4,2x)/))
90      format(/' * errors to previous file *')
	end

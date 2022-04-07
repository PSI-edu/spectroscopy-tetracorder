	subroutine chdata(i,x,ic)
	implicit integer*4 (i-n)

#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc                   This subroutine chasnges data for f14.
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc                    wjfren
#ccc  argument list description:
#ccc      arguments: i,x ic
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc

#     **********************************
#     *
#     * routine for f14 to change data
#     *
#     **********************************

	include "../common/spmaxes"   # max parameters, must be first

	include "../common/blank"
	include "../common/lbl4"
	include "../common/lbl3"
	include "../common/lundefs"
	include "../common/alphabet"
	include "../common/label1"



	ichan = x
	call wjfren(i,x1,ic1)
	i1 = i
	call wjfren(i,x2,ic2)
	i2 = i
	if (ic1==ihx || ic2==ihx) {
		ic = ihx
		return
	}

#     *** check for illegal entry ***
	if ((ic1!=0 && ic1!=ihe) ||
	   ichan<=0 || ichan>maxchn ||
	   ic2!=0) {
		write(ttyout,10)
		return
#     *** change both data and error ***
	} else if (ic1==0 && ic2==0 &&
		 ic!=ihe && ((ictrl!=ihe && i1<80) ||
			     (ictrl==ihe && i2<80))) {
		datac(ichan) = x1
		if (ictrl == ihe) error(ichan) = x2
#     *** change errors only ***
	} else if (ictrl==ihe && ic==ihe && i1!=80) {
		error(ichan) = x1
	} else if (ictrl==ihe && ic1==ihe && i2!=80) {
		error(ichan) = x2
#     *** found bad input ***
	} else {
		write(ttyout,10)
	}
	return
10      format(' illegal entry. reenter last entry.'/)
	end

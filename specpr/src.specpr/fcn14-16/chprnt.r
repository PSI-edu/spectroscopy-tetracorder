	subroutine chprnt(i,x,ic)
	implicit integer*4 (i-n)

#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc                   This is a routine for f14 to print data on line
#ccc                   printer.
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc                    wjfren,filinp,crtin,setspo,pdata,dumpsp
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

#     *************************************************
#     *
#     * routine for f14 to print data on line printer
#     *
#     *************************************************

	include "../common/spmaxes"   # max parameters, must be first

	include "../common/blank"
	include "../common/lbl6"
	include "../common/lbl4"
	include "../common/label1"
	include "../common/lbl3"
	include "../common/lundefs"
	include "../common/alphabet"

	logical errs


	call wjfren(i,x,ic)
	if ((ic == ihe) | (ic == ihx)) return
	if (ic != ihd) {
		write(ttyout,10)
		return
	}

#     *** print data ***
	ifile = ifl1
	itmp = ifl1
	call filinp(idv1,itmp,errs,ic)
	if ((errs) | (ic == ihx)) {
		write(ttyout,30)
		call crtin
		return
	}
	lpline = 60
	call setspo
	call pdata(0,1,1,1,ifl1,lpline)
	call dumpsp

#     *** print errors ***
	if (ictrl == ihe) {
		ierfl = itmp + 1
		call filinp(idv1,ierfl,errs,ic)
		if ((errs) | (ic == ihx)) {
			write(ttyout,30)
			call crtin
			return
		}
		lpline = 60
		call setspo
		call pdata(0,1,1,1,ierfl,lpline)
		call dumpsp
		ifl1 = ifile
	}

	return
10      format(' illegal entry. reenter last entry.'/)
30      format(' f14 error. fill out specpr bug sheet.',
	     ' press return to hard exit.'/)
	end

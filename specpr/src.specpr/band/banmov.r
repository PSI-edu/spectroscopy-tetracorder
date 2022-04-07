	subroutine banmov(ibnrm1,ibnrm2,nchans,iopcon)
	implicit integer*4(i-n)
#cc  version date: 06/01/83
#cc  author(s): roger clark & jeff hoover
#cc  language:  ratfor
#cc
#cc  short description:
#cc                   this subroutine allows the user to move the
#cc                   band limits.
#cc  algorithm description: none
#cc  system requirements:   none
#cc  subroutines called:
#cc                    crtin,wjfren
#cc  argument list description:
#cc     arguments: ibnrm1,ibnrm2,nchans,iopcon
#cc  parameter description:
#cc  common description:
#cc  message files referenced:
#cc  internal variables:
#cc  file description:
#cc  user command lines:
#cc  update information:
#cc  notes:
#cc

	include "../common/spmaxes"   # max parameters, must be first

	include "../common/lundefs"
	include "../common/alphabet"
	character*80 iopcon,outline



	repeat {
#
#   this routine allows the user to move the band limits
#
	    write(outline,20) char(0)
	    call gwrite(outline)
	    write(outline,21) char(0)
	    call gwrite(outline)
	    call crtin
	    if (nchans<2) nchans = 2
	    i = 1
	    do j = 1,2 {
		call wjfren(i,t,it)
		if (it==ihx) go to 900
		if (it==ihe) go to 900
		if (it!=0 || t<=0 || t>nchans) go to 10
		if (j==1) ibnrm1 = t
		if (j==2) ibnrm2 = t
	    }
	    return
10          write(outline,30) char(0)
	    call gwrite(outline)
	    j = 1
	}
900   continue
	iopcon(1:1) = 'x'
	return

20  format(' enter starting and ending',a1)
21  format(' channel nos:   aaaa bbbb',a1)
30  format(' **error**',a1)

	end

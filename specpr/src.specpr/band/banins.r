	subroutine banins(ibnrm1,ibnrm2,delcnt,iopcon)
	implicit integer*4(i-n)
#cc  version date: 06/01/83
#cc  author(s): roger clark & jeff hoover
#cc  language:  ratfor
#cc
#cc  short description:
#cc      this subroutine allows the user to reinsert deleted
#cc                   data points
#cc  algorithm description: none
#cc  system requirements:   none
#cc  subroutines called:
#cc                    crtin,wjfren
#cc  argument list description:
#cc     arguments: ibnrm1,ibnrm2,delcnt,iopcon
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
	include "../common/blank"
	include "../common/alphabet"
	include "../common/label1"

	integer*4 delcnt
	character*80 outline

#
#  allows user to reinsert deleted data points
#
	write(outline,20)char(0)
	call gwrite(outline)
	write(outline,21)char(0)
	call gwrite(outline)
	call crtin
	i = 1
	k = 0
	repeat {
	    call wjfren(i,t,it)
	    if (it==iha) {
		delcnt = maxchn
		return
	    }
	    if (t==0&&it==0) break 1
	    if (it!=0) {
		k1 = k+1
		write(outline,30)k1,char(0)
		call gwrite(outline)
	    } else {
		ich = t
		if (ich<ibnrm1||ich>ibnrm2) {
		    write(outline,40)ich,char(0)
		    call gwrite(outline)
		}
		else {
		    k = k+1
		    if (datab(ich)==0) {
			write(outline,50)ich,char(0)
			call gwrite(outline)
			k = k-1
		    }
		    if (datab(ich)==1) datab(ich) = 0
		}
	    }
	}
	delcnt = delcnt-k
	call crtin
	return


20  format(' enter channel nos. to be',a1)
21  format(' reinserted (a=all)',a1)
30  format(' point ',i5,' contains alphabetic',a1)
40  format(' ch ',i5,' is not within band',a1)
50  format(' note: ch ',i5,' wasn''t deleted',a1)

	end

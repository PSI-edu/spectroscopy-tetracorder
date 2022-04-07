	logical function gbldel(ii)
	implicit integer*4 (i-n)
#ccc  name:
#ccc  version date:
#ccc  author(s):
#ccc  language: RATFOR
#ccc
#ccc  short description: This routine handles deletions for all spectra
#ccc					plotted.
#ccc
#ccc  algorithm description:
#ccc  system requirements:
#ccc  subroutines called: crtin,wjfren,dltpts
#ccc  argument list description: ii --- dummy argument
#ccc  parameter description:
#ccc  common description: pltcnt 
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc

	include "../common/spmaxes"   # max parameters, must be first

	include "../common/pltcnt"
	include "../common/lundefs"
	include "../common/alphabet"
	include "../common/label1"

	real*4 x

	repeat {
		do i=1,maxchn
			gdel(i) = 0

		write(ttyout,10) maxchn

		call crtin
		i = 1
		call wjfren(i,x,il)
		if (il==ihe || il==ihx) {
			gbldel=.false.
			return
		}
		if (il==0) {
			gbldel=.true.
			return
		}
		if (il!=ihd) next

		n = maxchn
		j = 0
		call dltpts(i,j,gdel,n,il)
		if (il==ihe || il==ihx) {
			gbldel=.false.
			return
		}
		else {
			gbldel=.true.
			return
		}
	}

10 format (' Type   d   to DELETE CHANNELS FROM ALL',
			' SPECTRA plotted',/,
'               or press <return> to continue.',/,
' then type in the channels (max=',i5,') on the same line',/,
' and succeeding lines',/,
' Type   c  to CONTINUE when you are through.'/)
	end

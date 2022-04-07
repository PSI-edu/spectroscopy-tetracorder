	subroutine merger(ic)
	implicit integer*4 (i-n)

#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc                   This subroutine does actual merge for f13.
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc                    crtin,wjfren
#ccc  argument list description:
#ccc     arguments: ic
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc
#     *************************************
#     *
#     * routine does actual merge for f13
#     *
#     *************************************

	include "../common/spmaxes"   # max parameters, must be first

	include "../common/blank"
	include "../common/lbl4"
	include "../common/label1"
	include "../common/lbl3"
	include "../common/lblg"
	include "../common/f13com"
	include "../common/lundefs"
	include "../common/alphabet"
	include "../common/dscrch"

	real*4 dataf(SPMAXCHAN)
	equivalence (dataf,datsc1)


10  write(ttyout,20)
20  format(' ENTER data set (a or b) and channel number or',
	' channel limits.',/,
	' (74 characters max.)       Example: a1 30 b31 64 a65 72',/,
	' or enter  e  or x  to exit.',/,1x,'i',72('-'),'i')
	call crtin
	jline= iopcon
	i = 1
	j = 0

#     *** get file ***
30      call wjfren(i,x,ic)
	if ((ic == ihe) | (ic == ihx)) return
	if (i >= 74) return
	if ((ic == iha) | (ic == ihb)) go to 50
	write(ttyout,40)
40      format(' ERROR: ILLEGAL CHARACTER OR BOUND. ',
		' Reenter entire line.'/)
	go to 10

#     *** get lower bound or channel number ***
50      if (ic == iha) indx = 1
	if (ic == ihb) indx = 2
	call wjfren(i,x,ic)
	if ((ic == ihe) | (ic == ihx)) return
	if (i >= 74) return
	if (((ic == iha) | (ic == ihb) | (ic == 0)) &
	   ((x > 0.0) & (x <= nchans))) go to 60
	write(ttyout,40)
	go to 10

60      j = j + 1
	ilow = x
	if (j == nchans +1) return
	if (j <= nchans) go to 65
	itmp = nchans + 1
	write(ttyout,63) itmp
63      format('WARNING: trying to merge MORE THAN ALLOWED number of',
	  ' channels.',/,'Quit merging at',i6,' channels',/)
	return
65      if (indx == 1) datac(j) = dataa(ilow)
	if (indx == 2) datac(j) = datab(ilow)
	if ((ictrl == ihe) & (indx == 1)) error(j) = dataf(ilow)
	if ((ictrl == ihe) & (indx == 2)) error(j) = data(ilow)
	if ((ic == iha) | (ic == ihb)) go to 50
	call wjfren(i,x,ic)
	if ((ic == ihe) | (ic == ihx)) return
	if (i >= 74) return
	if (((ic == iha) | (ic == ihb)) &
	   (x == 0.0)) go to 50
	if (((ic == iha) | (ic == ihb) | (ic == 0)) &
	   ((x > 0.0) & (x <= nchans))) go to 70
	write(ttyout,40)
	go to 10

#     *** get upper bound if any and merge
70      if (x >= ilow) go to 80
	write(ttyout,40)
	go to 10
80      if ((x == ilow) & (ic != 0)) go to 50
	if (x == ilow) go to 30
	ilow = ilow + 1
	ihigh = x
90      j = j + 1
	if (j == nchans +1) return
	if (j <= nchans) go to 95
	itmp = nchans + 1
	write(ttyout,63) itmp
	return
95      if (indx == 1) datac(j) = dataa(ilow)
	if (indx == 2) datac(j) = datab(ilow)
	if ((ictrl == ihe) & (indx == 1)) error(j) = dataf(ilow)
	if ((ictrl == ihe) & (indx == 2)) error(j) = data(ilow)
	if ((ilow >= ihigh) & (ic != 0)) go to 50
	if (ilow >= ihigh) go to 30
	ilow = ilow + 1
	go to 90
	end

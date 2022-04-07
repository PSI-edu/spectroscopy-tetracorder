	subroutine chgin3 (inxt)
	implicit integer*4 (i-n)

#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc               This subroutine displays and changes following
#ccc               header information:
#ccc               band normalization factor,no. of revolutions
#ccc               ( or scans ), scan time, total integrating time
#ccc               no of runs, weighted no. olf runs, time per object
#ccc               per half chop
#ccc
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc                    er,scrn02,wjfren,rlchng,intchg
#ccc  argument list description:
#ccc      argument: inxt
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc
########################################################################
#                                                                      #
#    this routine displays & changes the following header information: #
#         band normalization factor, no. of revolutions (or scans),    #
#         scan time, total integrating time, no. of runs, weighted     #
#         no. of runs, time per object per half chop                   #
#                                                                      #
#    subroutines used:  er,scrn02,wjfren,rlchng,intchg                 #
#                                                                      #
########################################################################

	include "../common/spmaxes"   # max parameters, must be first

	include "../common/label1"
	include "../common/lbl4"
	include "../common/label3"
	include "../common/lundefs"
	include "../common/alphabet"


	repeat {
		inxt=0
		call scrn02
		call crtin
		i=1
		call wjfren (i,x,il)
		if (x!=0) {
			call what(i)
			next
		}
		if (il==ihx || il==ihe || il==ihr || il==ihg) {
			inxt = il
			return
		}

		if (x==0 && il==0) {
			inxt=4
			return
		}
		ier = 1
		while (ier==1) {
			i = 1
			if (il == iha) {
				write(ttyout,5)
				call crtin
				i = 1
				call rlchng(i,xnrm,ier)
			} else if (il == ihb) {
				write(ttyout,10)
				call crtin
				i = 1
				call intchg(i,revs,ier)
			} else if (il == ihc) {
				write(ttyout,20)
				call crtin
				i = 1
				call rlchng(i,scatim,ier)
			} else if (il == ihd) {
				write(ttyout,30)
				call crtin
				i = 1
				call rlchng(i,timint,ier)
			} else if (il == ihf) {
				write(ttyout,40)
				call crtin
				i = 1
				call intchg(i,nruns,ier)
			} else if (il == ihw) {
				write(ttyout,50)
				call crtin
				i = 1
				call intchg(i,iwtrns,ier)
			} else if (il == ihh) {
				write(ttyout,60)
				call crtin
				i = 1
				call intchg(i,itimch,ier)
			} else {
				ier = 1
			}
		}
	}
5       format (' enter band normalization factor:',/,
		' xx.xxxx or x.xxxx e xx',/)
10      format (' enter no. of revolutions (or scans):',/)
20      format (' enter scan time (seconds):',/)
30      format (' enter total integrating time:',/)
40      format (' enter no. of runs:',/)
50      format (' enter weighted no. of runs:',/)
60      format (' enter time observed per object per half chop',
			' (milliseconds):',/)
	end

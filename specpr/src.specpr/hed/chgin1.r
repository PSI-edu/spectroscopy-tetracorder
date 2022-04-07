	subroutine chgin1 (inxt)
	implicit integer*4 (i-n)

#ccc  version date: 05/03/85
#ccc  author(s): Roger Clark
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc         This subroutine displays and changes the following
#ccc         header information: bit flags
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc                    er,scrn00,wjfren,clrbit, setbit, chkbit
#ccc  argument list description:
#ccc        argument: inxt
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc
#######################################################################
#                                                                     #
#   this routine displays & changes the following header information  #
#                                                                     #
#######################################################################

	include "../common/spmaxes"   # max parameters, must be first

	include "../common/label1"
	include "../common/lbl4"
	include "../common/label3"
	include "../common/lundefs"
	include "../common/alphabet"

	character*80 iopsav

	character*1     idummy
	integer*4       decl(3)
	integer*2 	ibit

	ibscrn = 1

	repeat {
		inxt=0
		call scrn00(ibscrn)
		call crtin
		i=1
		call wjfren (i,x,il)
		if (x < 0 | x > 32) {
			call what(i)
			next
		}
		if (il==ihx || il==ihe || il==ihr || il==ihg) {
			inxt = il
			return
		}
		if (x==0 && il==0 && i >= 79) {
			if (ibscrn == 1) {
				ibscrn = 2
				next
			}
			if (ibscrn == 2) {
				ibscrn = 3
				next
			}
			inxt=2
			return
		}
		i = 1
		while (i < 80) {
			call wjfren (i,x,il)
			if (x < 0 | x > 32) {
				call what(i)
				go to 810
			}
			ibit = x
			if (il == 0) {
				if (i == 80) next
				call wjfren (i,x,il)
				if ( x != 0) {
					call what(i)
					go to 810      # error
				}
			}
			if (il == ihc) {               #clear bit
				if (ibit == 0) go to 820  # unchangeable
				if (ibit == 1) {	  # text bit, give warn
					iopsav = iopcon
					write (ttyout, 910)
					itemp = 1
					call crtin
					call wjfren(itemp, x, iltmp)
					if (iltmp == ihy) {    # ok to set bit
						call clrbit(icflag, ibit)
						call initdt
						iopcon = iopsav
						next
					}
				}
				call clrbit (icflag, ibit)
			}
			if (il == ihs) {               # set bit
				if (ibit == 0) go to 820  # unchangeable
				if (ibit == 1) {	  # text bit, give warn
					iopsav = iopcon
					write (ttyout, 900)
					itemp = 1
					call crtin
					call wjfren(itemp, x, iltmp)
					if (iltmp == ihy) {    # ok to set bit
						call setbit(icflag, ibit)
						itext = ' '
						iopcon = iopsav
						next
					}
					next
				}
				call setbit (icflag, ibit)
			}
		}
		next
				
			
810		if (il==ihr) break
		write(ttyout,811)
		next

820		write (ttyout, 821) ibit
		next
	}
#
811     format (' *** error ***',/)
821	format (' Bit ', i1, ' is NOT changeable',/)
900	format (' TEXT BIT to be set: WARNING, all text data will ',
		'be initialized',/,
		' type y for yes to proceed.  Any other response will',
		' negate action',/)
910	format (' TEXT BIT to be cleared: WARNING, all data will ',
		'be initialized',/,
		' type y for yes to proceed.  Any other response will',
		' negate action',/)
	end

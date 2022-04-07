	subroutine scrn00 (ibscrn)
	implicit integer*4 (i-n)

#ccc  version date: 5/2/85
#ccc  author(s): Roger Clark
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc         This subroutine prints the bit flags of
#ccc         information in the information change routine.
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:    none
#ccc  argument list description:
#ccc      arguments: none
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc
#################################################################
#       this routine prints the bits in iflags                  #
#       header information in the information change            #
#       routine.                                                #
#################################################################

	include "../common/spmaxes"   # max parameters, must be first

	include "../common/blank"
	include "../common/label1"
	include "../common/lundefs"
	include "../common/alphabet"
	include "../common/lbl6"

	integer*2 bitnum, chkbit
	integer*4 itest(32)

	do i = 0, 31 {
		bitnum = i
		itest(i+1) = chkbit(icflag, bitnum)
	}
	if (ibscrn < 1) ibscrn = 1
	if (ibscrn > 3) ibscrn = 1

	call eralph
	write (ttyout, 10) idv1, ifl1, ititl, itest

	if (ibscrn == 1) {
		write (ttyout, 20) (itest(i), i=1,11)
	}
	if (ibscrn == 2) {
		write (ttyout, 25) (itest(i), i=12,22)
	}
	if (ibscrn == 3) {
		write (ttyout, 30) (itest(i), i=23,32)
	}

	write (ttyout, 40)

	return

10	format (5x, 'Header Information Display and Change: Bit flags',
/, 5x, a, i5, 3x, a, /, 16x,            ' 1   5   10   15   20   25   30 ',/,
			 16x,            ' |   |    |    |    |    |    | ',/,
			  5x, 'Bit Flags= ', 32i1 )

20	format (' Bit  0: ', i1,
' continuation data flag; not changable',/,
' Bit  1: ', i1,
		' Text=1, Date=0 flag. WARNING, changing bit ',
                   /, 32x,             'DESTROYS existing data in memory.',/,
' Bit  2: ', i1,
' Error bars are located in following data set: 1=yes.',/,
' Bit  3: ', i1,
' Positions correspond to RA and DEC (=0), Long, Lat (=1)',/,
' Bit  4: ', i1,
' Time a correspond to UT (=0) or Civil (=1)',/,
' Bit  5: ', i1,
' Time b correspond to UT (=0) or Civil (=1)',/,
' Bit  6: ', i1, ' unused' ,/,
' Bit  7: ', i1, ' unused' ,/,
' Bit  8: ', i1, ' unused' ,/,
' Bit  9: ', i1, ' unused' ,/,
' Bit 10: ', i1, ' unused' )

25	format (' Bit 11: ', i1, ' unused' ,/,
' Bit 12: ', i1, ' unused' ,/,
' Bit 13: ', i1, ' unused' ,/,
' Bit 14: ', i1, ' unused' ,/,
' Bit 15: ', i1, ' unused' ,/,
' Bit 16: ', i1, ' unused' ,/,
' Bit 17: ', i1, ' unused' ,/,
' Bit 18: ', i1, ' unused' ,/,
' Bit 19: ', i1, ' unused' ,/,
' Bit 20: ', i1, ' unused' ,/,
' Bit 21: ', i1, ' unused' )

30	format (' Bit 22: ', i1, ' unused' ,/,
' Bit 23: ', i1, ' unused' ,/,
' Bit 24: ', i1, ' unused' ,/,
' Bit 25: ', i1, ' unused' ,/,
' Bit 26: ', i1, ' unused' ,/,
' Bit 27: ', i1, ' unused' ,/,
' Bit 28: ', i1, ' unused' ,/,
' Bit 29: ', i1, ' unused' ,/,
' Bit 30: ', i1, ' unused' ,/,
' Bit 31: ', i1, ' unused' )

40	format (' type the bit number and c to clear (=0) ',
			'or s to set (=1) bit',/,
' type  g  to exit to crt plot,  e  to soft exit ',
			'with write, no plot,',/,
'       x  to hard exit,  r  to return to beginning ',
			'of header info routine',/,
'          press return to display next ',
			'screen of information',/)

      end

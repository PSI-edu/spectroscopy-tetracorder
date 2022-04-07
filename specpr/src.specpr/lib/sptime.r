	subroutine sptime
#ccc  name: sptime
#ccc  version date: 
#ccc  author(s): Wendy Calvin
#ccc  language:  Ratfor
#ccc
#ccc  short description: set process time and date (of last operation)
#ccc
#ccc  algorithm description:
#ccc  system requirements:
#ccc  subroutines called: time
#ccc  argument list description: none
#ccc  parameter description:
#ccc  common description: none
#ccc  message files referenced: none
#ccc  internal variables:
#ccc  file description: none
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc	sets date as Julian day * 10: and returns # of secs since
#ccc						0:00 hours UT
#ccc

	implicit integer*4 (i-n)
	include "../common/spmaxes"   # max parameters, must be first

	include "../common/label1"
	integer*2 itmp

	itmp = 4
	call setbit(icflag,itmp)  # time is UT

# jdatea = Julian day data last processed
# iscta = UT when data was last processed.

	call jdatim(jdatea,iscta)
	iscta = iscta * 24000		#scale UT secs

	return
	end

	subroutine sptimb
#ccc  name: sptime
#ccc  version date: 
#ccc  author(s): Wendy Calvin
#ccc  language:  Ratfor
#ccc
#ccc  short description: set process and data acquisition times and dates
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

	call sptime     # set jdatea and iscta

	jdateb = jdatea
	isctb = iscta

	itmp = 5
	call setbit(icflag,itmp)    # time is UT

	return
	end

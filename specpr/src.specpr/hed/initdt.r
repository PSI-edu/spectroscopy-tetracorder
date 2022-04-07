	subroutine initdt
	implicit integer*4 (i-n)

#ccc  name: initdt
#ccc  version date: 5/14/85
#ccc  author(s): Roger N Clark
#ccc  language: Ratfor
#ccc
#ccc  short description: initializes all data in specpr label1 common
#ccc			assuming bit 1 is clear. Only ititl, icflag,
#ccc			and usernm are not initialized.
#ccc
#ccc  algorithm description:
#ccc  system requirements: none
#ccc  subroutines called: chkbit
#ccc  argument list description: none
#ccc  parameter description: none
#ccc  common description: label1, lundefs
#ccc  message files referenced: none
#ccc  internal variables:
#ccc  file description: none
#ccc  user command lines: none
#ccc  update information: none
#ccc  NOTES:
#ccc

	include "../common/spmaxes"   # max parameters, must be first

	include "../common/label1"
	include "../common/lundefs"

	integer*2 chkbit, ibit

	ibit = 1
	if (chkbit(icflag,ibit) == 1) {
		write (ttyout, 100)
		return
	}

	iscta = 0
	isctb = 0
	jdatea = 0
	jdateb = 0
	istb = 0
	isra = 0
	isdec = 0
	itchan = 0
	irmas = 0
	revs = 0
	iband(1) = 0
	iband(2) = 0
	irwav = 0
	irespt = 0
	irecno = 0
	itpntr = 0
	ihist = ' '
	mhist = ' '
	nruns = 0
	siangl = 0
	seangl = 0
	sphase = 0
	iwtrns = 0
	itimch = 0
	xnrm = 0.0
	scatim = 0.0
	timint = 0.0
	tempd = 0.0

	do i = 1, maxchn {
		data(i) = 0.0
	}
	return

100	format (' ERROR: initdt called but text bit set', /)
	end

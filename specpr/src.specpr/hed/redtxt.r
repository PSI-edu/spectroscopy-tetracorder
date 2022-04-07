	subroutine redtxt(ier)
	implicit integer*4 (i-n)

#ccc  name: redtxt
#ccc  version date: 5/10/85
#ccc  author(s): Roger N. Clark
#ccc  language: Ratfor
#ccc
#ccc  short description:
#ccc
#ccc  algorithm description:
#ccc  system requirements:
#ccc  subroutines called:
#ccc  argument list description: ier (return error value.  =0 is ok)
#ccc  parameter description:
#ccc  common description: label1, lundefs
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc

	include "../common/spmaxes"   # max parameters, must be first

	include "../common/label1"
	include "../common/lundefs"
#RED
	integer*4 lnb     # function lnb

	character*1024 ibuft
	character*1 newlin

	ier =0
	rewind (luntxt, iostat=ier)
	if (ier != 0) {
		write (ttyout, 510)
		call crtin
		return
	}

	newlin = char(10)     # new line character
	itst = 1
	itend = 1
	nlines = 0
	n = maxtxt

# readline:
2	read (luntxt, 10, end=1000, err=500) ibuft

	is = 1
	ie = lnb(ibuft)

	if (ie <= 1 & ibuft(1:1) == ' ') {       # no text on this line
		itext(itst:itend) = newlin
		itst = itst+1
		itend = itst
		go to 2		# readline
	}

	if (itst+ie+1 >n) {            # line too big, quit
		itext(itst:n) = ' '
		write (ttyout, 300) nlines
		itxtch = itst -1
		return
	}

	nlines = nlines +1
	itend = ie + itst
	itext(itst:itend) = ibuft(is:ie)

	itext(itend+1:itend+1) = newlin
	itxtch = itend+1

	if (itxtch >= n) return

	itst = itend +2

	itend = itst
	go to 2		# readline

500	write (ttyout, 501)
	call crtin
1000	return

10	format (a)

300	format (1x, i6, ' lines of text read')

501	format (' READ ERROR on text file. Press return to continue',/)

510	format (' REWIND ERROR in text file. Press return to continue',/,
		' no text read.  Exiting read routine',/)

	call crtin

	end

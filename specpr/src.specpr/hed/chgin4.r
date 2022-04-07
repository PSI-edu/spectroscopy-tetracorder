	subroutine chgin4 (inxt)
	implicit integer*4 (i-n)

#ccc  version date: 5/6/85
#ccc  author(s): Roger Clark 
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc               This subroutine displays and changes following
#ccc               header information:
#ccc               record pointer to wavelengths, record pointer to resolution
#ccc               record pointer to text, angle of emission, angle of
#ccc               incidence, and temperature.
#ccc            
#ccc
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc                    er,scrn03,wjfren,rlchng
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

	include "../common/spmaxes"   # max parameters, must be first

	include "../common/label1"
	include "../common/lbl4"
	include "../common/label3"
	include "../common/lundefs"
	include "../common/alphabet"


	repeat {
		inxt=0
		intsph = 2000000000
		call scrn03
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
			inxt=5
			return
		}
		ier = 1
		while (ier==1) {
			i = 1
			if (il == iha) {
				write(ttyout,5)
				call crtin
				i=1
				call wjfren(i,x,ier)
				if (x < 1 | x > maxchn) {
					write (ttyout, 200)
					ier = 1
					next
				}
				itchan = x
			} else if (il == ihb) {
				write(ttyout,10)
				call crtin
				i=1
				call wjfren(i,x,ier)
				if (x < 0 | x > maxrec) {
					write (ttyout, 200)
					ier = 1
					next
				}
				irwav = x
			} else if (il == ihc) {
				write(ttyout,20)
				call crtin
				i=1
				call wjfren(i,x,ier)
				if (x < 0 | x > maxrec) {
					write (ttyout, 200)
					ier = 1
					next
				}
				irespt = x
			} else if (il == ihd) {
				write(ttyout,30)
				call crtin
				i=1
				call wjfren(i,x,ier)
				if (x < 0 | x > maxrec) {
					write (ttyout, 200)
					ier = 1
					next
				}
				itpntr = x
			} else if (il == ihh) {			# angle of emission
				write(ttyout,40)
				call crtin
				i=1
				call wjfren(i,x,ier)
				if (ier == ihi && x == 0.0) {
					seangl = intsph
					next
				}
				if (ier != 0) {
					call what (i)
					ier=1
					next
				}
				if (x > 90.0 | x < -90.0) {
					write (ttyout, 200)
					ier = 1
					next
				}
				seangl = int(dble(x) * 3600.0d0 * 6000.0d0)
			} else if (il == ihf) {			# angle of incidence
				write(ttyout,50)
				call crtin
				i=1
				call wjfren(i,x,ier)
				if (ier == ihi && x == 0.0) {
					siangl = intsph
					next
				}
				if (ier != 0) {
					call what (i)
					ier=1
					next
				}
				if (x > 90.0 | x < -90.0) {
					write (ttyout, 200)
					ier = 1
					next
				}
				ier = 0
				siangl = int(dble(x) * 3600.0d0 * 6000.0d0)
			} else if (il == ihi) {			# phase angle
				write(ttyout,60)
				call crtin
				i=1
				call wjfren(i,x,ier)
				if (ier == ihi && x == 0.0) {
					sphase = intsph
					next
				}
				if (ier != 0) {
					call what (i)
					ier=1
					next
				}
				if (x > 180.0 | x < -180.0) {
					write (ttyout, 200)
					ier = 1
					next
				}
				sphase = int(dble(x) * 3600.0d0 * 1500.0d0)
			} else if (il == iht) {			# temperature
				write(ttyout,70)
				call crtin
				i=1
				call wjfren(i,x,ier)
				ier = 0
				if (ier != 0) {
					call what (i)
					ier=1
					next
				}
				if (x < 0.0) {
					write (ttyout, 200)
					ier = 1
					next
				}
				tempd = x
			} else {
				ier = 1
			}
		}
	}
5       format (' enter total number of channels in spectrum:', /)
10      format (' enter wavelength record pointer:',/)
20      format (' enter resolution record pointer:',/)
30      format (' enter text record pointer:',/)
40      format (' enter angle of emission (decimal degrees):',/,
		'       integrating sphere = i',/)
50      format (' enter angle of incidence (decimal degrees):',/,
		'       integrating sphere = i',/)
60      format (' enter phase angle (decimal degrees):',/,
		'       integrating sphere = i',/)
70      format (' enter temperature:',/)
200	format (' ERROR, out of range. reenter',/)
	end

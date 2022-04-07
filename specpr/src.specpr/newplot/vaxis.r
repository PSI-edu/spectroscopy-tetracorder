	logical function vaxis (ii)
	implicit integer*4(i-n)

#ccc  name: vaxis
#ccc  version date: 8/15/83
#ccc  author(s):J.A.Hoover
#ccc  language: RATFOR
#ccc
#ccc  short description: This routine reads the vertical axis label and
#ccc            plotting limits.
#ccc
#ccc  algorithm description:
#ccc  system requirements:
#ccc  subroutines called: wjfren,crtin
#ccc  argument list description:
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc


	include "../common/spmaxes"   # max parameters, must be first

	include "../common/lbl4"
	include "../common/pltcnt"
	include "../common/alphabet"
	include "../common/lundefs"

	character*60	tempc
	real*4		x

	repeat {
		write(ttyout,30)
		write(ttyout,40)
		write(ttyout,50)
		write(ttyout,60)
		write(ttyout,70)
		write(ttyout,80)
		write(ttyout,90)
		write(ttyout,100)
		write(ttyout,110)
		write(ttyout,120)
		write(ttyout,130)

		call crtin
		i = 1
		call wjfren(i,x,il)
		if (il==ihe || il==ihx) {
			vaxis=.false.
			return
		}
		if (il==0 && x>0. && x<11.) break 1
	}


	if (x == 1.) write(tempc,40)
	if (x == 2.) write(tempc,50)
	if (x == 3.) write(tempc,60)
	if (x == 4.) write(tempc,70)
	if (x == 5.) write(tempc,80)
	if (x == 6.) write(tempc,90)
	if (x == 7.) write(tempc,100)
	if (x == 8.) write(tempc,110)
	if (x == 9. | x == 10.) {
		if (x==10.) ilog = .true.
		write(ttyout,140)
		call crtin
		tempc(6:60) = iopcon(1:60)
	}
	vlabel = tempc(6:60)

	repeat {
		write (ttyout,200)
		call crtin
		i = 1
		call wjfren(i,x,il)
		if (il==ihx || il==ihe) {
			vaxis=.false.
			return
		}
		if (il!=0 || (ilog && x<=0.0)) next
		if (ilog) x = alog10(x)
		vminp = x

		call wjfren(i,x,il)
		if (il==ihx || il==ihe) {
			vaxis=.false.
			return
		}
		if (il!=0 || (ilog && x<=0.0)) next
		if (ilog) x = alog10(x)
		vmaxp = x

		if (vmaxp<=vminp) next
		else {
			vaxis=.true.
			return
		}
	}

30  format(' Type in the number of the VERTICAL SCALE LABEL:',/)
#
# change to all upper case letters because plots look better
#
40  format('  1: SPECTRAL REFLECTANCE (SCALED to 1.0 at 1.02 microns)')
50  format('  2: SPECTRAL REFLECTANCE (SCALED to 1.0 at 0.56 microns)')
60  format('  3: GEOMETRIC ALBEDO')
70  format('  4: RELATIVE FLUX')
80  format('  5: ABSOLUTE FLUX')
90  format('  6: REFLECTANCE')
100 format('  7: SCALED REFLECTANCE')
110 format('  8: INTENSITY')
#
120 format('  9: Type in your OWN LABEL (60 characters)')
130 format(' 10: CONVERT data TO LOGS and Type in your OWN LABEL')
140 format(' Type in the VERTICAL SCALE LABEL'/1x,59('-'),'i',/)
200  format(' Type in the VERTICAL SCALE (lower and upper bound) OF PLOT',/)
	end

      subroutine radec (itil, ih,im,is,jd,jm,js)
      implicit integer*4 (i-n)
	  character*(*) itil
########################################################################
#
#     this routine decodes the ra and dec from the title.  if a
#     character is encountered, the ra is set to 25 hours, and the dec
#     to zero.  the user is then instructed to type in the coordinates
#     in the subtraction routine
#
########################################################################
	include "../common/lundefs"
	logical minus

	minus = .false.


	do i= 1, 13 {
		if (itil(i:i)=='+') next
		if (itil(i:i)=='-') {
			minus = .true.
			next
		}
		if (itil(i:i)<'0' || itil(i:i)>'9') {
			ih= 25
			im= 0
			is= 0
			jd= 0
			jm= 0
			js= 0
			write(ttyout, 40)
			return
		}
	}
	read (itil,30)ih,im,is,jd,jm,js
	if (minus) {
		jd = -iabs(jd)
		jm = -iabs(jm)
		js = -iabs(js)
	}
	return

30      format (3i2, i3, 2i2)

40      format ('   coordinates not ok')

	end

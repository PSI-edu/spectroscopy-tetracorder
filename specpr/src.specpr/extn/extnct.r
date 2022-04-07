	subroutine extnct
	implicit integer*4 (i-n)
#################################################################
#                                                               #
#       extinction calculation main routine.                    #
#       print message and call requested routines.              #
#                                                               #
#################################################################
#	%W%		%G% %U%

	include "../common/spmaxes"   # max parameters, must be first

	include "../common/blank"
	include "../common/lbl4"
	include "../common/label1"
	include "../common/lblg"
	include "../common/labelf"
	include "../common/xtnct"
	include "../common/alphabet"
	include "../common/lundefs"

	ifilx=ifilex
	repeat {
		call eralph
		call whedr
		write (ttyout,5)
		call crtin
		i=1
		call wjfren(i,x,ichr)
		ifilex=ifilx
		if(ichr==ihe || ichr==ihx) return
		if(ichr==ihc) {
			igo = 15
3000		call extnta (igo)
			if (igo==2100) call extplt
			next
		}
		if(ichr==ihd) {
			igo = 361
			goto 3000
		}
		if(ichr==ihp)call extplt
		if(ichr==iht)call iplot
		if(ichr==ihs) {
			igo = 520
			goto 3000
		}
		if(ichr==ihm) {
			igo = 700
			goto 3000
		}
	}




5       format( '            EXTINCTION ROUTINES              '/,
		' type c to do extinction calculations'/,
		' type d to display starpacks', /,
		' type s to write a starpack on disk', /,
		' type p to plot extinction coefficients and goodness of fit',/,
	' type t to display airmass versus log intensity and to delete bad',/,
		'           runs and channels', /,
		' type m to enter manual history', /,
		' type e or x to exit routine', /)
	end

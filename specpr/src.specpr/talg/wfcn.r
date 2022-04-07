	subroutine wfcn(ilf,ifcn)
	implicit integer*4 (i-n)
#################################################################
#                                                               #
#       this routine handles trig and algebraic functions.      #
#                                                               #
#################################################################

	include "../common/spmaxes"   # max parameters, must be first

	include "../common/blank"
	include "../common/lbl6"
	include "../common/lbl4"
	include "../common/label1"
	include "../common/labl2"
	include "../common/lbl3"
	include "../common/label3"
	include "../common/labelf"
	include "../common/alphabet"
	include "../common/lundefs"


#
#     this subroutine computes the basic algebraic and trig functions
#


	call hreset(1)
	call whedr2
	write(ttyout,1)

	call wfcn1 (ilf, ifcn, x)
	xsave = x
	if (ifcn<0) {
		ifcn= ihe
		return
	}
#
#     subroutine wfcn1 returns a number code indicating the function:
#
#     02= exp
#     03= ln  (natural)
#     04= log (base 10)
#     05= 10**x
#     06= 1/x
#     07= x**c  (dataa > 0.0)
#     08= c**x  (c > 0.0)
#     09= sin   (radians)
#     10= cos   (radians)
#     11= tan   (radians)
#     12= invcos   (radians)
#     13= invsin   (radians)
#     14= invtan   (radians)
#     15= sind     (degrees)
#     16= cosd     (degrees)
#     17= tand     (degrees)
#     18= invcosd  (degrees)
#     19= invsind  (degrees)
#     20= invtand  (degrees)
#     21= cosh     (hyperbolic cos)
#     22= sinh     (hyperbolic sin)
#     23= tanh     (hyperbolic tan) all hyperbolic trig fcns in radians
#     24= abs
#     25= int
#     26= frac
#     27= 1/xe          (include errors)
#
#     number < 0= could not decode function
#
	write(ttyout,3) ifcn, idv1, ifl1, ititl
	call fnhist (ifcn, x)
	write(ttyout,4) ihist

	call crtin
	ii=1
	call wjfren(ii,x,ire)
	if (ire==ihe) {
		ifcn=ihe
		return
	}
	if (ire==ihx) {
		ifcn=ihx
		return
	}

	if (ifcn >= 2 & ifcn <= 5 ) {
                                             call fnlgs(ifcn)
	}

	if (ifcn == 6 | ifcn >= 24 & ifcn <= 27) {
                                             call fnalg(ifcn)
	}

	if (ifcn == 7 | ifcn == 8) {
                                             call fnpowr(ifcn,xsave)
	}

	if (ifcn >= 9 & ifcn <= 20) {
                                             call fntrig(ifcn)
	}

	if (ifcn == 21 | ifcn == 22 | ifcn == 23) {
                                             call fnhypt(ifcn)
	}

	if (ifcn==27) {
                           call fnhist(ifcn,x)
	}

	return


1       format (20x,
		    'basic function routines',/,
		20x, 23(1h-), /,
' available functions:  exp, ln, log, 10**x, 1/x, x**c, c**x, sin, cos, tan,',/,
'   sind, cosd, tand, invsin, invcos, invtan, invsind, invcosd, invtand, ',/,
'   sinh, cosh, tanh, abs, int, frac, 1/xe,', /,
 1x, 72(1h-), /)

3       format ('     function', i3, ' operating on ', a, i5, ':', a)

4       format (1x, a, //, 5x, 'press return to continue,  e  to exit')

	end

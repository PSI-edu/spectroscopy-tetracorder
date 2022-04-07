	subroutine range(dndt1,acdt1,dncm1,accm1,dndt2,acdt2,dncm2,accm2)
	implicit integer*4 (i-n)
#ccc    name:
#ccc    version date:
#ccc    author(s):
#ccc    language:
#ccc
#ccc    short description:
#ccc      computes scale factor and origin
#ccc
#ccc      ARGUMENTS:
#ccc
#ccc      dndt1:          point 1  down   coordinate in user units.
#ccc      acdt1:          point 1 across  coordinate in user units.
#ccc      dncm1:          point 1  down   coordinate in centimeters.
#ccc      accm1:          point 1 across  coordinate in centimeters.
#ccc      dndt2:          point 2  down        "     "  user units.
#ccc      acdt2:            "   2 across       "     "   "     "
#ccc      dncm2:            "   2  down        "     "  centimeters.
#ccc      accm2:            "   2 across       "     "       "
#ccc
#ccc
#ccc    algorithm description:
#ccc    system requirements:
#ccc    subroutines called:
#ccc    argument list description:
#ccc    parameter description:
#ccc    common description:
#ccc    message files referenced:
#ccc    internal variables:
#ccc    file description:
#ccc    user command lines:
#ccc    update information:
#ccc    NOTES:
#ccc


	include "../common/plot01"
	include "../common/plot02"

	dscale=dndt2-dndt1
	if (dscale == 0.) {
		i=1
		goto 200
	}
	dscale=(dncm2-dncm1)/dscale
	if (dscale == 0.) {
		i=3
		goto 200
	}
	odscal=dscale
	dmin=dndt1-dncm1/dscale

	ascale=acdt2-acdt1
	if (ascale == 0.) {
		i=2
		goto 200
	}
	ascale=(accm2-accm1)/ascale
	if (ascale == 0.) {
		i=4
		goto 200
	}
	oascle=ascale
	amin=acdt1-accm1/ascale
	return

200	j=i+4
	write(lunmsg,170) i,j
170	format(' ** range called with arg',i1,' = arg',i1)
	end

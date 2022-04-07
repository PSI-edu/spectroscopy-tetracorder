        subroutine pltsym(down,across,height,symbol,sympen)
	implicit integer*4 (i-n)
#ccc    name:
#ccc    version date:
#ccc    author(s):
#ccc    language:
#ccc
#ccc    short description:
#ccc       this routine plots a symbol at the position
#ccc       (down,across). if height<0 then down & across are
#ccc       in centimeters otherwise they are in the users
#ccc       units. iabs(height) determines the size of the symbol.
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
	integer*4 height,symbol
        integer*4 sympen


	include "fdefs.h"
	include "../common/plot01"


	i = height
	call tocent(down,across,d,a,i)

	nd = d * factor + .5
	na = a * factor + .5

        write (tlun,110) nd,na,iabs(height),symbol,sympen


110     format('S',5i6,' x')

	return
	end

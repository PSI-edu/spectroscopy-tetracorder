	integer*4 function ifnb(text)
	implicit integer*4 (i-n)

#ccc    name: ifnb
#ccc    version date: 8/18/09
#ccc    author(s): Jeff Hoover, Roger Clark, Eric livo
#ccc    language: Ratfor
#ccc
#ccc    short description:
#ccc            this function returns the position of the first
#ccc            non-blank character in the input array text
#ccc		7/5/85: Modified to not go over array bound, and
#ccc            value returned if <= length of array.  If array is
#ccc		all blanks, fnb = len(text)
#ccc		Spacifically, any ASCII character > ASC(32)
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

	character*(*)   text
	character*1 csingle

	ilen = len(text)
	for (ifnb=1; ifnb < ilen; ifnb=ifnb+1) {
		csingle = text(ifnb:ifnb)
		if ((csingle > char(32)) && (csingle != "=" )) return
	}

	return
	end

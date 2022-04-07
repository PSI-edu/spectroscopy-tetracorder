	integer*4 function fnb(text)
	implicit integer*4 (i-n)

#ccc    name: fnb
#ccc    version date: 7/5/85
#ccc    author(s): Jeff Hoover and Roger Clark
#ccc    language: Ratfor
#ccc
#ccc    short description:
#ccc            this function returns the position of the first
#ccc            non-blank character in the input array text
#ccc		7/5/85: Modified to not go over array bound, and
#ccc            value returned if <= length of array.  If array is
#ccc		all blanks, fnb = len(text)
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

	ilen = len(text)
	for (fnb=1; fnb < ilen; fnb=fnb+1) {
		if (text(fnb:fnb) != ' ') return
	}

	return
	end

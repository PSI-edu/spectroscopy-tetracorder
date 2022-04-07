	integer*4 function lnb(text)
	implicit integer*4 (i-n)
#ccc    name:
#ccc    version date:
#ccc    author(s):
#ccc    language:
#ccc
#ccc    short description:
#ccc            this function returns the position of the last
#ccc            non-blank character in the input array text
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

	for (lnb=len(text); lnb>=1; lnb=lnb-1) {
		if (text(lnb:lnb) != ' ') break
	}

	if (lnb <= 1) lnb = 1

	return
	end

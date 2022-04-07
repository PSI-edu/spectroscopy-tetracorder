	subroutine wavfil (iwavfl)
	implicit integer*4 (i-n)
#cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
#                                                               c
#       this routine asks for a wavelength record number        c
#       between 1 and 99 and returns it in 'iwavfl'             c
#       if 'x' is typed then the character 'x' is returned.     c
#                                                               c
#cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
#
	include "../common/spmaxes"   # max parameters, must be first

	include "../common/lbl4"
	include "../common/lundefs"


	repeat {
		write(ttyout,10)
		call crtin
		i= 1
		call wjfren (i,x,il)
		if (il!=0 || x<1 || x>99) next
		iwavfl= x
		return
	}

10      format (' type in the wavelength record number (1 to 99)', /)
	end

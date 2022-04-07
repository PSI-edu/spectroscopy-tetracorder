	subroutine f23 (ic)
	implicit integer*4 (i-n)

c  This subroutine is a tempory f23 that doesn't call anything
c  so that specpr can be linked without the parser.
c  compile by f77 -g -C -c f23temp.f
c  mv f23temp.o ../parser.o

	write (6,10)
10	format (1x, 'f23: parser not available',/)
	ic = 0
	return
	end

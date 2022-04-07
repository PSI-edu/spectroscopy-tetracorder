	subroutine snorm(smin,smax,x,n)
#
#   Determines a value x which satisfies 1.0e-4 <= x*smin < x*smax < 1.0e4
# where x = 10**n
#
	n=0
	x1 = 0.0
	x2 = 0.0
	if (smin != 0.0) x1 = alog10(abs(smin))
	if (smax != 0.0) x2 = alog10(abs(smax))
	if (x1 < -4.0 | x1 >= 4.0) n = x1
	else {
		if (x2 < -4.0 | x2 >= 4.0) n = x2
	}
	x = 10.0**n
	return
	end

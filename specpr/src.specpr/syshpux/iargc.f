c   Fortran 77
c
c   this function returns the number of arguments on the command
c	line.  It is a routine that is normally included in unix 
c	systems, but not in HP-UX.
c
	integer*4 function iargc (dummy)

$ALIAS fgetarg = 'Ftn_getarg'(%ref,%ref,%ref)
	
	integer fgetarg
	character*80 str
	integer str_len, argnum, actual_len

	str_len=40
	
	do 100 i = 0,1000

		actual_len = fgetarg (i, str, strlen)
		if (actual_len.eq.-1) go to 200

100	continue

	i = i-1
200	iargc = i-1
	return
	end

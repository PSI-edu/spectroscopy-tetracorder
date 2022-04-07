	subroutine pad(str)
	implicit integer*4 (i-n)

#ccc	Version	10/15/83
#ccc	Author:
#ccc	This routine converts all nonprintable characters in STR
#ccc	to blanks.
#ccc
	character str*(*)

	for (i=1; i<=len(str); i=i+1)
		if (str(i:i)<' ' || str(i:i)==char(127)) str(i:i) = ' '
	
	return
	end

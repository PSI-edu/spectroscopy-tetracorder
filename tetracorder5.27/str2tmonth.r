	subroutine str2tmonth (str, mon)
	implicit integer*4 (i-n)

#ccc  name: str2tmonth
#ccc  version date: 4/24/90 (cubeio/str2month.r)
#ccc  author(s): Roger N. Clark
#ccc  language:  Ratfor
#ccc
#ccc  short description: find integer month from 3 character string
#ccc                     month abbreviation.
#ccc
#ccc  algorithm description:
#ccc  system requirements: none
#ccc  subroutines called: none
#ccc  argument list description: str = month 3 character abbrev.
#ccc                                   (str can be any string >= 3 characters)
#ccc                             mon = integer month
#ccc  parameter description:
#ccc  common description: none
#ccc  message files referenced: none
#ccc  internal variables:
#ccc  file description: none
#ccc  user command lines: none
#ccc  update information:
#ccc  NOTES:
#ccc
	character*(*) str
	character*3 cm
	integer*4 mon

#TEMP:
	integer*4 ttyout
	ttyout=6

	cm = str(1:3)
	mon = 0

	if (cm == 'Jan') {
		mon = 1
	} else if (cm == 'Feb') {
		mon = 2
	} else if (cm == 'Mar') {
		mon = 3
	} else if (cm == 'Apr') {
		mon = 4
	} else if (cm == 'May') {
		mon = 5
	} else if (cm == 'Jun') {
		mon = 6
	} else if (cm == 'Jul') {
		mon = 7
	} else if (cm == 'Aug') {
		mon = 8
	} else if (cm == 'Sep') {
		mon = 9
	} else if (cm == 'Oct') {
		mon = 10
	} else if (cm == 'Nov') {
		mon = 11
	} else if (cm == 'Dec') {
		mon = 12
	} else {
		write (ttyout,10) cm
10		format ('Month CONVERSION ERROR: ',a,
			' is not a valid month abbreviation')
		mon = 0
	}
	return
	end

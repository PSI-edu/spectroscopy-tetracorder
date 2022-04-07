	subroutine strdate(str)
	implicit integer*4 (i-n)

#ccc  name:  strdate
#ccc  version date: 1/7/91
#ccc  author(s): Roger N. Clark
#ccc  language: ratfor
#ccc
#ccc  short description: makes date and time into string for vicar
#ccc  algorithm description: 
#ccc  system requirements: none
#ccc  subroutines called: jdatim
#ccc  argument list description:
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced: none
#ccc  internal variables:
#ccc  file description: none
#ccc  user command lines: none
#ccc  update information:
#ccc  NOTES:
#ccc

#ccc  07/07/2008 RED - Changed string to str to prevent being interrupted wrong by ratfor
	character*(*) str
	character*3   cmon

	il = len(str)
	if (il < 23) {   #string not long enough

		do i = 1, il {
			str(i:i) = '?'
		}
		return
	}

	call jdatim(jda,isfm)   # get Julian day and sec from midnight

	call frjuld(iy,imon,iday,jda)  # get year, month, day

# get name of month
	if (imon == 1) {
		cmon = 'Jan'
	} else if (imon == 2) {
		cmon = 'Feb'
	} else if (imon == 3) {
		cmon = 'Mar'
	} else if (imon == 4) {
		cmon = 'Apr'
	} else if (imon == 5) {
		cmon = 'May'
	} else if (imon == 6) {
		cmon = 'Jun'
	} else if (imon == 7) {
		cmon = 'Jul'
	} else if (imon == 8) {
		cmon = 'Aug'
	} else if (imon == 9) {
		cmon = 'Sep'
	} else if (imon == 10) {
		cmon = 'Oct'
	} else if (imon == 11) {
		cmon = 'Nov'
	} else if (imon == 12) {
		cmon = 'Dec'
	} else {
		cmon = '???'
	}

# compute hours, minutes, seconds
	ihr = isfm/3600
	ihs = ihr*3600
	imin= (isfm-ihs)/60
	isec= isfm - ihs - imin*60

######################
# Should look like:
#
# Jan  2 11:35:25 1991
######################

	write (str, 10) cmon, iday, ihr, imin, isec, iy
10	format (a,i3,i3,':',i2,':',i2,i5,' UT')

	return
	end



	subroutine fuser(uname)
	implicit integer*4 (i-n)

#ccc  name: fuser
#ccc  version date: 6/22/89
#ccc  author(s): Roger N. Clark
#ccc  language: ratfor
#ccc
#ccc  short description:  call the c routine fcuser to get userid
#ccc
#ccc  algorithm description:
#ccc  system requirements:
#ccc  subroutines called: fcuser
#ccc  argument list description: uname: user id, 8 characters
#ccc  parameter description:
#ccc  common description: none
#ccc  message files referenced: none
#ccc  internal variables:
#ccc  file description: none
#ccc  user command lines: none
#ccc  update information:
#ccc  NOTES:
#ccc

	character*20 string
	character*8 uname

	integer*4 i(5)
	equivalence (i,string)
#
# get userid
#
	j = fcuser(i)
#
# check for nulls, and set them to blanks
#       if fcuser returns error (-1) set to all blanks
#
	if (j == 0) {
		uname = string(1:8)
		do k = 1, 8 {
			if (uname(k:k) == char(0)) uname(k:k) = ' '
		}
	} else {
		uname = '        '
	}

	return
	end

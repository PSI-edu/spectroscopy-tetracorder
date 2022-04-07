	subroutine setals (k)
	implicit integer*4 (i-n)

#ccc  name: setals.r
#ccc  version date: 7-24-85
#ccc  author(s): Kathy Kierein
#ccc  language: ratfor
#ccc
#ccc  short description: This subroutine separates the line into
#ccc			 keywords and translations when storing the
#ccc			 alias words and translations into their arrays
#ccc  algorithm description: none
#ccc  system requirements: none
#ccc  subroutines called: getkey.r
#ccc  argument list description:
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables: send-positions of the last non-blank in
#ccc				the line
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc
# This subroutine separates the lines from the stored list of alias
# words into their keywords and translations

	include "../common/key1"
	include "../common/lbl4"
#RED
	integer*4 lnb     # function lnb

	integer*4 send, sbeg
	character*1 ihbcksl

	ibeg = 1
	ihbcksl = char(92)  # this is the backslash character

	k = numals

# This finds the beginning of the keyword

	send = lnb (set)

	if (send > 1) {
		do i=1,send-1 {    # check for coment
			if (set(i:i+1) == ihbcksl // '#') {  # strip comment
				send = i -1
				if(send == 0) send=1
				break
			}
		}
	}
	do i=1,send {
		if (set (i:i) == '[') {
			sbeg = i
		}else{
			next
		}
		break
	}

	if (i == (send + 1)) {
		return
	}

# This assigns the keyword into the array cmdals and finds the size
# of the alias word

	do j=i,i+16 {
		if (set (j:j) == ']') {
			alsize(k) = j-i-1
			cmdals(k)(1:alsize(k))=set(i+1:j-1)
		}else{
			next
		}
		break
	}

# This assigns the translation into the array cmdtrn and  finds the
# size of the word

	trnsiz(k) = send - j
	if (trnsiz(k) > 60) trnsiz(k) = 60

	if (trnsiz(k) > 0) {
		cmdtrn(k) (1:trnsiz(k)) = set (j+1:send)
	} else {
		cmdtrn(k) = ' '
	}
	

	return
	end

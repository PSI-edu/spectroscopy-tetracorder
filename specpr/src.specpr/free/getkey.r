	subroutine getkey 
	implicit integer*4 (i-n)

#ccc  name: getkey.r
#ccc  version date: 7-24-85
#ccc  author(s): Kathy Kierein
#ccc  language: ratfor
#ccc
#ccc  short description: This subroutine is used while in specpr and
#ccc                     finds alias words and their sizes.
#ccc
#ccc  algorithm description: none
#ccc  system requirements: none
#ccc  subroutines called: none
#ccc  argument list description:
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc

# This subroutine finds the alias words and their sizes

	include "../common/key1"
	include "../common/lbl4"
	include "../common/lundefs"

#RED
	integer*4 lnb     # function lnb
	character*1 bckslsh

	bckslsh=char(92)  # this is the backslash character

	keysiz = 0
	beg = 0
	lnend = 0
	nend = 0


# This finds the first [ or capital letter that indicates an alias

	nend = lnb (iopcon(1:80))

#	check if comment and do not go beyond it.

	if (nend < 79) {
		itmp = nend
		do i = 1, itmp {
			if ((iopcon (i:i) == bckslsh) &
				(iopcon (i+1:i+1) == '#')) {

				nend = i - 1
				if (nend < 1) nend = 1
				if (ibeg > nend) ibeg = nend
				go to 10
			}
		}
	}
		

10	do i=ibeg,nend {
		if ((iopcon (i:i) == bckslsh)&(iopcon (i+1:i+1) == '[')){
			return
                }else if (iopcon (i:i) == '[') {
			beg = i
		}else if (iopcon(i:i) >= 'A' & iopcon(i:i) <= 'Z') {
	 		beg = i
		}else{
			next
		}
		break
	}

	if (i == (nend+1)) {
		go to 50
	}


# This assigns the alias word to ikeywd if alias enclosed in []

	iend = i + 16
	if (iend > 79) iend = 79
	do j=i,iend {
		if (iopcon (j:j) == bckslsh & iopcon(j+1:j+1) == ']'){
			keysiz = 0
			return
		}else if (iopcon (j:j) == ']') {
			keysiz = j-i-1
			ikeywd (1:keysiz) = iopcon (i+1:j-1)
			lnend = j
			go to 100
		}else{
			next
		}
	}

# This assigns the alias word to ikeywd if alias is all capital
# letters

	do k=i,i+16 {
		if (iopcon(k:k) < 'A' | iopcon(k:k) > 'Z') {
			keysiz = k-i

			if (keysiz < 4) {
				ibeg = beg + 1
				go to 10
			}else{
				ikeywd (1:keysiz) = iopcon (i:k-1)
				lnend = k
			}

		}else{
			next
		}
		break
	}

50	beg = i

100	return     
	end 


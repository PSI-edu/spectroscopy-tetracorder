	subroutine delhist(jdlt,idlt,text,nbytes)
	implicit integer*4 (i-n)

#ccc  version date: 01/07/94
#ccc  author(s): Roger Clark
#ccc  language: Ratfor
#ccc
#ccc  short description:
#ccc         This is a deleted points history routine,
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc                    comprs
#ccc  argument list description:
#ccc     arguments: 
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc
#
#   delete points history routine
#   idlt    -  array containing deleted point channel numbers
#   jdlt    -  number of deleted points
#   text    -  the history.  dimensioned nbytes.
#   nbytes  - number of characters in array text
#   texta  -  working array for text.  dimensioned 800.

	include "../specpr/src.specpr/common/spmaxes"   # max parameters, must be first

	include "../specpr/src.specpr/common/alphabet"

#RED
	integer*4 fnb      # function
	integer*4 lnb      # function
	
	character*800	texta
	character*800	textb
	character*(*)   text

	integer*4	idlt(jdlt)

	texta = ' '
	textb = ' '
	text  = ' '

	if (jdlt < 1) return

	itext=1

1	format (i6,' ')

	for (i = 1; i <= jdlt; i = i+1) {

		if (i+2 <= jdlt) {

			if ((idlt(i+1) == idlt(i)+1) &
				(idlt(i+2) == idlt(i)+2)) {   # "to" mode


				write (texta(itext:itext+6),1) idlt(i)
				itext = itext + 7
				i = i+2
10				if (i+1 <= jdlt) {
					if (idlt(i+1) == idlt(i)+1) {
						i = i + 1
						go to 10
					}
				}
				write (texta(itext:itext+8),2) idlt(i)
2				format ('t ',i6,' ')
				itext = itext + 9

			} else {
				write (texta(itext:itext+6),1) idlt(i)
				itext = itext + 7
			}
		} else {
			write (texta(itext:itext+6),1) idlt(i)
			itext = itext + 7
		}

	}
	texta(itext:itext) = 'c'
	itext = itext + 1

# compress blanks
######	write (6,*) 'DEBUG: delhist before compress: n=',itext
	
	iblnk = 1  # consider last position a blank
	j = 1
	do i = 1, itext {
		if ((texta(i:i) == ' ') & (iblnk == 1)) {
			next
		} else if ((texta(i:i) == ' ') & (iblnk == 0)) {
			textb(j:j) = texta(i:i)
			iblnk = 1
			j = j+1
		} else {
			textb(j:j) = texta(i:i)
			iblnk = 0
			j = j+1
		}
	}
	n = j - 1
######	write (6,*) 'DEBUG: delhist n=',n

	texta(1:n) = textb(1:n)
	texta(n+1:800) = ' '

	if (n > 80) {

		m = lnb(texta(1:80))
		if (m == 80) {
			for (i = 80; i > 7; i = i - 1) {
				if (texta(i:i) == ' ') break
			}
			m = i
		}
######		write (6,*) 'DEBUG: test point 1'
		text(1:80) = texta(1:m)
		k = fnb (texta(m+1:m+79))  # first non-blank for lines 2
		if (k == 0) k = m+1
		write (6,*) 'DEBUG: test point 2 in delhist: k=',k,' m=',m
		write (6,*) 'DEBUG: test point 2 in delhist: text size=',len(text)
		write (6,*) 'DEBUG: test point 2 in delhist: texta=', texta
		text(81:160) = texta(k:k+79)
		if (k+79 < n) {
			text(161:240)='more deleted points, but need to code it!'
		}

	} else {
		text(1:n) = texta(1:n)
	}

	return

	end

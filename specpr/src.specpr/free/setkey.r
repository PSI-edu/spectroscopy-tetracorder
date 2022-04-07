	subroutine setkey
	implicit integer*4 (i-n)

#ccc  name: setkey.r
#ccc  version date: 7-24-85
#ccc  author(s): Kathy Kierein
#ccc  language: ratfor
#ccc
#ccc  short description: This subroutine is used in specpr to set
#ccc                     up alias words and translations in memory.
#ccc			 It also can delete an alias from memory.
#ccc  algorithm description: none
#ccc  system requirements: none
#ccc  subroutines called: getkey.r
#ccc  argument list description:
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables: set-used to store the keywords and
#ccc				translations
#ccc			  num-number in array for storage of new
#ccc				keywords
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc
# This subroutine sets up alias words and translations in memory
# It also deletes an alias from memory

	include "../common/key1"
	include "../common/lbl4"
	include "../common/lundefs"


	integer*4  lend
	ibeg = 1
	maxals = 199   # this should equal the array sizes -1 in
                       #      the key1 common block



# Check for ==! which indicates deleting an alias

	call getkey

	do l=1,beg-2 {
		if (iopcon (l:l+2) == '==!') {
			go to 20
		}else{
			next
		}
	}


# Check for option to print the list of alias words and translations
# from memory. Call subroutine allist.

	do m=1,nend-2 {
		if (iopcon (m:m+2) != '==[') {
			if (iopcon(m:m+5) == '==list') {
				lend = m+6
				call allist(lend)
				return
			}else if (m == nend-2) {
				write (ttyout,15)
15				format ('Error, [ ] needed around',
					' alias word for definition')
				return
			}else{
				next
			}
		}
		break
	}



# Add an alias to memory

	set = iopcon
	if (numals > maxals) {
		write (ttyout, 18) maxals
18		format ('ERROR, maximum number of aliases (',i5,
				') EXCEEDED')
	} else {
		numals = numals + 1
	}
	call setals(k)
	
# Write over alias if redefined

	do i=1,numals-1 {
		if (cmdals(i)(1:alsize(i)) == cmdals(k)(1:alsize(k))) {
			trnsiz(i) = trnsiz(k)
			cmdtrn(i)= cmdtrn(k)
			cmdals(k)(1:alsize(k)) = ' '
			cmdtrn(k)(1:trnsiz(k)) = ' '
			numals = numals - 1
		}
	}
	return

# Delete an alias from memory


20	do j=1,numals {
		if (keysiz == alsize(j)) {
			if (ikeywd(1:keysiz) == cmdals(j)(1:alsize(j))) {
				write (ttyout,10) iopcon
10				format('The alias and its translation',
					a, ' are deleted from memory')

				do k=j,numals-1 {
					alsize(k) = alsize(k+1)
					trnsiz(k) = trnsiz(k+1)
				        cmdals(k) = cmdals(k+1)
					cmdtrn(k) = cmdtrn(k+1)
				}
				numals = numals - 1
				return
			}
		}
	}

	write (ttyout,30)
30	format ('Error, alias not found on list')

	return
	end

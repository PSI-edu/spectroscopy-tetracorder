	subroutine sphelp(helpst)
	implicit integer*4 (i-n)

#ccc  version date: %W% %G% %U%
#ccc  author(s): Roger Clark
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc         This subroutine processes a help command by calling a
#ccc         shell script which reads the manual page.
#ccc      
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc          system
#ccc  argument list description: helpst: string containing help request
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc
#####################################################################
############### new version of crtin using FORTRAN-77 ###############
#####################################################################

	include "../common/lundefs"
	include "../common/alphabet"
	include "../common/cmd"

	character*80	helpst, astring
	character*1	null
	character*2 bckslpnd

	bckslpnd(1:1)=char(92)  # this is the backslash character
	bckslpnd(2:2)=char(35)  # this is the pound character

	i = 1
	call wjfren (i,x,il)
	if (i > 78 | il != ihh) {
		write (ttyout,100)
100		format ('ERROR in help command: help not found!')
		call what(i)
		return
	}
	astring = helpst
	if (astring(i-1:i+2) != 'help') {
		write (ttyout,100)
		call what(i)
		return
	}
#
# delete the word help from astring
#
	astring(i-1:i+2) = '    '
#
# delete comments
#
	ii = 1
	while (ii < 80) {

		if (astring(ii:ii+1) == bckslpnd) { #comment
			if (ii > 1 & astring(ii-1:ii-1) != bckslpnd(1:1)) {
#
#					if no backslash to
#					escape the \# then
#					remove comment.

				do i = ii, 80 {
					astring(i:i) = ' '
				}
			}
			go to 200
		}
		ii = ii +1
	}

200	continue

#
# check that helppg is not blank, if so, set to error
#
	null = char(0)
	ihlp = 0
	do i = 1, 40 {
		if (helppg(i:i) == null) helppg(i:i) = ' '
		if (helppg(i:i) == ' ' ) ihlp = ihlp + 1
	}
	if (ihlp == 40 ) {
		helppg =   'specprhelp                              '
	}

# send help string to shell script command

	call system (helppg // ' ' // astring // char(0))

	return
	end

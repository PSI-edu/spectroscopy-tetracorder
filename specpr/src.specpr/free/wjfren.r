	subroutine wjfren(i, x, il)
	implicit integer*4 (i-n)

#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc                   This subroutine sets il=0 if a variable is
#ccc                   decoded properly. if not il is set to the
#ccc                   character encountered.
#___________________________________________________________________
#
#ccc  ALGORITHM DESCRIPTION:
#ccc
#ccc  This subroutine operates as a command string parser in specpr
#ccc  but is not limited to this utility.  It is also useful for 
#ccc  most general string parsing needs.  It requires the include
#ccc  file lbl4 which contains the main working array iopcon.  As
#ccc  an example of how it works consider an input line that is 
#ccc  supposed to contain only digits.  The string containing the 
#ccc  digits is parsed until either a trailing blank or a non-digit character
#ccc  is encountered.  If a trailing blank is encountered the variable il is
#ccc  set to 0.  Otherwise, il is set to the character encountered.
#ccc  In the second case the indexor i is incremented to the next byte
#ccc  (character) in the iopcon array.  Leading blanks are ignored.
#ccc  Some common examples follow.
#
#ccc  EXAMPLE #1: Integer Input - No Characters
#ccc              Input:
#ccc                    iopcon = "1024 "
#ccc                              ^         i = 1 (input index)
#ccc              Output:
#ccc                    iopcon = "1024 " 
#ccc                                  ^     i = 5 (output index)
#ccc                                        x = 1024.0
#ccc                                       il = 0
#
#ccc  EXAMPLE #2: Integer Input - Character Follows
#ccc              Input:
#ccc                    iopcon = "1024w "
#ccc                              ^         i = 1 (input index)
#ccc              Output:
#ccc                    iopcon = "1024w " 
#ccc                                   ^    i = 6 (output index)
#ccc                                        x = 1024.0
#ccc                                       il = nnnnnnnn  (integer equivalent
#ccc                                           of the character w)
#
#ccc  EXAMPLE #3: Character Input - Character Follows (or Blank)
#ccc              Input:
#ccc                    iopcon = "tuvw "
#ccc                              ^         i = 1 (input index)
#ccc              Output:
#ccc                    iopcon = "tuvw " 
#ccc                               ^        i = 2 (output index)
#ccc                                        x = 0.0
#ccc                                       il = nnnnnnnn  (integer equivalent
#ccc                                           of the character t)
#
#ccc  EXAMPLE #4: Input Index (i) Points to a Blank
#ccc              Input:
#ccc                    iopcon = " 1024w "
#ccc                              ^         i = 1 (input index)
#ccc              Output:
#ccc                    iopcon = " 1024w " 
#ccc                                    ^   i = 7 (output index)
#ccc                                        x = 1024.0
#ccc                                       il = nnnnnnn (int equiv of w)
#
#ccc  EXAMPLE #5: Real Numbers - No Characters
#ccc              Input:
#ccc                    iopcon = "1024.5 "
#ccc                              ^         i = 1 (input index)
#ccc              Output:
#ccc                    iopcon = "1024.5 " 
#ccc                                    ^   i = 7 (output index)
#ccc                                        x = 1024.5
#ccc                                       il = 0
#
#___________________________________________________________________
#ccc  subroutines called:
#ccc                    decod1
#ccc  argument list description:
#ccc    arguments: i,x,il
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  fie description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc
#################################################################
#                                                               #
#     free format subroutine                                    #
#     if a variable is decoded properly, il= 0                  #
#     if not, il is set to the character encountered.           #
#                                                               #
#       arguments:                                              #
#       i:      (input) starting point in array iopcon          #
#       x:      (output)value found (if any) or zero            #
#       il:     (output)last character encountered              #
#                                                               #
#################################################################

	include "../common/lbl4"
	include "../common/lundefs"
	include "../common/alphabet"

#RED
	integer*4 ihchar     # function ihchar

	character*1 itmp

####### for DEBUGGING:

	character*4 chdbg
	integer*4   ibdg
	equivalence (chdbg,idbg)

	il = 0
	j = 0
	m = 0
	x = 0.
	sign = 1.
        if (i < 1) {
                write (ttyout,*) 'WARNING in wjfren'
                write (ttyout,*) 'i < 1, reset to 1'
                write (ttyout,*) 'MAY INDICATE A SOFTWARE ERROR'
                i = 1
        }


	# Check to see if index at end of iopcon array
	if (i <= 80) {
		# Decode minus sign, digit, decimal point
		for ( ; i <= 80; i = i + 1) {
			itmp = iopcon(i:i)
			if (j > 0 && itmp == ' ') go to 26
			if (itmp == '-') sign= -1.
			if (itmp == '-' && i == 80) go to 30
			if (itmp == '-' &&
				(iopcon(i+1:i+1) == ' ' || i == 80)) go to 30
			if (itmp == '-' || itmp == ' ') next
			if (itmp == '.') go to 20
			if (itmp < '0' || itmp > '9') go to 30
			j = j + 1
			call decod1 (itmp,m)
			if (x < 1.0e37) x= x*10.+ m
		}
		return
20     		i = i + 1
		n = 0
#
#     decode digits to the right of the decimal point
#
		for ( ; i <= 80; i = i + 1) {
			itmp = iopcon(i:i)
			if (j > 0 && itmp == ' ') break
			if (j == 0 && itmp == ' ') go to 29
			if (itmp < '0' || itmp > '9') go to 30
			n= n + 1
			call decod1 (itmp,m)
			j = j + 1
			xd = m
			x = x + xd/(10.**n)
		}
26      	x = x*sign
		return
#
#     if a character is encountered, decode.
#
29      	i = i-1
30      	il = ihchar(iopcon(i:i))
		#write (*,*) 'DEBUG: 1char: iopcon=',iopcon(i:i),' ihchar=',il,
		#		' u=',ihu
		idbg = il  # DEBUGGING
		#write (*,111) chdbg, idbg
111		format ('DEBUG: 1char: chdbg=',a4,' idbg=',i12)
		x = x*sign
		i = i + 1
	}
	#write (*,*) 'DEBUG wjfren just before return (3): chdbg=', chdbg, ' i=',i
	return
	end

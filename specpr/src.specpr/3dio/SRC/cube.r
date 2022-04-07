
	SUBROUTINE CUBE 
	IMPLICIT INTEGER*4 (i-n)

#...................................................................
#   TITLE:                 CUBE PATTERN SUBROUTINE                 .
#...................................................................
#   DESCRIPTION:                                                   .
#               This portion of the program is designed to make    .
#               the way the data is read easier to visualize.  The .
#               user is provided with an echo on his choice of     .
#               spectral axis.  If the picture does not match the  .
#               user's idea of how the data needs to be read the   .
#               program is set up to loop back to correct the      .
#               choice.                                            .
#...................................................................
#   VARIABLES:                                                     .
#             flag      - indicates the extraction direction for   .
#                       the intended spectra                       .
#             ans       - check set up to catch user errors        .
#                                                                  .
#...................................................................

#  Set variable type
	INCLUDE        ../common/label1
	INCLUDE        ../common/ioftyp

#  Print visual aids for user
1	WRITE (ttyout,*)'                     ......................'
	WRITE (ttyout,*)'                   .                    . .'
	WRITE (ttyout,*)'  ................                    .   .'
	WRITE (ttyout,*)'.....................................     .'
	WRITE (ttyout,*)'. File header  .                    .     .'
	WRITE (ttyout,*)'................                    .     .'
	WRITE (ttyout,*)'               .                    .     .'
	WRITE (ttyout,*)'            x  .       data         .     .'
	WRITE (ttyout,*)'               .                    .     . '
	WRITE (ttyout,*)'               .                    .   .'
	WRITE (ttyout,*)'               .                    . . z'
	WRITE (ttyout,*)'               ......................'
	WRITE (ttyout,*)'                          y'
	WRITE (ttyout,*)

	WRITE (ttyout,*)'Choose extraction direction:'
	WRITE (ttyout,*)'           1    x dim (skewer the y-z plane)'
	WRITE (ttyout,*)'           2    y dim (skewer the x-z plane)'
	WRITE (ttyout,*)'           3    z dim (skewer the x-y plane)'
	WRITE (ttyout,*)'           e,x  to exit' 
	WRITE (ttyout,*)
	READ (ttyin,*)flag

#  Decode input and check validity
	CALL crtin 
	i=1
	CALL wjfren (i,x,il)
	IF (il != 0)  {
	   WRITE (ttyout,*)'Improper decode, try again.'
	   GO TO 1
	}
	IF (il == ihe | il == ihx) RETURN
	IF (x < 1 | il > 3) WRITE (ttyout,*)'Invalid input.'; GO TO 1
	flag=x

#  Indicate extraction direction chosen
	IF (flag == 1)   {
	   DO i=1,10  {
	     WRITE (ttyout,*)
	   }
	   WRITE (ttyout,*)'                    ......................'
 	   WRITE (ttyout,*)'                   .                    ..'
	   WRITE (ttyout,*)'                  .- - - - - - *       . .'
	   WRITE (ttyout,*)'                 .            /*      .  .'
	   WRITE (ttyout,*)'                .            / *     .   .'
	   WRITE (ttyout,*)'               ............./..*.....    .'
	   WRITE (ttyout,*)'               .               *    .    .'
	   WRITE (ttyout,*)'               .               *    .    .'
	   WRITE (ttyout,*)'               .               *    .    .'
	   WRITE (ttyout,*)'            x  .               *    .   .'
	   WRITE (ttyout,*)'               .   - - - - - - *    .  . z'
	   WRITE (ttyout,*)'               .              /     . .'
	   WRITE (ttyout,*)'               .             /      ..'
	   WRITE (ttyout,*)'               ............ /........'
	   WRITE (ttyout,*)'                          y'
	}

	ELSE IF (flag == 2) {
	l=dy
	   DO i=1,10  {
	     WRITE (ttyout,*)
	   }
	   WRITE (ttyout,*)'                    ......................'
 	   WRITE (ttyout,*)'                   .                    ..'
	   WRITE (ttyout,*)'                  .                    . .'
	   WRITE (ttyout,*)'                 .                    .  .'
	   WRITE (ttyout,*)'                .                    .   .'
	   WRITE (ttyout,*)'               ......................    .'
	   WRITE (ttyout,*)'               .                    .    .'
	   WRITE (ttyout,*)'               .  ********************** .'
	   WRITE (ttyout,*)'               . /|                 . /| .'
	   WRITE (ttyout,*)'            x  ./ |                 ./ |.'
	   WRITE (ttyout,*)'               .  |                 .  | z'
	   WRITE (ttyout,*)'               .                    . . '  
	   WRITE (ttyout,*)'               .                    ..  '
	   WRITE (ttyout,*)'               ...................... '
	   WRITE (ttyout,*)'                          y'
	}   
	ELSE IF (flag == 3)  {
	   DO i=1,10 {
	     WRITE (ttyout,*)
	   }
	   WRITE (ttyout,*)'                         ......................'
 	   WRITE (ttyout,*)'                       .                    . .'
	   WRITE (ttyout,*)'                     .                    .   .'
	   WRITE (ttyout,*)'                   .                    .     .'
	   WRITE (ttyout,*)'                 .         - - - *    .       .'
	   WRITE (ttyout,*)'               ................*.|...         .'
	   WRITE (ttyout,*)'               .             *   |  .         .'
	   WRITE (ttyout,*)'               .           *     |  .         .'
	   WRITE (ttyout,*)'               .         *       |  .         .'
	   WRITE (ttyout,*)'            x  . - - - *            .       .'
	   WRITE (ttyout,*)'               .       |            .     . z'
	   WRITE (ttyout,*)'               .       |            .   .'
	   WRITE (ttyout,*)'               .       |            . .'
	   WRITE (ttyout,*)'               ........|.............'
	   WRITE (ttyout,*)'                          y'
	}

	WRITE (ttyout,*)
	WRITE (ttyout,*)
	
#  Prompt for possible mistake
2	WRITE (ttyout,*)'Is this the way you intended your data to be read?'

#  Decode input and check validity
	CALL crtin 
	i=1
	CALL wjfren (i,x,il)
	IF (il != 0)  {
	   WRITE (ttyout,*)'Improper decode, try again.'
	   GO TO 2
	}
	IF (il == ihe | il == ihx) RETURN
	IF (il!='Y'|il!='y'|il!='N'|il!='n') {
	   WRITE (ttyout,*)'Invalid input.'; GO TO 1
	}
	flag=x
  	IF (ans == 'Y' | ans =='y') {
	   CONTINUE
	}
	ELSE {
 	   WRITE (ttyout,*)'OK, well try again.';GO TO 1;
	}
   	
#  Back to main program
	RETURN
	END


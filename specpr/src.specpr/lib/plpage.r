	subroutine plpage (lpline, inm)
	implicit integer*4 (i-n)

#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc                   This subroutine starts a new line printer listing
#ccc                   page if the current listing is past line 55 on
#ccc                   the page
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc                    none
#ccc  argument list description:
#ccc       argument: lpline,inm
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc
#################################################################
#                                                               #
#       this routine starts a new line printer listing page     #
#       if the current listing is past line 50 on the page.     #
#                                                               #
#       AUTHOR: unknown                                         #
#       modified:       04-06-83    JAH     convert to ratfor   #
#                                                               #
#       arguments:                                              #
#         lpline     input   current listing line (maintained   #
#                            by calling routine)                #
#         inm        input   tape name being listed.            #
#################################################################
#
#	I apologize to all who read this routine.
#	There is something wrong with the way this fortran
#   compiler generates format statements.  I tried a bunch
#   of ways to get it to linefeed 4 times after a formfeed.
#   It is now kludged to work. timj.
#
    include "../common/lundefs"
	character*(*) inm

	if (lpline==0 | lpline>55) {
		write(lstlun,100)
		write(lstlun,150)
		write(lstlun,151)
		write(lstlun,152)
		write(lstlun,202) inm
		lpline = 5
	}
	return
100 format("")
150 format(1x,"		 ")
151 format(1x,"		 ")
152 format(1x,"		 ")
202	format (50x, a, /, 49x, 10(1h*), /)
	end

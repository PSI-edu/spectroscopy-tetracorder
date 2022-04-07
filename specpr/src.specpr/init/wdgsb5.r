	subroutine wdgsb5
	implicit integer*4 (i-n)
#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Fortran
#ccc
#ccc  short description:
#ccc                    This subroutine displays various commands
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc                     none
#ccc  argument list description:
#ccc       argument: none
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc
	include "../common/lundefs"
	include "../common/lblprt"

	write (ttyout,330)
	write (ttyout, 331)

	if ( vfollow > 0) write (ttyout, 332) 'v'
	if ( wfollow > 0) write (ttyout, 332) 'w'
	if ( dfollow > 0) write (ttyout, 332) 'd'
	if ( ufollow > 0) write (ttyout, 332) 'u'
	if ( yfollow > 0) write (ttyout, 332) 'y'

	write (ttyout, 333)
	return

330   format (20x, '*** SETUP parameters ***', /)

331   format (5x, 
          'type  o    to change the OBSERVATORY or observatory site',/,
      5x, 'type  r    to REASSIGN FILES and devices', /,5x,
          'type  f    to EVALUATE PROTECTION vs file sizes', /,5x,
          '           (no response indicates all is consistent)',/,5x,
          'type  g    and number to set GRAPHICS type (see manual)',/,5x,
          'type  b    to toggle BELL',/,5x,
          'type  v    to change the NAME of file v',/,5x,
          'type  d    to change the NAME of file d',/,5x,
          'type  u    to change the NAME of file u',/,5x,
          'type  y    to change the NAME of file y',/,5x,
          'type  w    to change the NAME of file w',/,5x,
          'type  cp   to change the  FILE PROTECTION',/)

332    format (5x,
	   'If file ',a1,' grows in size, protection ',
                           'will be automatically updated')

333    format (/, 5x,
          'press return to go back to the MAIN routines.', /)
      end

	subroutine what(i)
	implicit integer*4 (i-n)

#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc               This subroutine flags an error in user input
#ccc               given by the variable i (the column count in
#ccc               the array iopcon). i is the variable used in
#ccc               wjfren and is the value returned when the error
#ccc               was detected (as determined by the calling routine.
#ccc
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc                    none

#ccc  argument list description:
#ccc      argument: i
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc
########################################################################
#    this subroutine flags an error in user input given by the         #
#    variable  i  (the column count in the array iopcon).              #
#    i  is the variable used in wjfren and is the value returned       #
#    when the error was detected (as determined by the calling         #
#    routine).                                                         #
########################################################################
	include "../common/lbl4"
	include "../common/cmd"
	include "../common/lundefs"
	include "../common/filenames"
	include "../common/pipes"
	include "../common/iocontrol"

	if (i == -1) go to 1000

	if (i>2 & i<80) {
		iopcon = ' '
		iopcon(i-1:i-1) = '^'
		write(ttyout,10)iopcon(2:79)
		return
	}
	write(ttyout,20)

# now if reading commands from a file, turn it off

1000	if (redire) {
		write (ttyout,30)

2000		close(pipe(pipelv))
		if (pipelv <= 1) {
			open(5,file=TTY)
			redire = .false.
			return
		} else {
			pipelv = pipelv -1
			go to 2000
		}

		ioutverbose = 0   # do all usr prompt output
		redire = .false.  # no more redirection
	}

	return

10      format(1x,'?',a,/)
20      format(' ?',/)
30	format(' ** ERROR: turning off command redirection',/)
	end

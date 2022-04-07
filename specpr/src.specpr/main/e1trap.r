	subroutine e1trap

        include "../common/lbl4"
        include "../common/cmd"
        include "../common/lundefs"


	write (ttyout,1) char(7), char(7)
1	format (/,' ', a, '***********   ERROR   ************',/,
		' There is an "IEEE invalid operation" condition',
		' on this data.',//, ' Check all values by hand before',
		' proceeding.    ***** IMPORTANT **********',a,//)

# now if reading commands from a file, turn it off

        if (redire) {
                write (ttyout,30)
                redire = .false.
        }

	write (ttyout, 20)
20	format (' press return to continue')

	call crtin

30      format(' ** ERROR: turning off command redirection',/)

	return
	end

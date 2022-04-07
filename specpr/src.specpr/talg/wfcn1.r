#
        subroutine wfcn1(ilf, ifcn, x)
#
#
#
        implicit integer*4 (i-n)
        include "../common/lbl4"
	include "../common/lundefs"
        character*80 ia
        character*80 itemp
#
#    There are some things this subroutine must handle
# correctly. The first is to accept a function name
# that can have any number of inserted blanks and
# still be recognized. For this, all blanks are 
# compresed out by subroutine comprs.
#    Secondly, functions 7 and 8 require a number to
# be decoded.  This is done with a call to wjfren.
#    Thirdly, if the input is not recognized, a 
# negative function number will be returned.
#
        itemp = ' '
        ia = ' '
        itemp = iopcon
	x=0.0
#
#
        ia = iopcon(ilf:80)
        call comprs(ia)
        iopcon = ia
#
# Note: some compilers have a limit of 20 if-then-else
#       so change to if and no else.  This is technically
#       less efficient, but machines are plenty fast these days.
#           - RNC 12/22/2009

        ifcn = -1

        if (ia == 'exp') {
            ifcn = 2
	}
        if (ia == 'ln') {
            ifcn = 3
	}
        if (ia == 'log') {
            ifcn = 4
	}
        if (ia == '10**x') {
            ifcn = 5
	}
        if (ia == '1/x') {
            ifcn = 6
	}
        if (ia(1:4) == 'x**c') {
            ifcn = 7
	}
        if (ia(1:4) == 'c**x') {
            ifcn = 8
	}
        if (ia == 'sin') {
            ifcn = 9
	}
        if (ia == 'cos') {
            ifcn = 10
	}
        if (ia == 'tan') {
            ifcn = 11
	}
        if (ia == 'invcos') {
            ifcn = 12
	}
        if (ia == 'invsin') {
            ifcn = 13
	}
        if (ia == 'invtan') {
            ifcn = 14
	}
        if (ia == 'sind') {
            ifcn = 15
	}
        if (ia == 'cosd') {
            ifcn = 16
	}
        if (ia == 'tand') {
            ifcn = 17
	}
        if (ia == 'invcosd') {
            ifcn = 18
	}
        if (ia == 'invsind') {
            ifcn = 19
	}
        if (ia == 'invtand') {
            ifcn = 20
	}
        if (ia == 'cosh') {
            ifcn = 21
	}
        if (ia == 'sinh') {
            ifcn = 22
	}
        if (ia == 'tanh') {
            ifcn = 23
	}
        if (ia == 'abs') {
            ifcn = 24
	}
        if (ia == 'int') {
            ifcn = 25
	}
        if (ia == 'frac') {
            ifcn = 26
	}
        if (ia == '1/xe') {
            ifcn = 27
	}
        if (ifcn == -1) {
            write(ttyout,10)
        }
        if (ifcn == 7 || ifcn == 8) {
            i=5
            call wjfren(i,x,il)
        }
        iopcon = itemp
        return
10      format(' invalid function name')
        end

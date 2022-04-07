	subroutine getcmdargs
	implicit integer*4 (i-n)

#ccc  name: getcmdargs
#ccc  version date: 12/2/88
#ccc  author(s): Roger N. Clark
#ccc  language: Ratfor
#ccc
#ccc  short description: get the command line arguments and put them
#ccc			into /cmdarg/ common block
#ccc
#ccc  algorithm description: non hpux: use iargc, getarc sys calls,
#ccc                         hpux: use fortran "program" statement feature
#ccc  system requirements: unix: iargc, getarg
#ccc  subroutines called: unix: iargc, getarg
#ccc  argument list description: none
#ccc  parameter description: none
#ccc  common description: cmdarg
#ccc  message files referenced: none
#ccc  internal variables:
#ccc  file description: none
#ccc  user command lines: none
#ccc  update information:
#ccc  NOTES:
#ccc
#ccc  02/28/2002
#ccc      Modified to accommodate Fortran 90,etc on HPUX (IA64HPUX)

	include "../common/cmdarg"
	include "../common/lundefs"

	integer*4	iargc

	ncmdarg = 0

# make sure arrays are blank

charg1 = ' '
charg2 = ' '
charg3 = ' '
charg4 = ' '
charg5 = ' '
charg6 = ' '
charg7 = ' '
charg8 = ' '
charg9 = ' '
charg10= ' '
charg11= ' '
charg12= ' '


######### linux no dummy in iargc call:
#NONHPUX	ncmdarg = iargc(dummy)
#SUNOS		ncmdarg = iargc(dummy)
#SOLARIS	ncmdarg = iargc(dummy)
#LINUX		ncmdarg = iargc()
#IA64HPUX	ncmdarg = iargc()

#write (ttyout,*) 'DEBUG: ncmdarg =', ncmdarg

if (ncmdarg >= 1)  call getarg( 1,charg1)
if (ncmdarg >= 2)  call getarg( 2,charg2)
if (ncmdarg >= 3)  call getarg( 3,charg3)
if (ncmdarg >= 4)  call getarg( 4,charg4)
if (ncmdarg >= 5)  call getarg( 5,charg5)
if (ncmdarg >= 6)  call getarg( 6,charg6)
if (ncmdarg >= 7)  call getarg( 7,charg7)
if (ncmdarg >= 8)  call getarg( 8,charg8)
if (ncmdarg >= 9)  call getarg( 9,charg9)
if (ncmdarg >= 10) call getarg(10,charg10)
if (ncmdarg >= 11) call getarg(11,charg10)
if (ncmdarg >= 12) call getarg(12,charg10)


# For HPUX, the arguments are already installed into the arrays
#     from the program statement in the main routine.

# now make sure there are no embedded nulls

	do i = 1, 80 {
		if (charg1(i:i)  == char(0)) charg1(i:i) = ' '
		if (charg2(i:i)  == char(0)) charg2(i:i) = ' '
		if (charg3(i:i)  == char(0)) charg3(i:i) = ' '
		if (charg4(i:i)  == char(0)) charg4(i:i) = ' '
		if (charg5(i:i)  == char(0)) charg5(i:i) = ' '
		if (charg6(i:i)  == char(0)) charg6(i:i) = ' '
		if (charg7(i:i)  == char(0)) charg7(i:i) = ' '
		if (charg8(i:i)  == char(0)) charg8(i:i) = ' '
		if (charg9(i:i)  == char(0)) charg9(i:i) = ' '
		if (charg10(i:i) == char(0)) charg10(i:i) = ' '
		if (charg11(i:i) == char(0)) charg11(i:i) = ' '
		if (charg12(i:i) == char(0)) charg12(i:i) = ' '
	}

# now find out how many arguments are non-blank and ncmdarg
#	if ncmdarg is zero (mainly for HPUX)
# note: commands must start with a non-blank

	if (ncmdarg == 0) {
		if (charg1(1:1)  != ' ') ncmdarg = 1
		if (charg2(1:1)  != ' ') ncmdarg = 2
		if (charg3(1:1)  != ' ') ncmdarg = 3
		if (charg4(1:1)  != ' ') ncmdarg = 4
		if (charg5(1:1)  != ' ') ncmdarg = 5
		if (charg6(1:1)  != ' ') ncmdarg = 6
		if (charg7(1:1)  != ' ') ncmdarg = 7
		if (charg8(1:1)  != ' ') ncmdarg = 8
		if (charg9(1:1)  != ' ') ncmdarg = 9
		if (charg10(1:1) != ' ') ncmdarg = 10
		if (charg11(1:1) != ' ') ncmdarg = 11
		if (charg12(1:1) != ' ') ncmdarg = 12
	}

### debug:
# write (ttyout, *) 'DEBUG: cmd args:'
# if (ncmdarg >= 1) write (ttyout, *) 'charg1=', charg1
# if (ncmdarg >= 2) write (ttyout, *) 'charg2=', charg2
# if (ncmdarg >= 3) write (ttyout, *) 'charg3=', charg3
# if (ncmdarg >= 4) write (ttyout, *) 'charg4=', charg4
# if (ncmdarg >= 5) write (ttyout, *) 'charg5=', charg5
# if (ncmdarg >= 6) write (ttyout, *) 'charg6=', charg6

	return
	end

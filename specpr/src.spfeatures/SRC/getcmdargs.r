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

	include "cmdarg.h"

	integer*4	iargc

	ncmdarg = 0
# RED Added SOLARIS and SUNOS

# make sure arrays are blank
#NONHPUX	charg(1) = ' '
#NONHPUX	charg(2) = ' '
#NONHPUX	charg(3) = ' '
#NONHPUX        charg(4) = ' '
#NONHPUX        charg(5) = ' '
#NONHPUX        charg(6) = ' '
#NONHPUX        charg(7) = ' '
#NONHPUX        charg(8) = ' '
#NONHPUX        charg(9) = ' '
#NONHPUX        charg(10) = ' '
#NONHPUX        charg(11) = ' '
#NONHPUX        charg(12) = ' '
#NONHPUX        charg(13) = ' '
#NONHPUX        charg(14) = ' '
#NONHPUX        charg(15) = ' '
#NONHPUX        charg(16) = ' '
#NONHPUX        charg(17) = ' '
#NONHPUX        charg(18) = ' '
#NONHPUX        charg(19) = ' '
#NONHPUX        charg(20) = ' '

#SOLARIS        charg(1) = ' '
#SOLARIS        charg(2) = ' '
#SOLARIS        charg(3) = ' '
#SOLARIS        charg(4) = ' '
#SOLARIS        charg(5) = ' '
#SOLARIS        charg(6) = ' '                                                           
#SOLARIS        charg(7) = ' '
#SOLARIS        charg(8) = ' '
#SOLARIS        charg(9) = ' '
#SOLARIS        charg(10) = ' '
#SOLARIS        charg(11) = ' '
#SOLARIS        charg(12) = ' '
#SOLARIS        charg(13) = ' '
#SOLARIS        charg(14) = ' '
#SOLARIS        charg(15) = ' '
#SOLARIS        charg(16) = ' '
#SOLARIS        charg(17) = ' '
#SOLARIS        charg(18) = ' '
#SOLARIS        charg(19) = ' '
#SOLARIS        charg(20) = ' '

#SUNOS        charg(1) = ' '
#SUNOS        charg(2) = ' '
#SUNOS        charg(3) = ' '
#SUNOS        charg(4) = ' '
#SUNOS        charg(5) = ' '
#SUNOS        charg(6) = ' '                                                           
#SUNOS        charg(7) = ' '
#SUNOS        charg(8) = ' '
#SUNOS        charg(9) = ' '
#SUNOS        charg(10) = ' '
#SUNOS        charg(11) = ' '
#SUNOS        charg(12) = ' '
#SUNOS        charg(13) = ' '
#SUNOS        charg(14) = ' '
#SUNOS        charg(15) = ' '
#SUNOS        charg(16) = ' '
#SUNOS        charg(17) = ' '
#SUNOS        charg(18) = ' '
#SUNOS        charg(19) = ' '
#SUNOS        charg(20) = ' '

#LINUX          charg(1) = ' '
#LINUX          charg(2) = ' '
#LINUX          charg(3) = ' '
#LINUX          charg(4) = ' '
#LINUX          charg(5) = ' '
#LINUX          charg(6) = ' '
#LINUX          charg(7) = ' '
#LINUX          charg(8) = ' '
#LINUX          charg(9) = ' '
#LINUX          charg(10) = ' '
#LINUX          charg(11) = ' '
#LINUX          charg(12) = ' '
#LINUX          charg(13) = ' '
#LINUX          charg(14) = ' '
#LINUX          charg(15) = ' '
#LINUX          charg(16) = ' '
#LINUX          charg(17) = ' '
#LINUX          charg(18) = ' '
#LINUX          charg(19) = ' '
#LINUX          charg(20) = ' '

#IA64HPUX	charg(1) = ' '
#IA64HPUX	charg(2) = ' '
#IA64HPUX	charg(3) = ' '
#IA64HPUX       charg(4) = ' '                                                     
#IA64HPUX       charg(5) = ' '
#IA64HPUX       charg(6) = ' '
#IA64HPUX       charg(7) = ' '                                                     
#IA64HPUX       charg(8) = ' '
#IA64HPUX       charg(9) = ' '
#IA64HPUX       charg(10) = ' '                                                     
#IA64HPUX       charg(11) = ' '
#IA64HPUX       charg(12) = ' '
#IA64HPUX       charg(13) = ' '
#IA64HPUX       charg(14) = ' '                                                     
#IA64HPUX       charg(15) = ' '
#IA64HPUX       charg(16) = ' '
#IA64HPUX       charg(17) = ' '                                                     
#IA64HPUX       charg(18) = ' '
#IA64HPUX       charg(19) = ' '
#IA64HPUX       charg(20) = ' '

#NONHPUX	ncmdarg = iargc(dummy)
#NONHPUX	if (ncmdarg >= 1) call getarg(1,charg(1))
#NONHPUX	if (ncmdarg >= 2) call getarg(2,charg(2))
#NONHPUX	if (ncmdarg >= 3) call getarg(3,charg(3))
#NONHPUX	if (ncmdarg >= 4) call getarg(4,charg(4))
#NONHPUX        if (ncmdarg >= 5) call getarg(5,charg(5))
#NONHPUX        if (ncmdarg >= 6) call getarg(6,charg(6))
#NONHPUX        if (ncmdarg >= 7) call getarg(7,charg(7))
#NONHPUX        if (ncmdarg >= 8) call getarg(8,charg(8))
#NONHPUX        if (ncmdarg >= 9) call getarg(9,charg(9))
#NONHPUX        if (ncmdarg >= 10) call getarg(10,charg(10))
#NONHPUX        if (ncmdarg >= 11) call getarg(11,charg(11))
#NONHPUX        if (ncmdarg >= 12) call getarg(12,charg(12))
#NONHPUX        if (ncmdarg >= 13) call getarg(13,charg(13))
#NONHPUX        if (ncmdarg >= 14) call getarg(14,charg(14))
#NONHPUX        if (ncmdarg >= 15) call getarg(15,charg(15))
#NONHPUX        if (ncmdarg >= 16) call getarg(16,charg(16))
#NONHPUX        if (ncmdarg >= 17) call getarg(17,charg(17))
#NONHPUX        if (ncmdarg >= 18) call getarg(18,charg(18))
#NONHPUX        if (ncmdarg >= 19) call getarg(19,charg(19))
#NONHPUX        if (ncmdarg >= 20) call getarg(20,charg(20))

#SOLARIS        ncmdarg = iargc(dummy)
#SOLARIS        if (ncmdarg >= 1) call getarg(1,charg(1))
#SOLARIS        if (ncmdarg >= 2) call getarg(2,charg(2))
#SOLARIS        if (ncmdarg >= 3) call getarg(3,charg(3))
#SOLARIS        if (ncmdarg >= 4) call getarg(4,charg(4))
#SOLARIS        if (ncmdarg >= 5) call getarg(5,charg(5))
#SOLARIS        if (ncmdarg >= 6) call getarg(6,charg(6))
#SOLARIS        if (ncmdarg >= 7) call getarg(7,charg(7))
#SOLARIS        if (ncmdarg >= 8) call getarg(8,charg(8))
#SOLARIS        if (ncmdarg >= 9) call getarg(9,charg(9))
#SOLARIS        if (ncmdarg >= 10) call getarg(10,charg(10))
#SOLARIS        if (ncmdarg >= 11) call getarg(11,charg(11))
#SOLARIS        if (ncmdarg >= 12) call getarg(12,charg(12))
#SOLARIS        if (ncmdarg >= 13) call getarg(13,charg(13))
#SOLARIS        if (ncmdarg >= 14) call getarg(14,charg(14))
#SOLARIS        if (ncmdarg >= 15) call getarg(15,charg(15))
#SOLARIS        if (ncmdarg >= 16) call getarg(16,charg(16))
#SOLARIS        if (ncmdarg >= 17) call getarg(17,charg(17))
#SOLARIS        if (ncmdarg >= 18) call getarg(18,charg(18))
#SOLARIS        if (ncmdarg >= 19) call getarg(19,charg(19))
#SOLARIS        if (ncmdarg >= 20) call getarg(20,charg(20))

#SUNOS        ncmdarg = iargc(dummy)
#SUNOS        if (ncmdarg >= 1) call getarg(1,charg(1))
#SUNOS        if (ncmdarg >= 2) call getarg(2,charg(2))
#SUNOS        if (ncmdarg >= 3) call getarg(3,charg(3))
#SUNOS        if (ncmdarg >= 4) call getarg(4,charg(4))
#SUNOS        if (ncmdarg >= 5) call getarg(5,charg(5))
#SUNOS        if (ncmdarg >= 6) call getarg(6,charg(6))
#SUNOS        if (ncmdarg >= 7) call getarg(7,charg(7))
#SUNOS        if (ncmdarg >= 8) call getarg(8,charg(8))
#SUNOS        if (ncmdarg >= 9) call getarg(9,charg(9))
#SUNOS        if (ncmdarg >= 10) call getarg(10,charg(10))
#SUNOS        if (ncmdarg >= 11) call getarg(11,charg(11))
#SUNOS        if (ncmdarg >= 12) call getarg(12,charg(12))
#SUNOS        if (ncmdarg >= 13) call getarg(13,charg(13))
#SUNOS        if (ncmdarg >= 14) call getarg(14,charg(14))
#SUNOS        if (ncmdarg >= 15) call getarg(15,charg(15))
#SUNOS        if (ncmdarg >= 16) call getarg(16,charg(16))
#SUNOS        if (ncmdarg >= 17) call getarg(17,charg(17))
#SUNOS        if (ncmdarg >= 18) call getarg(18,charg(18))
#SUNOS        if (ncmdarg >= 19) call getarg(19,charg(19))
#SUNOS        if (ncmdarg >= 20) call getarg(20,charg(20))

#LINUX          ncmdarg = iargc()
#LINUX          if (ncmdarg >= 1) call getarg(1,charg(1))
#LINUX          if (ncmdarg >= 2) call getarg(2,charg(2))
#LINUX          if (ncmdarg >= 3) call getarg(3,charg(3))
#LINUX          if (ncmdarg >= 4) call getarg(4,charg(4))
#LINUX          if (ncmdarg >= 5) call getarg(5,charg(5))
#LINUX          if (ncmdarg >= 6) call getarg(6,charg(6))
#LINUX          if (ncmdarg >= 7) call getarg(7,charg(7))
#LINUX          if (ncmdarg >= 8) call getarg(8,charg(8))
#LINUX          if (ncmdarg >= 9) call getarg(9,charg(9))
#LINUX          if (ncmdarg >= 10) call getarg(10,charg(10))
#LINUX          if (ncmdarg >= 11) call getarg(11,charg(11))
#LINUX          if (ncmdarg >= 12) call getarg(12,charg(12))
#LINUX          if (ncmdarg >= 13) call getarg(13,charg(13))
#LINUX          if (ncmdarg >= 14) call getarg(14,charg(14))
#LINUX          if (ncmdarg >= 15) call getarg(15,charg(15))
#LINUX          if (ncmdarg >= 16) call getarg(16,charg(16))
#LINUX          if (ncmdarg >= 17) call getarg(17,charg(17))
#LINUX          if (ncmdarg >= 18) call getarg(18,charg(18))
#LINUX          if (ncmdarg >= 19) call getarg(19,charg(19))
#LINUX          if (ncmdarg >= 20) call getarg(20,charg(20))

#IA64HPUX	ncmdarg = iargc()
#IA64HPUX	if (ncmdarg >= 1) call getarg(1,charg(1))
#IA64HPUX	if (ncmdarg >= 2) call getarg(2,charg(2))
#IA64HPUX	if (ncmdarg >= 3) call getarg(3,charg(3))
#IA64HPUX       if (ncmdarg >= 4) call getarg(4,charg(4))
#IA64HPUX       if (ncmdarg >= 5) call getarg(5,charg(5))
#IA64HPUX       if (ncmdarg >= 6) call getarg(6,charg(6))
#IA64HPUX       if (ncmdarg >= 7) call getarg(7,charg(7))
#IA64HPUX       if (ncmdarg >= 8) call getarg(8,charg(8))
#IA64HPUX       if (ncmdarg >= 9) call getarg(9,charg(9))
#IA64HPUX       if (ncmdarg >= 10) call getarg(10,charg(10))
#IA64HPUX       if (ncmdarg >= 11) call getarg(11,charg(11))
#IA64HPUX       if (ncmdarg >= 12) call getarg(12,charg(12))
#IA64HPUX       if (ncmdarg >= 13) call getarg(13,charg(13))
#IA64HPUX       if (ncmdarg >= 14) call getarg(14,charg(14))
#IA64HPUX       if (ncmdarg >= 15) call getarg(15,charg(15))
#IA64HPUX       if (ncmdarg >= 16) call getarg(16,charg(16))
#IA64HPUX       if (ncmdarg >= 17) call getarg(17,charg(17))
#IA64HPUX       if (ncmdarg >= 18) call getarg(18,charg(18))
#IA64HPUX       if (ncmdarg >= 19) call getarg(19,charg(19))
#IA64HPUX       if (ncmdarg >= 20) call getarg(20,charg(20))


# For HPUX, the arguments are already installed into the arrays
#     from the program statement in the main routine.

# now make sure there are no embedded nulls

	do j = 1, 20 {
		do i = 1, 80 {
			if (charg(j)(i:i) == char(0)) charg(j)(i:i) = ' '
		}
	}

# now find out how many arguments are non-blank and ncmdarg
#	if ncmdarg is zero (mainly for HPUX)
# note: commands must start with a non-blank

	if (ncmdarg == 0) {
		do j = 1, 20 {
			if (charg(j)(1:1) == ' ') break
			ncmdarg = j
		}
	}

	return
	end

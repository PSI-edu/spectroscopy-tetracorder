integer*4 function search(renum,restr,strng)
#
# alias for HPUX because HPUX fortran doesn't pass character arrays
#  by reference, however, it seems to when passing only one array!
#  but if pass two arrays, it crashes with segmentation violation
#  unless the ALIAS is used (How WEIRD!!!)

#HPUX%$ALIAS reexex="reexec"(%REF,%REF)

implicit integer*4 (i-n)
	include "../common/spmaxes"   # max parameters, must be first


include "srchdefs.h"
include "../common/alphabet"
include "../common/lbl4"
include "../common/label1"

integer*2 renum(1:MAXFIELD)
integer*2 ibit, chkbit
integer*4 iyr, imon, iday
integer*4 itextf
#RED
integer*4 retnum

character*10 date1

	integer*4 reexec
	integer*4 reexex

character restr(1:MAXFIELD)*1
character strng(1:MAXFIELD)*80
character strng2(1:MAXFIELD)*80
character str*297


retnum=1
itextf=0
ibit = 1  # text bit
if (chkbit(icflag,ibit) == 1) itextf = 1  #text data record

for (iptr=1;iptr<=MAXFIELD;iptr=iptr+1) {
	if (restr(iptr) == 't') 
		str=ititl // char(0)
	else if (restr(iptr) == 'h') {
		if (itextf == 0) {
			str=ihist // char(0)
		} else {
			str = char(0)  # text data record: no hist
		}
	}
	else if (restr(iptr) == 'd') {
		if (itextf == 0) {
			call frjuld(iyr,imon,iday,jdateb)  #get date 
							   # data obtained
			write (date1 ,100) imon, iday, iyr
100			format (i2,'/',i2,'/',i4)
			do itemp = 1, 10 {
				if (date1(itemp:itemp) == 
					' ') date1(itemp:itemp)='0'
			}
			str = date1 // char(0)
		} else {
			str = char(0)  # text data record: no date
		}
	}
	else if (restr(iptr) == 'm') {
		if (itextf == 0) {
			str=mhist // char(0)
		} else {
			str = char(0)  # text data record: no mhist
		}
	}
	else {
		if (iptr == 1) {
# next line not needed (Roger N. Clark 10/1/85)?
#			call  recomp("." // char(0))
#
			search=1 
			return 
		}
		else {

			search=retnum
			return
		}
	}
#
# This section compiles a string and compares it to the fields
# specified.  This also handles the logical operators within
# the command simulating AND with multiplication and OR with
# addition.
#
# RED Added SOLARIS and SUNOS 
#NONHPUX    call  recomp(strng(iptr))
#SOLARIS    call  recomp(strng(iptr))
#SUNOS      call  recomp(strng(iptr))
#LINUX      call  recomp(strng(iptr))

#NONHPUX	itemp = reexec(strng(iptr), str)
#SOLARIS        itemp = reexec(strng(iptr), str)
#SUNOS          itemp = reexec(strng(iptr), str)
#LINUX		itemp = reexec(strng(iptr), str)
#HPUX		itemp = reexex(strng(iptr), str)
#IA64HPUX	itemp = reexec(strng(iptr), str)
	if (itemp == ERROR) {
		print *,"String not compiled correctly"
		search=0
		return
	}
		retnum = retnum*itemp
    if (renum(iptr) == AND) {
		if (iptr == 1) retnum = 1
		retnum = retnum*itemp
		if (retnum > 0) retnum = 1
	}
	else if (renum(iptr) == OR) {
		if (iptr == 1) retnum = 0
		retnum = retnum+itemp
		if (retnum > 0) retnum = 1
	}
	
}

search=retnum
return
end

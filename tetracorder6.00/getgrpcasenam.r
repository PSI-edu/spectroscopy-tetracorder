	subroutine getgrpcasenam 

	implicit integer*4 (i-n)

#ccc  name:         getgrpcasenam
#ccc  version date: 
#ccc  author(s): Roger N. Clark
#ccc  language: ratfor
#ccc
#ccc  short description: sets group and case names
#ccc                     for tetracorder
#ccc
#ccc  algorithm description: 
#ccc  system requirements: Linux
#ccc  subroutines called: many specpr routines, need specpr.a library
#ccc  argument list description: see below
#ccc  parameter description: see below
#ccc  common description: see below
#ccc  message files referenced: none
#ccc  internal variables: see below
#ccc  file description: see below
#ccc  user command lines: see below
#ccc  update information: see below
#ccc  NOTES:
#ccc

	include "../specpr/src.specpr/common/spmaxes"   # max parameters, must be first

	include 	"../specpr/src.specpr/common/label1"
	include 	"../specpr/src.specpr/common/lbl3"
	include 	"../specpr/src.specpr/common/lbl4"
	include 	"../specpr/src.specpr/common/lbl7"
	include 	"../specpr/src.specpr/common/lundefs"
	include 	"../specpr/src.specpr/common/alphabet"
	include 	"../specpr/src.specpr/common/cmd"
	include 	"../specpr/src.specpr/common/lblg"
	include 	"../specpr/src.specpr/common/lblwav"
	include 	"../specpr/src.specpr/common/cmdarg"
	include 	"../specpr/src.specpr/common/dscrch"
	include 	"../specpr/src.specpr/common/ioftyp"
	include 	"../specpr/src.specpr/common/blank"
	include		"../specpr/src.specpr/common/lblvol"

# arrays for multiple materials

	include "multmap.h"

	include "tricube.h"

# basic tetracorder parameters

	include "tri1.h"

#RED
	integer*4 ihchar     # function
	logical*4 fexist     # file exists: true, false if doesn't

	integer*4 cmdverbose   # function cmdverbose
	integer*4 fnb          # function fnb
	integer*4 lnb          # function lnb
	integer*4 ictlimflg, ictrlimflg, ictllimflg
	integer*4 ichtmp2, ichtmp1, ndl
	integer*4 itmpfr, nfr, irtn

	character*1 imch(5)
	character*20 chtest
	character*110 chtmp1, chtmp2
	character cht1*3, cht2*5, cht3*2
	integer*4 imat   # material number

	integer*4 ignum, icnum  # group, case number

	real*4 x

	irtn = 0

###########################################################
## define group and case names
#
# start with: define grougcasenames
#
# group  #   name
# case   #   name
#
# end   with: endgroupcasenames


310	if (cmdverbose(-1) <= 1) write (ttyout, 305)
305	format(///,'Enter:  define grougcasenames  to start the group/case name block')

	call crtin
	if (cmdverbose(-1) <= 2) write (ttyout,*) iopcon
	i = 1
	call wjfren (i,x,il)
	if (i >= 80 & il == 0) {
		write (lunhist,*) ' '   # blank line
		go to 310    # blank line, read again
	}

	#write (ttyout,*) 'DEBUG action: i=',i,' iopcon(i:80)=',iopcon(i:80)

	ik = index(iopcon,'define grougcasenames')
	if (ik > 0) {                 # define grougcasenames found
		#write (ttyout,*) 'DEBUG: define grougcasenames found'
		write (lunhist,*) 'define grougcasenames'

	}  else if (il==ihx || il==ihe)  {
		ic=il
		icrst = 1
		call rstart(icrst)
		call what (-1)
		irtn = 1
		return
	} else {

		write (ttyout,*) 'ERROR: define grougcasenames NOT FOUND'	
		go to 310
	}

510	if (cmdverbose(-1) <= 1) write (ttyout,505)
505	format(///,'Enter: groupname  #   name  (name <= 12 chracters) or',/,
		   '       casename   #   name  (name <= 12 chracters) or',/,
		   '       endgroupcasenames',//,
		   '       example:',/,
		   '       groupname  1  1um region',/,
		   '       casename   1  veg red-edge')

	call crtin
	if (cmdverbose(-1) <= 2) write (ttyout,*) iopcon
	i = 1
	call wjfren (i,x,il)
	if (i >= 80 & il == 0) {
		write (lunhist,*) ' '   # blank line
		go to 510    # blank line, read again
	}

	#write (ttyout,*) 'DEBUG action: i=',i,' iopcon(i:80)=',iopcon(i:80)

	ik = index(iopcon,'endgroupcasenames')
	if (ik > 0) {                 # end of action input
		write (ttyout,*) 'DEBUG: endgroupcasenames found'
		write (lunhist,*) 'endgroupcasenames'
		go to 3112         # complete, end of block
	}

	if (il==ihx || il==ihe)  {
		ic=il
		icrst = 1
		call rstart(icrst)
		call what (-1)
		irtn = 1
		return
	}

###########  groupname or casename

	ik = index(iopcon,'groupname')
	jk = index(iopcon,'casename')
	if (ik == 0 && jk == 0) {
		call what (-1)
		write (ttyout,*) 'ERROR: groupname or casename keyword not found'
		go to 510
	}

###########  groupname

	if (ik > 0) {

		i = i + 9           # skip over groupname

		call wjfren(i,x,il)            # get group #
		
		if (i > 70) {
			call what(i)
			write (ttyout,*) 'ERROR: up to column 70: what group #?'
			go to 510
		}
		ignum = int(x)  # group #

		if ( ignum < 0 || ignum > ngroups) {

			write (ttyout,*) 'ERROR: group # out of range:', ignum
			go to 510
		}
		
		#call wjfren(i,x,il)            # get group name
		for (j=i; j<=67; j=j+1) {  # next non blank
			if (iopcon(j:j) != ' ') {
				i=j
				break
			}
		}
		#i = i-1
		if (i > 67) {
			call what(i)
			write (ttyout,*) 'ERROR: up to column 67: no name, insufficient room to get name'
			go to 510
		}
		groupname(ignum) = iopcon(i:i+11)

		write (lunhist,*) 'groupname', ignum, groupname(ignum)
		write (ttyout,*) 'DEBUG: groupname', ignum, groupname(ignum)

		go to 510  # get another entry
	}
	

###########  casename

	if (jk > 0) {

		i = i + 9           # skip over casename

		call wjfren(i,x,il)            # get case #
		
		if (i > 70) {
			call what(i)
			write (ttyout,*) 'ERROR: up to column 70: what case #?'
			go to 510
		}
		icnum = int(x)  # case #

		if ( icnum < 0 || icnum > ncases) {

			write (ttyout,*) 'ERROR: case # out of range:', icnum
			go to 510
		}
		
		#call wjfren(i,x,il)            # get case name
		for (j=i; j<=67; j=j+1) {  # next non blank
			if (iopcon(j:j) != ' ') {
				i=j
				break
			}
		}
		#i = i-1
		if (i > 67) {
			call what(i)
			write (ttyout,*) 'ERROR: up to column 67: no name, insufficient room to get name'
			go to 510
		}
		casename(icnum) = iopcon(i:i+11)

		write (lunhist,*) 'casename', icnum, casename(icnum)
		write (ttyout,*) 'DEBUG: casename', icnum, casename(icnum)

		go to 510  # get another entry
		
	}

	# if none of the above key words are found, will get to here 

	call what(i)
	write (ttyout,*) 'ERROR: unexpected input'
	go to 510

######## done:

3112	write (lunhist,3111) ihbcksl
3111	format (a,'#',70('#'))
	write (ttyout,*) 'DEBUG: exiting getgrpcasenam'

	return
	end

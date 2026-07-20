	subroutine getaction (imat, irtn)

	implicit integer*4 (i-n)

#ccc  name:         reflsetup
#ccc  version date: 
#ccc  author(s): Roger N. Clark
#ccc  language: ratfor
#ccc
#ccc  short description: sets up the reference library features
#ccc                     for tetracorder to map with.
#ccc
#ccc  algorithm description: See Clark et al, 1990, JPL AVIRIS Conf.
#ccc  system requirements: Unix
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
#ccc            This routine generates a band depth map

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

	real*4 x

	irtn = 0

###########################################################
## action

504	nxx = 0  # number of cases, default
	dosound(imat) = 0
	istartcase = 1
	do jj = 1, maxcse {          # default: do no cases
		iaction(imat,jj) = 0
	}

310	if (cmdverbose(-1) <= 1) write (ttyout, 305)
305	format(///,'Enter:  define actions  to start the actions block')

	call crtin
	if (cmdverbose(-1) <= 2) write (ttyout,*) iopcon
	i = 1
	call wjfren (i,x,il)
	if (i >= 80 & il == 0) {
		write (lunhist,*) ' '   # blank line
		go to 310    # blank line, read again
	}

	#write (ttyout,*) 'DEBUG action: i=',i,' iopcon(i:80)=',iopcon(i:80)

	i = index(iopcon,'define actions')
	if (i > 0) {                 # define actions found
		#write (ttyout,*) 'DEBUG: define actions found'
		write (lunhist,*) 'define actions'
	}

	if (il==ihx || il==ihe)  {
		ic=il
		icrst = 1
		call rstart(icrst)
		call what (-1)
		irtn = 1
		return
	}

510	if (cmdverbose(-1) <= 1) write (ttyout,505)
505	format(///,'Enter: action: none   or:',/,
		   '       action: case #  (where # is a case number)',/,
		   '       action: sound1 "soundfile"',/,
		   '                 if no file name, use default output file',/,
		   '                 when done entering actions, enter:',/,
		   '       endaction')

	call crtin
	if (cmdverbose(-1) <= 2) write (ttyout,*) iopcon
	i = 1
	call wjfren (i,x,il)
	if (i >= 80 & il == 0) {
		write (lunhist,*) ' '   # blank line
		go to 510    # blank line, read again
	}

	#write (ttyout,*) 'DEBUG action: i=',i,' iopcon(i:80)=',iopcon(i:80)

	i = index(iopcon,'endaction')
	if (i > 0) {                 # end of action input
		#write (ttyout,*) 'DEBUG: endaction found'
		write (lunhist,*) 'endaction'
		go to 3112
	}

	if (il==ihx || il==ihe)  {
		ic=il
		icrst = 1
		call rstart(icrst)
		call what (-1)
		irtn = 1
		return
	}
	i = index(iopcon,'action:')
	if (i == 0) {
		call what (-1)
		write (ttyout,*) 'ERROR: action: keyword not found'
		go to 510
	}
	i = i + 7

	call wjfren(i,x,il)
	i = i -1
	
	if (i > 70) {
		call what(i)
		write (ttyout,*) 'ERROR: up to column 70: what action?'
		go to 510
	}

	#write (ttyout,*) 'DEBUG action: i=',i,' iopcon(i:80)=',iopcon(i:80)

	if (iopcon(i:i+3) == 'none') {

		#write (ttyout,*) 'DEBUG: action none found'

		write (lunhist,506) 'none'
506		format ('action: ',a)
		go to 510
	}

	if (iopcon(i:i+5) == 'sound1') {  # get sound file
		i = i +6
		#write (ttyout,*) 'DEBUG: action sound1 found'
		do jj = i, 79 {
			if (iopcon(jj:jj) != ' ') break
		}
		if (jj > 78) {  # no file name, use base output file name

			sound1fil(imat) = mfile(imat)(1:lenfile(imat)) // '.wav'
			dosound(imat) = 1
			#write (ttyout,*) 'DEBUG: soundfile=',sound1fil(imat)
			write (lunhist,506) 'sound1'
			go to 510
		}
		isfirst = jj
		i = jj + 1
		if (jj > 78) {
			write (ttyout,*)'ERROR: no sound file name'
			go to 510
		}
		do jj = i, 80 {
			if (iopcon(jj:jj) == ' ') go to 111 # find next blank
		}

111		islast = jj - 1  
		sound1fil(imat) = iopcon(isfirst:ilast)
		dosound(imat) = 1
		write (lunhist,1506) iopcon(isfirst:ilast)
1506		format ('action: sound1 ',a)
		go to 510
	}

	if (iopcon(i:i+3) == 'case') {

		#write (ttyout,*) 'DEBUG: action case found'

		i = i + 4
		jj1 = istartcase
		do jj = jj1, maxcse {
			#write (ttyout,*) 'DEBUG: case test ',jj,' i=',i
			call wjfren (i,x,il)
			if (il != 0) {
				call what (i)
				write (ttyout,509) il
509				format ('ERROR: invalid ',
					'character "',a1,
					'" when decoding case number')
				go to 510
			}
			if (jj > 1 & i > 78) go to 508
			ixx = x + 0.5
			#write (ttyout,*) 'DEBUG: x=',x,' i=',i,' ixx=',ixx
			if (ixx > 0 & ixx <= maxcse) {

				iaction(imat,jj) = ixx
				nxx = nxx + 1
				istartcase = istartcase + 1
			} else {
				#write (ttyout,*) 'DEBUG: i=',i
				call what(i)
				#write (ttyout,*) 'DEBUG: i=',i
				write (ttyout,*) 'ERROR: case ',ixx,
						'is out of range: 1 to ',
						 maxcse
				go to 510
			}

		}
508		write (lunhist,507) (iaction(imat,kk),kk = jj1, nxx)
507		format ('action: case',10(i4,1x))

		go to 510

	}

	# of none of the above ifs are found, will get to here 

		write (ttyout,*) 'ERROR: that action not allowed'
		call what(i)
		go to 510

3112	write (lunhist,3111) ihbcksl
3111	format (a,'#',70('#'))

	return
	end

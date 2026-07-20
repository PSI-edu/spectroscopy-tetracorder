	subroutine getnotfeat (i, ifeat, imat, irtn)

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
	integer*4 itmpfr, nfr, i, ifeat, imat, irtn

	character*1 imch(5)
	character*20 chtest
	character*110 chtmp1, chtmp2
	character cht1*3, cht2*5, cht3*2

	real*4 x

# define feature importance characters

	imch(1) = 'O'
	imch(2) = 'W'
	imch(3) = 'D'
	imch(4) = 'M'
	imch(5) = '?'

	irtn = 0 # return value good so far

#####################################################################
# The define featers block looks like below, as of 6/2011. 
#  This subroutine gets called when the "f" is found.

#define features                  \# Number of features, and not feats
#                  \# blank line recycles to same input line
#                  \# defining features outside of range of work is OK
#                  \# if outside, they are ignored
#   f1a Dw 0.902  0.930  1.080  1.110  ct 0.08 0.7
#   f1a                                rcbblc> .1 .2  \# shoulderness
#   f1a                                weight*2.2     \# multiply weight by 2.2
#   f1a ignore w 0.98 0.99  \# ignore wavelength range from 0.98 to 0.99
#   f1a ignore w 1.01 1.02  \# ignore wavelength range from 1.01 to 1.02
#                          \# can specify up to 3 ranges per feature.
#   f1a mode= all  \# could be airless, mars_orbit, earth_orbit, moon, lab, titan, comet ...
#   f1a move leftc left 2c    \# allow left continuum to move left up to 2 channels
#   f1a move rightc right 2c  \# allow left continuum to move right up to 2 channels
#
#   f2a Dw 1.150   1.178   1.315   1.345  ct 0.05 lct/rct> 0.9 1.1
#   f2a mode= all
#
#   f3b DW 3.4     3.5     3.6     3.7    ct 0.01 0.02   lct/rct> 0.9 1.1
#
#   n1a NOT [NOTGREENVEG]  1  0.50a 0.5   \# NOT veg, feat 1, depth .50, fit .5
#                \# the [NOTGREENVEG] could (should) be changed to the ID
#                \# no mode specified, so default = all
#
#    \# concept for future: need a "degrades to" so if diagnostic features are
#    \# all deleted, the entry degrades to a simpler answer.
#        \# how about a case: degrades to case X
#
#endfeatures


	itmp =cmdverbose(-1)
######	write (ttyout,*) 'DEBUG: cmdverbose(-1) = ',itmp

# we are already scanning the line at the time of entry to this subroutine
#    so we should alread have the "n" for not,
# so the  next thing is the not feature number
# scan position = i

# example input line:
#   n1a NOT    y  7170   1  0.15a 0.5   \# NOT vegetation, feat 1,
#
# i should be pointing to the number 1 in the above line.`


	go to 111  # skip over following block when we first come into
			# this subrouting.  Loop back to it (3095) for
			# successive lines until endfeatures found

#######################################################################
# Dec 17, 2015: this block apparently should not be here, delete?  RNC
#3095	call crtin
#	call wjfren(i,x1,il)   # not feature processing
#
######################################################################

305	if (cmdverbose(-1) <= 1) write (ttyout,303)
303	format (//,'NOT Features:',//,
		'  Enter NOT followed by the File ID, Rec. No., ',
				'feature #, threshold',
					' depth[a,r#] and threshold fit',/,
		'         where a  = absolute depth',/,
		'               r# = relative depth, so that',/,
		'                    NOT feat depth / r# depth is tested',/,
		'                    where # = a feature from the current',/,
                '                    material',/,
		'  Examples:',/,
		'     NOT [sprln01] 12 2 0.05a  0.3 \# absolute depth .05, fit .3',/,
		'     NOT [sprln01] 12 2 0.05r2 0.3 \# relative depth .05, fit .3',//,
		'  File ID and Rec No. must be an previosly entered',
			' spectrum')
		
	call crtin
	if (cmdverbose(-1) <= 2) write (ttyout,*) iopcon
	i = 1
	call wjfren(i,x1,il)
	if (i >= 80 & il == 0) go to 305    # blank line, read again

        if (il==ihe && iopcon(i-1:i+9) == 'endfeatures') { # endfeatures

		write (lunhist,*) 'endfeatures'
		if (cmdverbose(-1) <= 1) write (ttyout,*) 'DEBUG: endfeatures found'
                irtn = 0    # features complete
		return
        }
#        *** check for hard or soft exit ***
	if (il == ihx || il == ihe) {
		ic = il
		icrst = 1
		call rstart(icrst)
		call what (-1)
		return
	}

	# at this point only aother not featrue is expected, so expect an n
	if (il != ihn) {
		ic = il
		call what (-1)
		write (ttyout,*) 'ERROR: expected an n for a new not feature, or endfeatures'
		return
	}
########## end block for  ####################################

111	ireflibnum = 0  # which reference library to use (only a = 1 currently valid)

	call wjfren (i,x,il)  # next expect notfeature number

	itmpf=int(x+0.5)

	if (itmpf < 1 || itmpf > maxnotfeat) {
                call what(i)
                write (ttyout,*) 'ERROR: notfeature number out of range:', itmpf
                irtn = 1
                return
        }
        infeat=itmpf   # this is the current not feature number
	notfrn(infeat,imat) = 0  # initialize

	if (infeat > numnotfeat(imat)) numnotfeat(imat) = infeat  # set new maximum number

	if (il == 0) call wjfren (i,x,il)  # scan for next character

        jsplib = 0
        if (il == iha ) jsplib = 1
        if (il == ihb ) jsplib = 2
        if (il == ihc ) jsplib = 3
        if (il == ihd ) jsplib = 4
        if (il == ihe ) jsplib = 5
        if (il == ihf ) jsplib = 6
        if (il == ihg ) jsplib = 7
        if (il == ihh ) jsplib = 8
        if (il == ihi ) jsplib = 9
        if (jsplib > maxlibs  || jsplib == 0) {
                write (ttyout,*) 'ERROR: library letter code out of range'
                call what(i)
                irtn = 1
                return
        }

	call wjfren (i,x,il)  # next expect NOT


#	                   check for NOT key word
	if (il != ihcn) {
		call what (i)
		write (ttyout,*) 'NOT key word not found'
		go to 69
	}
	call wjfren(i,x1,il)
	if (il != ihco) {
		call what (i)
		write (ttyout,*) 'NOT key word not found'
		go to 69
	}
	call wjfren(i,x1,il)
	if (il != ihct) {
		call what (i)
		write (ttyout,*) 'NOT key word not found'
		go to 69
	}
	call wjfren(i,x1,idevb)
	if (i >= 79 && idevb == 0) {
		call what(i)
		go to 69
	}
	call wjfren(i,xfilb,ic2)

#       *** check for invalid input ***
	if (x1!=0.0 || ic2!=0 || xfilb<=0.0) {
		write(ttyout,66)
66              format ('invalid input, reenter')
		go to 69

#       *** looks ok so get which material ***
	} else {
		if (imat > 1) {
			notfid(infeat,imat) = 0
			do intmp = 1, imat -1 {
				###write (6,*) 'DEBUG:',intmp,ndevr(intmp),nrrec(jsplib,intmp),'=?',idevb,xfilb
				if (ndevr(jsplib,intmp) == idevb &
						nrrec(jsplib,intmp) == xfilb) {	
					notfid(infeat,imat) = idevb
					notrec(infeat,imat) = xfilb
					notmat(infeat,imat) = intmp
				}
			}
			if (notfid(infeat,imat) == 0) {
				call what(-1)
				write (ttyout,*) 'ERROR: NOT feature not found:',
							notfid(infeat,imat),
							notrec(infeat,imat)
				go to 69
			}
		} else {
			call what(-1)
			write (ttyout,*) 'ERROR: can not have NOT features with material 1'
			go to 69
		}

	}

	call wjfren(i,x, il)  # get feature number
	if (nint(x) <= nfeat(notmat(infeat,imat)) ) {   # check if NOT feature number
						#  is valid
		notfeat(infeat,imat) = x + 0.5
	} else {
		call what(-1)
		write (ttyout,*) 'ERROR: ',nint(x),'is not a valid feature in',
					'material ',notmat(infeat,imat),
					' which has ',
					nfeat(notmat(infeat,imat)),' features'
		go to 69
	}
	thrshdnot(infeat,imat) = 1.0
	call wjfren(i,x, il)  # get threshold depth
	if (x > 0.0 & x < 20.0 & (il == iha | il == ihr)) {
		thrshdnot(infeat,imat) = x
	} else {
		call what(i)
		write (ttyout,*) "threshold NOT depth out of range (>0, <20)"
		write (ttyout,*) "    or no [r] or [a] flag"
		go to 69
	}
	if (il == iha) {   # depth is absolute number

		notfrn(infeat,imat) = 0

	} else if (il == ihr) {   # depth is relative, so get what its relative to

		call wjfren(i,x, il)
		if (x > 0.0 & nint(x) <= nfeat(imat) & il == 0) {

			notfrn(infeat,imat) = nint(x)

		} else {
			call what(i)
			write (ttyout,*) "NOT relative feature invalid"
			go to 69
		}
	} else {
		call what(i)
                write (ttyout,*) "NOT feature: no [r] or [a] flag"
                go to 69
	}

	thrshfnot(infeat,imat) = 1.0
	call wjfren(i,x, il)  # get threshold fit
	if (x > 0.0 & x < 1.0 & il == 0) {
		thrshfnot(infeat,imat) = x
	} else {
		call what(i)
		write (ttyout,*) threshold NOT fit out of range
		go to 69
	}
	if (notfrn(infeat,imat) == 0) {            # absolute feat depth history
		write (lunhist, 3031) notfid(infeat,imat),
				notrec(infeat,imat),
				notfeat(infeat,imat),
				thrshdnot(infeat,imat),
				thrshfnot(infeat,imat),  ihbcksl,
				notmat(infeat,imat)
	} else {
		write (lunhist, 3033) notfid(infeat,imat),
				notrec(infeat,imat),
				notfeat(infeat,imat),
				thrshdnot(infeat,imat),
				notfrn(infeat,imat),
				thrshfnot(infeat,imat),  ihbcksl,
				notmat(infeat,imat)
	}
3031	format ('NOT ',a1,i6,2x,i3,2x,f7.4,'a',1x,f7.4,5x,a,
		'# NOT ID, rec, (mat=',i5,') feat, thrsh depth, thrsh fit')
3033	format ('NOT ',a1,i6,2x,i3,2x,f7.4,'r',i2,1x,f7.4,5x,a,
		'# NOT ID, rec, (mat=',i5,') feat, thrsh depth, thrsh fit')

	if (nftype(notfeat(infeat,imat),notmat(infeat,imat)) == 1) {
							# absorption band
		wavtmp = dataa(nchmin(notfeat(infeat,imat),notmat(infeat,imat)))
	} else {					# emission band
		wavtmp = dataa(nchmax(notfeat(infeat,imat),notmat(infeat,imat)))
	}
	write (lunhist, 3032) ihbcksl, wavtmp, otitle(notmat(infeat,imat))
3032	format (a,'#  NOT feature at wavelength',f7.3,' for:',a)


	go to 305  # read another line  (only exit when endfeatures found)

4096	irtn = 0
	return

69	irtn = 1   # error
	return

	end

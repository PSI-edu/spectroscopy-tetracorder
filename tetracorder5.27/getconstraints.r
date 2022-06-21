	subroutine getconstraints (imat, irtn)

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

	character*20 chtest
	character*110 chtmp1, chtmp2
	character cht1*3, cht2*5, cht3*2
	integer*4 imat   # material number

	real*4 x, xpressure, xtemperature

	irtn = 0

###########################################################
###########################################################
#       now check for fit, dpeth, fd thresholds

7	thrshfit(1,imat)     = 0.0  # thresholds
 	thrshfit(2,imat)     = 0.0  # thresholds
	thrshdepth(1,imat)   = 0.0
	thrshdepth(2,imat)   = 0.0
	thrshfd(1,imat)      = 0.0
	thrshfd(2,imat)      = 0.0
	thrshfdfit(1,imat)   = 0.0
	thrshfdfit(2,imat)   = 0.0
	thrshfddepth(1,imat) = 0.0
	thrshfddepth(2,imat) = 0.0
	thrshfitall(1,imat)  = 0.0
	thrshfitall(2,imat)  = 0.0
	thrshdepthall(1,imat)= 0.0
	thrshdepthall(2,imat)= 0.0
	thrshdepthfit(1,imat)= 0.0
	thrshdepthfit(2,imat)= 0.0
	thrshfdall(1,imat)   = 0.0
	thrshfdall(2,imat)   = 0.0

# set other constraints

	mtemp(1,imat)   = -999.   # internal units in centigrade
	mtemp(2,imat)   = -999.
	mtemp(3,imat)   = -999.
	mtemp(4,imat)   = -999.

        mpressure(1,imat)   = -999.   # units in bars
        mpressure(2,imat)   = -999.   # units in bars
        mpressure(3,imat)   = -999.   # units in bars
        mpressure(4,imat)   = -999.   # units in bars

	mclass(imat) = 1   # default class is 1st class
	dclass(imat) = 1.0 # class difference for action (1= no rejection)

	nfeatratio(imat) = 0          # zero feature ratios
	do itmpfr = 1, maxfeatratio {
		featidratio (1,itmpfr,imat) = 0
		featidratio (2,itmpfr,imat) = 0
		featratio(1,itmpfr,imat)    = 0.0
		featratio(2,itmpfr,imat)    = 0.0
		featratio(3,itmpfr,imat)    = 0.0
		featratio(4,itmpfr,imat)    = 0.0
	}

466	if (cmdverbose(-1) <= 1) write (ttyout,467) imat, otitle(imat)
	if (cmdverbose(-1) <= 1) write (ttyout,4671)
	if (cmdverbose(-1) <= 1) write (ttyout,468)
467	format(///,
	' Material',i3,': ',a,/,
	' to start constraints, enter: define constraints',/,
	' Options: enter:  Default = 1.0.',//,10x,
	'constraint: FIT>       fit   threshold, apply to fit   image',/,10x,
	'constraint: FITALL>    fit   threshold, apply to all   images',/,10x,
	'constraint: DEPTH>     depth threshold, apply to depth image',/,10x,
	'constraint: DEPTHALL>  depth threshold, apply to all   images',/,10x,
	'constraint: DEPTH-FIT> fit   threshold, apply to depth image',/,10x,
	'constraint: FD>        f*d   threshold, apply to f*d   image',/,10x,
	'constraint: FDALL>     f*d   threshold, apply to all   images',/,10x,
	'constraint: FD-FIT>    fit   threshold, apply to f*d   image',/,10x,
	'constraint: FD-DEPTH>  depth threshold, apply to f*d   image')
4671	format (/,10x,
	'constraint: fratio: a / b = r1 r2 r3 r4  where a, b = feature number',/,10x,
	'                            r1, r2=min; r3, r4=max with fuzzy logic')

468     format(/,10x,
	'constraint: temperature C t1 t2 t3 t4 ',/,10x,
	'constraint: temperature K t1 t2 t3 t4 ',/,10x,
	'constraint: pressure    Torr p1 p2 p3 p4 ',/,10x,
	'constraint: pressure    Bar  p1 p2 p3 p4 ',/,10x,
	'constraint: class       n  classdiff',//,8x,
	'each fit, depth, f*d is 2 values: min-max fuzzy logic levels',/,
		10x,' Example:   constraint: FIT> 0.2 0.4 DEPTH> 0.004 0.006',/,
		10x,' Example:   constraint: temperature C -10 0 100 110 ',/,
		10x,' Example:   constraint: class 2 0.02 ')

100	call crtin
	if (cmdverbose(-1) <= 2) write (ttyout,*) iopcon
	i = 1
	call wjfren (i,x,il)
	if (i >= 80 & il == 0) go to 466    # blank line, read again
	if (il==ihx )  {
		ic=il
		icrst = 1
		call rstart(icrst)
		call what (-1)
		return
	}
	ifirst= fnb(iopcon) # find first non blank
	if (ifirst < 1) ifirst = 1
	i = ifirst - 1
	ilast= lnb(iopcon)  # find last non blank
	if (ilast > 80) ilast = 80
	if (ilast < 1) ilast = 1
	#print *,"i :",i
	if (iopcon(ifirst:ifirst+17) == 'define constraints') {
		write (lunhist, *) 'define constraints'
		write (ttyout,*) 'define constraints found, enter constraints'

		go to 100  # define constraints line found, read next line
	}
	if (iopcon(i+1:i+11) == 'constraint:') {
		go to 3103   # constraint found, continue.

	} else if (iopcon(i+1:i+13) == 'endconstraint') {
		
		write (lunhist, *) 'endconstraint'
		write (ttyout,*) 'endconstraint found, exiting constraints'

		#go to 474   # next input line  when this code was in reflsetup
		irtn = 474   # next input line
		return
	} else {

		write (ttyout,*) 'ERROR: expecting constraint or ',
				'endconstraint key word'
		write (ttyout,*) 'input line:'
		write (ttyout,*) iopcon
		call what(i)
		go to 466
	}
	
3103	i = i + 1
	if (i >= 70) go to 3106  #beyond 70 has no room for parameters
				 # so write history for this line
	if (iopcon(i:i) == ' ') go to 3103

	###write (ttyout,*) 'DEBUG: i, iopcon:',i, iopcon(i:i+9)

	if (iopcon(i:i+3) == 'FIT>') {
		i = i + 4
		call wjfren (i,x,il)
		if (il != 0) {
			call what(i)
			write (ttyout,*) 'ERROR getting FIT> first number; non-numeric character found'
			go to 7
		} else {
			thrshfit(1,imat) = x
			###write (ttyout,*) 'DEBUG:thrshfit=',thrshfit(1,imat)
		}
		call wjfren (i,x,il)
		if (il != 0 && x == 0.0) {
			thrshfit(2,imat) = 0.0
			go to 7   # no second number, so ok
		}
		if (il != 0) {
			call what(i)
			write (ttyout,*) 'ERROR(?) getting 2nd number; non-numeric character found, skipping'
			go to 7
		} else {
			thrshfit(2,imat) = x
			###write (ttyout,*) 'DEBUG:thrshfit=',thrshfit(2,imat)
			go to 3103
		}
	}
	if (iopcon(i:i+6) == 'FITALL>') {
		i = i + 7
		call wjfren (i,x,il)
		if (il != 0) {
			call what(i)
			write (ttyout,*) 'ERROR getting FITALL> first number; non-numeric character found'
			go to 7
		} else {
			thrshfitall(1,imat) = x
			###write (ttyout,*) 'DEBUG:thrshfitall=',thrshfitall(1,imat)
		}
		call wjfren (i,x,il)
		if (il != 0 && x == 0.0) {
			thrshfitall(2,imat) = 0.0
			go to 7   # no second number, so ok
		}
		if (il != 0) {
			call what(i)
			write (ttyout,*) 'ERROR(?) getting 2nd number; non-numeric character found, skipping'
			go to 7
		} else {
			thrshfitall(2,imat) = x
			###write (ttyout,*) 'DEBUG:thrshfitall=',thrshfitall(2,imat)
			go to 3103
		}
	}
	if (iopcon(i:i+5) == 'DEPTH>') {
		i = i + 6
		call wjfren (i,x,il)
		if (il != 0) {
			call what(i)
			write (ttyout,*) 'ERROR getting DEPTH> first number; non-numeric character found'
			go to 7
		} else {
			thrshdepth(1,imat) = x
			###write (ttyout,*) 'DEBUG:thrshdepth=',thrshdepth(1,imat)
		}
		call wjfren (i,x,il)
		if (il != 0 && x == 0.0) {
			thrshdepth(2,imat) = x
			go to 7   # no second number, so ok
		}
		if (il != 0) {
			call what(i)
			write (ttyout,*) 'ERROR(?) getting 2nd number; non-numeric character found, skipping'
			go to 7
		} else {
			thrshdepth(2,imat) = x
			###write (ttyout,*) 'DEBUG:thrshdepth=',thrshdepth(2,imat)
			go to 3103
		}
	}
	if (iopcon(i:i+8) == 'DEPTHALL>') {
		i = i + 9
		call wjfren (i,x,il)
		if (il != 0) {
			call what(i)
			write (ttyout,*) 'ERROR getting DEPTHALL> first number; non-numeric character found'
			go to 7
		} else {
			thrshdepthall(1,imat) = x
			###write (ttyout,*) 'DEBUG:thrshdepthall=',thrshdepthall(1,imat)
		}
		call wjfren (i,x,il)
		if (il != 0 && x == 0.0) {
			thrshdepthall(2,imat) = x
			go to 7   # no second number, so ok
		}
		if (il != 0) {
			call what(i)
			write (ttyout,*) 'ERROR(?) getting 2nd number; non-numeric character found, skipping'
			go to 7
		} else {
			thrshdepthall(2,imat) = x
			###write (ttyout,*) 'DEBUG:thrshdepthall=',thrshdepthall(2,imat)
			go to 3103
		}
	}
	if (iopcon(i:i+9) == 'DEPTH-FIT>') {
		i = i + 10
		call wjfren (i,x,il)
		if (il != 0) {
			call what(i)
			write (ttyout,*) 'ERROR getting DEPTH-FIT> first number; non-numeric character found'
			go to 7
		} else {
			thrshdepthfit(1,imat) = x
			###write (ttyout,*) 'DEBUG:thrshdepthfit=',thrshdepthfit(1,imat)
		}
		call wjfren (i,x,il)
		if (il != 0 && x == 0.0) {
			thrshdepthfit(2,imat) = x
			go to 7   # no second number, so ok
		}
		if (il != 0) {
			call what(i)
			write (ttyout,*) 'ERROR(?) getting 2nd number; non-numeric character found, skipping'
			go to 7
		} else {
			thrshdepthfit(2,imat) = x
			###write (ttyout,*) 'DEBUG:thrshdepthfit=',thrshdepthfit(2,imat)
			go to 3103
		}
	}
	if (iopcon(i:i+2) == 'FD>') {
		i = i + 3
		call wjfren (i,x,il)
		if (il != 0) {
			call what(i)
			write (ttyout,*) 'ERROR getting FD> first number; non-numeric character found'
			go to 7
		} else {
			thrshfd(1,imat) = x
			###write (ttyout,*) 'DEBUG:thrshfd=',thrshfd(1,imat)
		}
		call wjfren (i,x,il)
		if (il != 0 && x == 0.0) {
			thrshfd(2,imat) = x
			go to 7   # no second number, so ok
		}
		if (il != 0) {
			call what(i)
			write (ttyout,*) 'ERROR(?) getting 2nd number; non-numeric character found, skipping'
			go to 7
		} else {
			thrshfd(2,imat) = x
			###write (ttyout,*) 'DEBUG:thrshfd=',thrshfd(2,imat)
			go to 3103
		}
	}
	if (iopcon(i:i+5) == 'FDALL>') {
		i = i + 6
		call wjfren (i,x,il)
		if (il != 0) {
			call what(i)
			write (ttyout,*) 'ERROR getting FDALL> first number; non-numeric character found'
			go to 7
		} else {
			thrshfdall(1,imat) = x
			###write (ttyout,*) 'DEBUG:thrshfdall=',thrshfdall(1,imat)
		}
		call wjfren (i,x,il)
		if (il != 0 && x == 0.0) {
			thrshfdall(2,imat) = x
			go to 7   # no second number, so ok
		}
		if (il != 0) {
			call what(i)
			write (ttyout,*) 'ERROR(?) getting 2nd number; non-numeric character found, skipping'
			go to 7
		} else {
			thrshfdall(2,imat) = x
			###write (ttyout,*) 'DEBUG:thrshfdall=',thrshfdall(2,imat)
			go to 3103
		}
	}
	if (iopcon(i:i+6) == 'FD-FIT>') {
		i = i + 7
		call wjfren (i,x,il)
		if (il != 0) {
			call what(i)
			write (ttyout,*) 'ERROR getting FD-FIT> first number; non-numeric character found'
			go to 7
		} else {
			thrshfdfit(1,imat) = x
			###write (ttyout,*) 'DEBUG:thrshfdfit=',thrshfdfit(1,imat)
		}
		call wjfren (i,x,il)
		if (il != 0 && x == 0.0) {
			thrshfdfit(2,imat) = x
			go to 7   # no second number, so ok
		}
		if (il != 0) {
			call what(i)
			write (ttyout,*) 'ERROR(?) getting 2nd number; non-numeric character found, skipping'
			go to 7
		} else {
			thrshfdfit(2,imat) = x
			###write (ttyout,*) 'DEBUG:thrshfdfit=',thrshfdfit(2,imat)
			go to 3103
		}
	}
	if (iopcon(i:i+8) == 'FD-DEPTH>') {
		i = i + 9
		call wjfren (i,x,il)
		if (il != 0) {
			call what(i)
			write (ttyout,*) 'ERROR getting FD-DEPTH> first number; non-numeric character found'
			go to 7
		} else {
			thrshfddepth(1,imat) = x
			###write (ttyout,*) 'DEBUG:thrshfddepth=',thrshfddepth(1,imat)
		}
		call wjfren (i,x,il)
		if (il != 0 && x == 0.0) {
			thrshfddepth(2,imat) = x
			go to 7   # no second number, so ok
		}
		if (il != 0) {
			call what(i)
			write (ttyout,*) 'ERROR(?) getting 2nd number; non-numeric character found, skipping'
			go to 7
		} else {
			thrshfddepth(2,imat) = x
			###write (ttyout,*) 'DEBUG:thrshfddepth=',thrshfddepth(2,imat)
			go to 3103
		}
	}

	#constraint: fratio: a / b = r1 r2 r3 r4  where a, b = feature number
	#                            r1, r2=min; r3, r4=max with fuzzy logic
	#       nfeatratio(imat)
	#	featidratio (1,itmpfr,imat) 
	#	featidratio (2,itmpfr,imat)
	#	featratio(4,itmpfr,imat)

	if (iopcon(i:i+6) == 'fratio:') {
		i = i + 7
		call wjfren (i,x,il)
		if (il != 0) {
			if (il != ihchar('/')) {
				call what(i)
				write (ttyout,*) 'ERROR in fratio expecting: /'
				go to 7
			}
		} else {
			nfeatratio(imat) = nfeatratio(imat) + 1
			nfr = nfeatratio(imat) 
			if (nfr > maxfeatratio) {
				write (ttyout,*) 'ERROR: too many ',
					'feature ratios:', nfr,
					'> max of ', maxfeatratio,
					'material ', imat
				call what(0)
				go to 7
			}
			featidratio(1,nfr,imat) = x
			###write (ttyout,*) 'DEBUG:fratio1=',x

			if (featidratio(1,nfr,imat) >  nfeat(imat) |
				featidratio(1,nfr,imat) < 1) {

				write (ttyout,*) 'ERROR: ',
					'feature ratio 1:', int(x),
					'> features ', nfeat(imat),
					'material ', imat,
					' (or < 0)'
				call what(0)
				go to 7
			}

		}
		call wjfren (i,x,il)
		if (il != 0) {
			if (il != ihchar('=')) {
				call what(i)
				write (ttyout,*) 'ERROR: fratio: extraneous character found',x
				go to 7
			}
		} else {
			featidratio(2,nfr,imat) = x
			###write (ttyout,*) 'DEBUG:fratio2=',x

			if (featidratio(2,nfr,imat) >  nfeat(imat) |
				featidratio(2,nfr,imat) < 1) {

				write (ttyout,*) 'ERROR: ',
					'feature ratio 1:', int(x),
					'> features ', nfeat(imat),
					'material ', imat,
					' (or < 0)'
				call what(0)
				go to 7
			}
		}
		call wjfren (i,x,il)
		if (il == ihchar('=')) {
			call wjfren (i,x,il)
		}
		if (il != 0) {
			call what(i)
				write (ttyout,*) 'ERROR: fratio: extraneous character found',x
			go to 7
		} else {
			featratio(1,nfr,imat) = x
			###write (ttyout,*) 'DEBUG:fratio 1=', x
		}

		call wjfren (i,x,il)
		if (il != 0) {
			call what(i)
				write (ttyout,*) 'ERROR: fratio: extraneous character found',x
			go to 7
		} else {
			featratio(2,nfr,imat) = x
			###write (ttyout,*) 'DEBUG:fratio 2=', x

			if (featratio(2,nfr,imat) <= featratio(1,nfr,imat)) {
				write(ttyout,*) 'ERROR: fratio parameter 2<=1'
				call what(0)
				go to 7
			}
		}
		call wjfren (i,x,il)
		if (il != 0) {
			call what(i)
			go to 7
		} else {
			featratio(3,nfr,imat) = x
			###write (ttyout,*) 'DEBUG:fratio 3=', x

			if (featratio(3,nfr,imat) <= featratio(2,nfr,imat)) {
				write(ttyout,*) 'ERROR: fratio parameter 3<=2'
				call what(0)
				go to 7
			}
		}
		call wjfren (i,x,il)
		if (il != 0) {
			call what(i)
			go to 7
		} else {
			featratio(4,nfr,imat) = x
			###write (ttyout,*) 'DEBUG:fratio 4=', x

			if (featratio(4,nfr,imat) <= featratio(3,nfr,imat)) {
				write(ttyout,*) 'ERROR: fratio parameter 4<=3'
				call what(0)
				go to 7
			}
			go to 3103
		}
	}

	if (iopcon(i:i+11) == 'temperature:' | iopcon(i:i+10) == 'temperature') {
		i = i + 12
		xtemperature=1.0
		call wjfren (i,x,il)
		if (il == ihck ) {
			xtemperature=0.0     # Kelvin
		} else if (il == ihcc ) {
			xtemperature=273.0   # subtract values by 272 to convert to Kelvin
		} else {
			write (ttyout,*) 'ERROR: C or K expected after temperature'
			call what(i)
			go to 7
		}	
		call wjfren (i,x,il)
		if (il != 0) {
			call what(i)
			write (ttyout,*) 'ERROR: getting temperature'
			go to 7
		} else {
			mtemp(1,imat) = x + xtemperature
			if (mtemp(1,imat) < 0.0) mtemp(1,imat) = 0.0
			###write (ttyout,*) 'DEBUG:temperature 1=',mtemp(1,imat)
		}
		call wjfren (i,x,il)
		if (il != 0) {
			call what(i)
			write (ttyout,*) 'ERROR: getting temperature'
			go to 7
		} else {
			mtemp(2,imat) = x + xtemperature
			if (mtemp(2,imat) < 0.0) mtemp(2,imat) = 0.0
			###write (ttyout,*) 'DEBUG:temperature 2=',mtemp(2,imat)
		}
		call wjfren (i,x,il)
		if (il != 0) {
			call what(i)
			write (ttyout,*) 'ERROR: getting temperature'
			go to 7
		} else {
			mtemp(3,imat) = x + xtemperature
			if (mtemp(3,imat) < 0.0) mtemp(3,imat) = 0.0
			###write (ttyout,*) 'DEBUG:temperature 3=',mtemp(3,imat)
		}
		call wjfren (i,x,il)
		if (il != 0) {
			call what(i)
			write (ttyout,*) 'ERROR: getting temperature'
			go to 7
		} else {
			mtemp(4,imat) = x + xtemperature
			if (mtemp(4,imat) < 0.0) mtemp(4,imat) = 0.0
			###write (ttyout,*) 'DEBUG:temperature 4=',mtemp(4,imat)
		}
		write (ttyout,*) 'DEBUG: temperatures=',mtemp(1,imat),mtemp(2,imat),mtemp(3,imat),mtemp(4,imat)
		go to 3103
	}

	if (iopcon(i:i+8) == 'pressure:' | iopcon(i:i+8) == 'pressure') {
		i = i + 9
		xpressure=1.0
		write (ttyout,*) 'DEBUG: pressure constraint found'
		call wjfren (i,x,il)
		if (iopcon(i-1:i+2) == 'Torr' | iopcon(i-1:i+2) == 'torr') {
			xpressure=760.0   # divide values by 760.0 for bars
		} else if (iopcon(i-1:i+2) == 'Bars' | iopcon(i-1:i+2) == 'bars' |
				iopcon(i-1:i+1) == 'bar' | iopcon(i-1:i+1) == 'Bar') {
			xpressure=1.0   # pressure in bars
		} else {
			call what(i)
			write (ttyout,*) 'ERROR: Torr or Bars, Bar, bars, bar expected after pressure'
			write (ttyout,*) 'DEBUG: iopcon(i-1:i+2)=', iopcon(i-1:i+2),' i=',i
			go to 7
		}	
		i=i+3
		call wjfren (i,x,il)
		if (il != 0) {
			call what(i)
			write (ttyout,*) 'ERROR: getting pressure'
			go to 7
		} else {
			mpressure(1,imat) = x / xpressure
			###write (ttyout,*) 'DEBUG:thrshfddepth=',mpressure(1,imat)
		}
		call wjfren (i,x,il)
		if (il != 0) {
			call what(i)
			write (ttyout,*) 'ERROR: getting pressure'
			go to 7
		} else {
			mpressure(2,imat) = x / xpressure
			###write (ttyout,*) 'DEBUG:thrshfddepth=',mpressure(2,imat)
		}
		call wjfren (i,x,il)
		if (il != 0) {
			call what(i)
			write (ttyout,*) 'ERROR: getting pressure'
			go to 7
		} else {
			mpressure(3,imat) = x / xpressure
			###write (ttyout,*) 'DEBUG:thrshfddepth=',mpressure(3,imat)
		}
		call wjfren (i,x,il)
		if (il != 0) {
			call what(i)
			write (ttyout,*) 'ERROR: getting pressure'
			go to 7
		} else {
			mpressure(4,imat) = x / xpressure
			###write (ttyout,*) 'DEBUG:thrshfddepth=',mpressure(4,imat)
		}
		write (ttyout,*) 'DEBUG: pressures=',mpressure(1,imat),mpressure(2,imat),mpressure(3,imat),mpressure(4,imat)
		go to 3103
	}

	if (iopcon(i:i+5) == 'class:') {
		i = i + 6
		call wjfren (i,x,il)
		if (il != 0) {
			call what(i)
			write (ttyout,*) 'ERROR: getting class'
			go to 7
		} else {
			mclass(imat) = int(x+0.5)
			###write (ttyout,*) 'DEBUG:thrshfddepth=',mclass(imat)
		}
		call wjfren (i,x,il)
		if (il != 0) {
			call what(i)
			write (ttyout,*) 'ERROR: getting class'
			go to 7
		} else {
			dclass(imat) = x
			###write (ttyout,*) 'DEBUG:thrshfddepth=',dclass(imat)
			go to 3103
		}
	}

        # add additional constraints here

#  constraint: bdratio: f1/f2 0.1 0.2 0.8 0.9 \# band depth ratio fuzzy logic min max

# alternate methods of specifying pressure:
# constraint: mode = [CONSTRAINTMODE]         \# earth mode applies (this should be a
#                                             \# a variable, set at execution time)
#                                             \# e.g. = Earth

# future
# constraint: pressure: Mars                  \# mars conditions
# constraint: pressure: Earth                 \# earth conditions
# constraint: elevation: earth m 0 1 1000 2000 \# m=meters, f=feet fuzzy logic
#                                              \# if in earth mode
# constraint: elevation: mars  m 0 1 1000 2000 \# m=meters, f=feet fuzzy logic
#                                              \# if in mars  mode
#              \# note: elevation may be more complex, e.g. where vegetation
#              \#       grows is a function of both elevation and latitude.
#              \#       perhaps need elevation * cosine latitude

	go to 3103

	
# write history
3106	write (lunhist, 3110) iopcon(ifirst:ilast), ihbcksl
	write (ttyout, 3110) iopcon(ifirst:ilast), ihbcksl
3110	format ('constraint: ',a,5x,a,'# options')

        go to 466   # loop back for more constraint input.

###########################################################

	end

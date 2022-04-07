#HPUX	program spfeatures(ic1,ic2,ic3,ic4,ic5,ic6,ic7,ic8,ic9,ic10,
#HPUX     		ic11,ic12,ic13,ic14,ic15,ic16,ic17,ic18,ic19,ic20)
#IA64HPUX	program spfeatures
	implicit integer*4 (i-n)
	include "spfeat.h"
	include "cmdarg.h"


	character*80 ic1,ic2,ic3,ic4,ic5,ic6,ic7,ic8,ic9,ic10
	character*80 ic11,ic12,ic13,ic14,ic15,ic16,ic17,ic18,ic19,ic20

#specific for HPUX:

#HPUX		charg(1) = ic1
#HPUX		charg(2) = ic2
#HPUX		charg(3) = ic3
#HPUX		charg(4) = ic4
#HPUX		charg(5) = ic5
#HPUX		charg(6) = ic6
#HPUX		charg(7) = ic7
#HPUX		charg(8) = ic8
#HPUX		charg(9) = ic9
#HPUX		charg(10) = ic10
#HPUX		charg(11) = ic11
#HPUX		charg(12) = ic12
#HPUX		charg(13) = ic13
#HPUX		charg(14) = ic14
#HPUX		charg(15) = ic15
#HPUX		charg(16) = ic16
#HPUX		charg(17) = ic17
#HPUX		charg(18) = ic18
#HPUX		charg(19) = ic19
#HPUX		charg(20) = ic20

#end specific for HPUX

	call getcmdargs


#
# Get options off command line, passed through spfeat 
#
	call getopt
	if (ferror==1) {
		print *, 'Fatal error. Please reenter options '
		stop  
	}
#
# make sure there's no confliciting options
#
	if (band==1 & oflag==1) {
		print *,'Cannot write binary-out from ASCII file, quitting'
		stop
	}
	
# 
# write header?
#
	if (head==1) {
# band header
		if (band==1) {
			write(ttyout,15)
			write(ttyout,16)
		} else {
# binary header verbose
			if (verb==1)  write(ttyout,17)
# binary header short
			else write(ttyout,18)
		}
	}
#
# make sure there's at least one option entered
#
	if (wflag==0 & eflag==0 & cflag==0 & dflag==0 & aflag==0) {
		print *,'no options entered, quitting '
		stop
	}
#
# call bopt for +band option or binary for -band option
#
	if (band==1) {
		call bopt
	} else {
		call binary
	}
	
15	format('Mineral',16x,'wave',5x,'error',3x,'fwhm',3x,'bndep',3x,'aymet',
           2x,'comments',9x,'record')
16	format(10x)
17	format('Title',38x,'center',2x,'width',1x,'depth',1x,
	'error',3x,'asym',1x,'cntn',3x,'it',1x,'frac',2x,'comment',14x,
	'file',4x,'#')
18	format ('Title',15x,'center',2x,'width',1x,'depth',1x,'error',2x,
	'asym',1x,'cntn',1x,'it',1x,'frac',3x,'file',1x,'#')
9000      end

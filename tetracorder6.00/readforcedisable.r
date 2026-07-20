	subroutine readforcedisable

	implicit integer*4 (i-n)

#ccc  name:         readforcedisable
#ccc  version date: 
#ccc  author(s): Roger N. Clark
#ccc  language: ratfor
#ccc
#ccc  short description: 
#ccc                      read the DISABLE/force-disable.txt file
#ccc                      and disable groups and cases 
#ccc
#ccc  algorithm description: 
#ccc  system requirements: Unix/Linux
#ccc  subroutines called: 
#ccc  argument list description: 
#ccc  parameter description: 
#ccc  common description: 
#ccc  message files referenced: 
#ccc  internal variables: 
#ccc  file description: 
#ccc  user command lines: 
#ccc  update information: 
#ccc  NOTES:
#ccc
#ccc

	include "../specpr/src.specpr/common/spmaxes"   # max parameters, must be first
	include "../specpr/src.specpr/common/lbl4"      # has iopcon array
        include "../specpr/src.specpr/common/lundefs"
	include "../specpr/src.specpr/common/alphabet"

	include "multmap.h"

	include "tricube.h"

# basic tetracorder parameters

	include "tri1.h"

	integer*4 imat   # material number
	integer*4 imode12  # mode for cases where features defined but get disabled (imod12=12)

	logical fexist

	character*1024  chlinein  # array for reading a line.

	character*8  a, b, d 

	integer*4 i, j, il, ix
	real*4    x

# notes:
# nfeat(maxmat)    # number features to analyze in each material
# nmats            # number of materials
# ngroups          # number of groups
# ncases           # number of cases
# nzgroup          # highest non-zero group
# nzcase           # highest non-zero case
# nmatgrp(0:maxgrp)        # number of materials in each group
# nmatcse(maxcse)          # number of materials in each case
# nftype(maxfeat,maxmat) # feature type (1=absorb, -1=emission)
# group0(maxmat)         # group 0 suppression enable = 1 (no 0)
#                           = 0 (do group 0 with other groups)
# group(maxmat)          # group number for each material
#                           group numbers < 0 are cases
# icase(maxmat)          # case number for each material
#                           case numbers <= 0 are groups
# imatenable(maxmat)          # enable this material = 1, 0= disable.
# ifeatenable(maxfeat,maxmat) # enable this material = 1, 0= disable.
# notfeatenable(maxnotfeat,maxmat) # enable this notfeat material = 1, 0= disable.
# groupenable(maxgrp)   # disable group = 0, enable >= 1
# caseenable(maxcse)    # disable case  = 0, enable >= 1

#  character*12 groupname(maxgrp)          # short names for groups
#  character*12 casename(maxcse)           # short names for cases

# integer*4 groupfdisable(maxgrp)  # force disable group (read from DISABLE/force-disable.txt)
#                                  #   final enable is groupenable * groupfdisable
#                                  #    group = 0, enable >= 1
#
# integer*4 casefdisable(maxcse)   # force disable case (read from DISABLE/force-disable.txt)
#                                  #   final enable is caseenable * casefdisable
#                                  # disable case  = 0, enable >= 1


#  
#	write (ttyout, *)  'setting all ', maxgrp, ' groups to 1 (enable) for readforcedisable'
#	do i = 1, maxgrp {
#
#		groupfdisable(i) = 1   # default: all groups enabled
#	}
#	write (ttyout, *)  'setting all ', maxcse, ' cases to 1 (enable) for readforcedisable'
#	do i = 1, maxcse {
#
#		casefdisable(i) = 1    # default: all cases enabled
#	}

	inquire (file="DISABLE/force-disable.txt", exist=fexist)

	if ( fexist ) {

		open(lunfdisable, file="DISABLE/force-disable.txt", iostat=ier, access='sequential', form='formatted')

		if (ier != 0) {

			write (ttyout, *) "open error ", ier, " on DISABLE/force-disable.txt"
			write (ttyout, *) "exit, ignoring DISABLE/force-disable.txt "
			write (ttyout, *) " "
			return  # return, nothing to do
		}

		write (ttyout, *) "DISABLE/force-disable.txt  found and opened"
		write (ttyout, *) " "
	} else {

		write (ttyout, *) "DISABLE/force-disable.txt   not found"
		write (ttyout, *) " "
		return
	}

	rewind (lunfdisable)

	do j = 1, 99999999  {     # for each line in DISABLE/force-disable.txt

		read(lunfdisable, 80, end=511, iostat=ier)  chlinein
80		format(a120)

		if ( ier > 0 ) {
			write (ttyout, *) "READ ERROR ", ier, " on DISABLE/force-disable.txt "
		}

		#write (ttyout, *)  chlinein(1:lnb(chlinein))
		write (ttyout, *) "DEBUG: force-disable: ", chlinein(1:75)

		iopcon(1:80) = chlinein(1:80)

		# check for DISABLE lines, skip all others

		i=1
		call wjfren(i,x,il)
		if (i < 60) {

			if (il == ihcd) {    # found D, want DISABLE

				write (ttyout,*) "DEBUG: found D, force-disable.txt  line=", j
				if (iopcon(i-1:i+5) == 'DISABLE') {

					write (ttyout,*) "           DEBUG: found word DISABLE. line=", j
					i = i+6
					call wjfren(i,x,il)
					if (il == ihg) {           # group disable

					    if (iopcon(i-1:i+1) == 'grp' ) {

						i = i +2
						call wjfren(i,x,il)
						ix = x
						if (ix <= maxgrp) {
							groupfdisable(ix) = 0  # disable case
							write (ttyout,*) "NOTE: FORCING DISABLE of group ", ix
							write (lunhist,*) "\# NOTE: FORCING DISABLE of group ", ix
						}
					    }
					} else if (il == ihc) {   # case disable

					    if (iopcon(i-1:i+1) == 'cse' ) {

						i = i +2
						call wjfren(i,x,il)
						ix = x
						if (ix <= maxcse) {
							casefdisable(ix) = 0  # disable case
							write (ttyout,*) "NOTE: FORCING DISABLE of case ", ix
							write (lunhist,*) "\# NOTE: FORCING DISABLE of case ", ix
						}
					    }
					}
				} else {

					next
				}
			} else {

				next
			}
		} else {

			next
		}
	}

511	return
	end


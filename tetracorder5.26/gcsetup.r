	subroutine gcsetup

######	implicit integer*4 (i-n)
	implicit none

#ccc  name:         gcsetup
#ccc  version date: 12/13/94
#ccc  author(s): Roger N. Clark
#ccc  language: ratfor
#ccc
#ccc  short description: build cross reference tables of which materials
#ccc                     are in what groups and cases.
#ccc
#ccc  algorithm description: 
#ccc  system requirements: Unix
#ccc  subroutines called: 
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
#ccc---------------------------------------------------------------------

	include "../specpr/src.specpr/common/spmaxes"   # max parameters, must be first

	include 	"../specpr/src.specpr/common/lundefs"

# arrays for multiple materials

	include "multmap.h"

	integer*4 ig,ic, imat
	integer*4 jmat


        ## nmats            # number of materials
        ## ngroups          # number of groups
        ## ncases           # number of cases
        ## matgrp(maxmat1,0:maxgrp) # cross reference list for groups
        ## matcse(maxmat1,maxcse)   # cross reference list for cases
        ## nmatgrp(0:maxgrp)        # number of materials in each group
        ## nmatcse(maxcse)          # number of materials in each case
	## nxgroup                  # highest non-zero group

	nzgroup = 0
	do ig = 0, maxgrp {    # check groups

		jmat = 0
		do imat = 1, nmats {

			if (group(imat) == ig) {  #this material is 
                                                  # in the group
				jmat = jmat + 1
				matgrp(jmat,ig) = imat

				if (imatenable(imat) > 0) {

					groupenable(ig) = groupenable(ig) + 1  # number materials enabled in group
				}

			}
		}
		if (ig > 0) write (ttyout,*) 'DEBUG: group:',ig,'  groupenable=', groupenable(ig)
		nmatgrp(ig) = jmat
		if(jmat != 0) nzgroup = ig  # highest non xero group

	}

	# now do cases

	nzcase = 0
	do ic = 1, maxcse {    # check cases

		jmat = 0
		do imat = 1, nmats {

			if (icase(imat) == ic) {  #this material is 
                                                  # in the case
				jmat = jmat + 1
				matcse(jmat,ic) = imat

				if (imatenable(imat) > 0) {
					caseenable(ic) = caseenable(ic) + 1  # number materials enabled in case
				}

			}
		}
		nmatcse(ic) = jmat
		if(jmat != 0) nzcase = ic  # highest non xero case

	}

	return
	end

	subroutine wrtgcsetup (iflag1)

######	implicit integer*4 (i-n)
	implicit none

#ccc  name:         wrtgcsetup
#ccc  version date: 12/13/94
#ccc  author(s): Roger N. Clark
#ccc  language: ratfor
#ccc
#ccc  short description: write cross reference tables of which materials
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
#ccc        iflag1 = 1 write lunresult
#ccc        iflag1 = 0 write ttyout
#ccc
#ccc---------------------------------------------------------------------

	include "../specpr/src.specpr/common/spmaxes"   # max parameters, must be first

	include 	"../specpr/src.specpr/common/label1"
	include 	"../specpr/src.specpr/common/lundefs"
	include 	"../specpr/src.specpr/common/alphabet"
	include 	"../specpr/src.specpr/common/blank"
	include 	"../specpr/src.specpr/common/dscrch"

# arrays for multiple materials

	include "multmap.h"

	include "tricube.h"

# basic tetracorder parameters

	include "tri1.h"

	integer*4 cmdverbose   # function cmdverbose


	integer*4 ii, j, k
	integer*4 iflag1

	integer*4 ignums(0:maxgrp)
	integer*4 icnums(maxcse)


        ## nmats            # number of materials
        ## ngroups          # number of groups
        ## ncases           # number of cases
        ## matgrp(maxmat1,0:maxgrp) # cross reference list for groups
        ## matcse(maxmat1,maxcse)   # cross reference list for cases
        ## nmatgrp(0:maxgrp)        # number of materials in each group
        ## nmatcse(maxcse)          # number of materials in each case
	## nzgroup                  # highest non-zero group
	## nzcase                   # highest non-zero case

	# do we need to set ngroups and ncases here?

	if (iflag1 == 0) {
		write (ttyout,20) nzgroup
		write (ttyout,21) nzcase
	}
	if (iflag1 == 1) {
		write (lunresult,20) nzgroup
		write (lunresult,21) nzcase
	}
20	format ('highest non-zero group:',i5)
21	format ('highest non-zero  case:',i5)

	if (nzgroup > 0) {
		do ii = 0, maxgrp {

			ignums(ii) = ii
		}
		if (iflag1 == 0) {

		    write (ttyout,9) ignums(0), nmatgrp(0)

		    if (maxgrp >= 10) {
			write (ttyout,10) (ignums(ii),ii=1,10),
				(nmatgrp(j),j=1,10),
				(incgrp0(k), k=1,10)
		    }
		    if (maxgrp >= 20) {
			write (ttyout,10) (ignums(ii),ii=11,20),
				(nmatgrp(j),j=11,20),
				(incgrp0(k), k=11, 20)
		    }
		    if (maxgrp >= 30) {
			write (ttyout,10) (ignums(ii),ii=21,30),
				(nmatgrp(j),j=21,30),
				(incgrp0(k), k=21, 30)
		    }
		    if (maxgrp >= 40) {
			write (ttyout,10) (ignums(ii),ii=31,40),
				(nmatgrp(j),j=31,40),
				(incgrp0(k), k=31, 40)
		    }
		    if (maxgrp >= 50) {
			write (ttyout,10) (ignums(ii),ii=41,50),
				(nmatgrp(j),j=41,50),
				(incgrp0(k), k=41, 50)
		    }
		
		}

		if (iflag1 == 1) {

		    write (lunresult,9) ignums(0), nmatgrp(0)

		    if (maxgrp >= 10) {
			write (lunresult,10) (ignums(ii),ii=1,10),
				(nmatgrp(j),j=1,10),
				(incgrp0(k), k=1,10)
		    }
		    if (maxgrp >= 20) {
			write (lunresult,10) (ignums(ii),ii=11,20),
				(nmatgrp(j),j=11,20),
				(incgrp0(k), k=11, 20)
		    }
		    if (maxgrp >= 30) {
			write (lunresult,10) (ignums(ii),ii=21,30),
				(nmatgrp(j),j=21,30),
				(incgrp0(k), k=21, 30)
		    }
		    if (maxgrp >= 40) {
			write (lunresult,10) (ignums(ii),ii=31,40),
				(nmatgrp(j),j=31,40),
				(incgrp0(k), k=31, 40)
		    }
		    if (maxgrp >= 50) {
			write (lunresult,10) (ignums(ii),ii=41,50),
				(nmatgrp(j),j=41,50),
				(incgrp0(k), k=41, 50)
		    }
		
		}

9 		format (/,'Number of materials in group:',/,
			'      group #:',i5,/,
			'   # in group:',i5,/)
10		format (/,'Number of materials in each group:',/,
			'       group #:',10(i5),/,
			'    # in group:',10(i5),/,
			' no grp 0 (=0):',10(i5),/)

	} else {
		write (ttyout,*) 'zero groups!'
	}

	# now do cases
	if (nzcase > 0) {
		do ii = 1, maxcse {

			icnums(ii) = ii
		}
		if (iflag1 == 0) {
		    if (maxcse >= 10) {
			write (ttyout,30) (icnums(ii),ii=1,10),
				     (nmatcse(j),j=1,10)
		    }
		    if (maxcse >= 20) {
			write (ttyout,30) (icnums(ii),ii=11,20),
				     (nmatcse(j),j=11,20)
		    }
		    if (maxcse >= 30) {
			write (ttyout,30) (icnums(ii),ii=21,30),
				     (nmatcse(j),j=21,30)
		    }
		}

		if (iflag1 == 1) {
		    if (maxcse >= 10) {
			write (lunresult,30) (icnums(ii),ii=1,10),
				     (nmatcse(j),j=1,10)
		    }
		    if (maxcse >= 20) {
			write (lunresult,30) (icnums(ii),ii=11,20),
				     (nmatcse(j),j=11,20)
		    }
		    if (maxcse >= 30) {
			write (lunresult,30) (icnums(ii),ii=21,30),
				     (nmatcse(j),j=21,30)
		    }
		}

30		format ('Number of materials in each case:',/,
			'       case #:',10(i5),/,
			'    # in case:',10(i5),/)

	}


	return
	end

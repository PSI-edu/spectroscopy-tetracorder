	subroutine enabldisablhist

	implicit integer*4 (i-n)

#ccc  name:         enabldisablhist
#ccc  version date: 
#ccc  author(s): Roger N. Clark
#ccc  language: ratfor
#ccc
#ccc  short description: 
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

	include "multmap.h"

	include "tricube.h"

# basic tetracorder parameters

	include "tri1.h"

	integer*4 imat   # material number
	integer*4 imode12  # mode for cases where features defined but get disabled (imod12=12)


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



	write (lunhist,1) 
1	format ("EDinfo: ########## Enable - Disable History #################")
	write (lunhist,2) 
2	format ("EDinfo:")

	# summary: groups and cases:  Phase 1

	do ig = 1, ngroups {

		if (groupenable(ig) > 0) {    # enabled

			write (lunhist,100) ig, groupname(ig), pathgrp(ig)
100			format ("EDinfo1:  group",i4," ",a,"  is ENABLED,                dir= ",a)

		} else {                       # disabled

			write (lunhist,101) ig, groupname(ig), pathgrp(ig)
101			format ("EDinfo1:  group",i4," ",a," ----------- is DISABLED,    dir= ",a)

		}
	}

	do ic = 1, ncases {

		if (caseenable(ic) > 0) {    # enabled

			write (lunhist,200) ic, casename(ic), pathcase(ic)
200			format ("EDinfo1:  case",i4," ",a,"  is ENABLED,                 dir= ",a)

		} else {                       # disabled

			write (lunhist,201) ic, casename(ic), pathcase(ic)
201			format ("EDinfo1:  case",i4," ",a," ----------- is DISABLED,     dir= ",a)

		}
	}

	write (lunhist,2) 

	# summary: groups and cases, Phase 2

	do ig = 0, maxgrp {    # check groups

		jmat = 0
		do imat = 1, nmats {

			if (group(imat) == ig) {  #this material is 
                                                  # in the group
				jmat = jmat + 1
				matgrp(jmat,ig) = imat

				if (imatenable(imat) > 0) {

					write (lunhist,300) ig, groupname(ig), imat, mfile(imat)
300					format ("EDinfo2:  group",i4," ",a," material",i4,"   is ENABLED:                  ", a)

				} else {

					write (lunhist,301) ig, groupname(ig), imat, matid(imat)
301					format ("EDinfo2:  group",i4," ",a," material",i4," ----------- is DISABLED:  ID= ", a)

				}

			}
		}

	}

	write (lunhist,2) 

	# summary: groups and cases and features, Phase 3

	do ig = 0, maxgrp {    # check groups

		jmat = 0
		do imat = 1, nmats {

			if (group(imat) == ig) {  #this material is 
                                                  # in the group
				jmat = jmat + 1
				matgrp(jmat,ig) = imat

				if (imatenable(imat) > 0) {

					write (lunhist,400) ig, groupname(ig), imat, mfile(imat)
400					format ("EDinfo3:  group",i4," ",a," material",i4,"   is ENABLED:                  ", a)

					do ifeat = 1, nfeat(imat) {

						if (ifeatenable(nfeat(imat),imat) > 0) {   # feature enabled

							write (lunhist,401) ig, imat, ifeat
401							format ("EDinfo3:      g",i4,"              material",i4," featuee",i3,"  is ENABLED")

						} else {                              # feature disabled
							write (lunhist,402) ig, imat, ifeat
402							format ("EDinfo3:      g",i4,"              material",i4," featuee",i3,"  ---------- is DISABLED")

						}
					}
					write (lunhist,900)

				} else {

					write (lunhist,411) ig, groupname(ig), imat, matid(imat)
411					format ("EDinfo3:  group",i4," ",a," material",i4," ----------- is DISABLED:  ID= ", a)

					write (lunhist,900)

				}

			}
		}
		write (lunhist,900)
900		format ("EDinfo3")

	}

	return
	end


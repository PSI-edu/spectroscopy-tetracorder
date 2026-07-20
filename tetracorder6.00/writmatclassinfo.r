	subroutine writmatclassinfo

	implicit integer*4(i-n)

#ccc  name:         readgeo
#ccc  version date:
#ccc  author(s): Roger N. Clark
#ccc  language: ratfor
#ccc
#ccc  short description: write material calss for all reference materials
#ccc
#ccc
#ccc  algorithm description:
#ccc  system requirements: Unix
#ccc  subroutines called: many specpr routines, need specpr.a library
#ccc  argument list description:
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced: none
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines: none
#ccc  update information:
#ccc  NOTES:


	include "../specpr/src.specpr/common/spmaxes"   # max parameters, must be first

	include 	"../specpr/src.specpr/common/alphabet"
	include         "../specpr/src.specpr/common/lundefs"
	include 	"../specpr/src.specpr/common/cmd"
	include 	"../specpr/src.specpr/common/blank"
	include		"../specpr/src.specpr/common/lblvol"

# basic tetracorder parameters

        include "multmap.h"

	include "tri1.h"

	integer*4 cmdverbose   # function cmdverbose

	logical*4       fexist     # file exists: true, false if doesn't
	integer*4       lnb        # function lnb

	character*44 geooclasf  # file name for  geologic-origins/material-classification.txt

	character*200 inputline

	integer*4 jjx(geochans)    # integer version of geologic origins for one material

	real*4 x
	integer*4 ier, i, ilin, il, imat
	integer*4 mce, ign, jlen, ikcomps

	character enabledisablestr*10   # enable or   DISABLE

	ihbcksl = char(92)  # this is the backslash character


	if (geoflag == 0) {

		# not doing geologic origins, so do nothing here
		return
	}

	geooclasf = 'geologic-origins/material-classification.txt'
                   #          11111111112222222222333333333344444
                   # 12345678901234567890123456789012345678901234



#  geologic-origins/material-classification.txt
#  \#       11111111112222222222333333333344444444445555555555666666666677777777778
#  \# 45678901234567890123456789012345678901234567890123456789012345678901234567890
#  \#                            |--|                    |--|                    |
#  acid_mine_drainage                mixture
#  actinolite
#  adularia                          tectosilicate           feldspar
#  albite                            tectosilicate           feldspar


#	write AAA.info/material-classifications.txt


	inquire (file='AAA.info/material-classifications.txt', exist=fexist)
	if (fexist) {
		write (ttyout,*) 'File AAA.info/material-classifications.txt is ALREADY MADE'
		write (ttyout,*) '     skipping generation of  AAA.info/material-classifications.txt'

	} else {
		open (unit=lunmatclass,
			file='AAA.info/material-classifications.txt',
			access='sequential', form='formatted',
			status='new', iostat=ier)

		if (ier != 0) {
			geoflag = 0
			write (ttyout,*) ' OPEN ERROR ON FILE: ', 
				'AAA.info/material-classifications.txt'
			write (ttyout,*) ' STOP'
			stop
		}

		do imat = 1, nmats {

		       if (imatenable(imat) == 0) {

		                enabledisablestr = "  DISABLE "
		        } else {

		                enabledisablestr = " enable   "
		        }

			ikcomps = nmatcomponent(imat)  # number of materials in reference spectrum imat
			
			igrpnum = group(imat)

			if (igrpnum > 0) {
				ign = lnb(pathgrp(igrpnum))    # length of groupname directory

			} else if (igrpnum == 0) {    # do this because group 0 directories are not defined as the
							# data are put in groups 1, 2, etc.
							# so use group 1 for where to fine group 0 results
				igrpnum = 1
				ign = lnb(pathgrp(igrpnum))    # length of groupname directory
			}

			do ilin = 1, ikcomps {

				# write:
				# material, classification, group, imat, enable/disable, reference spectrum outpout file name
				# reference spectrum outpout file name = mfile(imat)(1:mfilelen)  72 chars max

				# note: do not confuse the tetracorder group with mineral group
				# matgroupname is the mineral group.

				mce = matcomponidx(ilin,imat)  # index pointing to the material name

				jlen = lnb(mfile(imat))

				if ( igrpnum >= 0 ) {            # group, not case

				   if (imatenable(imat) <= 1) {  # do for enabled or disabled.  If decide
								# later to only do enabled, change <= to ==

					write (ttyout,      3001) matname(mce)(1:30),
						matclassname(mce)(1:20), matgroupname(mce)(1:20),
						group(imat), imat, ilin, enabledisablestr, mce, pathgrp(igrpnum)(1:ign), mfile(imat)(1:jlen)
	
					write (lunmatclass, 3001, iostat=ier) matname(mce)(1:30),
						matclassname(mce)(1:20), matgroupname(mce)(1:20),
						group(imat), imat, ilin, enabledisablestr, mce, pathgrp(igrpnum)(1:ign), mfile(imat)(1:jlen)
	
3001					format (a, 4x, a, 4x, a,' group= ',i3,' imat= ', i5,' c= ',i2,' ',a,' mce= ',i3,'  ',a,a)
				   } else {
					write (ttyout,      3002) matname(mce)(1:30),
						matclassname(mce)(1:20), matgroupname(mce)(1:20),
						group(imat), imat, ilin, enabledisablestr, mce
	
					write (lunmatclass, 3002, iostat=ier) matname(mce)(1:30),
						matclassname(mce)(1:20), matgroupname(mce)(1:20),
						group(imat), imat, ilin, enabledisablestr, mce
	
3002					format (a, 4x, a, 4x, a,' group= ',i3,' imat= ', i5,' c= ',i2,' ',a,' mce= ',i3)
				   }
				} else {    # case

				   if (imatenable(imat) == 1) {

					write (ttyout,      4001) matname(mce)(1:30),
						matclassname(mce)(1:20), matgroupname(mce)(1:20),
						group(imat), imat, ilin, enabledisablestr, mce, pathgrp(igrpnum)(1:ign), mfile(imat)(1:jlen)
	
					write (lunmatclass, 4001, iostat=ier) matname(mce)(1:30),
						matclassname(mce)(1:20), matgroupname(mce)(1:20),
						group(imat), imat, ilin, enabledisablestr, mce, pathgrp(igrpnum)(1:ign), mfile(imat)(1:jlen)
	
4001					format (a, 4x, a, 4x, a,'  case= ',i3,' imat= ', i5,' c= ',i2,' ',a,' mce= ',i3,'  ',a,a)
				   } else {
					write (ttyout,      4002) matname(mce)(1:30),
						matclassname(mce)(1:20), matgroupname(mce)(1:20),
						group(imat), imat, ilin, enabledisablestr, mce
	
					write (lunmatclass, 4002, iostat=ier) matname(mce)(1:30),
						matclassname(mce)(1:20), matgroupname(mce)(1:20),
						group(imat), imat, ilin, enabledisablestr, mce
	
4002					format (a, 4x, a, 4x, a,'  case= ',i3,' imat= ', i5,' c= ',i2,' ',a,' mce= ',i3)
				   }

				}
			}
		}
		do imat = 1, ncases {
		}
		close (lunmatclass)
	}

	return
	end


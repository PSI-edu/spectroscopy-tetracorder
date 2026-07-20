	subroutine readgeo

	implicit integer*4(i-n)

#ccc  name:         readgeo
#ccc  version date:
#ccc  author(s): Roger N. Clark
#ccc  language: ratfor
#ccc
#ccc  short description: read the geologic origins files 
#ccc             files to read:
#ccc                      geologic-origins/geological-origin-table-by-material.txt
#ccc                      geologic-origins/material-classification.txt
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

# basic tetracorder parameters

        include "multmap.h"

	include "tri1.h"

	integer*4 cmdverbose   # function cmdverbose

	logical*4       fexist     # file exists: true, false if doesn't

	character*80 geoorigf   # file name for  geologic-origins/geological-origin-table-by-material.txt
	character*80 geooclasf  # file name for  geologic-origins/material-classification.txt

	character*200 inputline

	integer*4 jjx(geochans)    # integer version of geologic origins for one material

	real*4 x
	integer*4 ier, i, ilin, il

	ihbcksl = char(92)  # this is the backslash character

#       geologic-origins/geological-origin-table-by-material.txt
#                11111111112222222222333333333344444444445555555
#       12345678901234567890123456789012345678901234567890123456
#       geologic-origins/material-classification.txt


	if (geoflag == 0) {

		# not doing geologic origins, so do nothing here
		return
	}

	geoorigf  = 'geologic-origins/geological-origin-table-by-material.txt     '
	geooclasf = 'geologic-origins/material-classification.txt                 '

	inquire (file=geoorigf(1:56), exist=fexist)

#   geologic-origins/geological-origin-table-by-material.txt
#   \# Column:
#   \#       11111111112222222222333333333344444444445555555555666666666677777777778
#   \# 45678901234567890123456789012345678901234567890123456789012345678901234567890
#   \#   GO1   Lacustrine/Marine     
#   \#   GO2   Evaporitic / playa   
#   \#   GO3   Metamorphic  
#   \#   GO4   Hydrothermal  
#   \#   GO5   Pedogenic /diagenic /weathering (include secondary coatings)  
#   \#   GO6   Igneous (include carbonatites) 
#   \#   GO7   Biogenic, Includes Vegetation
#   \#   G08   Organic, not necessarily biogenic
#   \#   G09   Man-Made
#   \#   G10   Water Liquid or Ice
#   \#   G11   Fire /Thermal Emission
#   \#
# modifications:  0 = not this origin
#                 1-9 scale = this origin with 9 being highest probability
#                     9 is scaled to 1.0 
#   \#
#   \# Material                   GO1 GO2 GO3 GO4 GO5  GO6  GO7  G08  G09  G10  G11
#   \#
#   acid_mine_drainage             0   0   0   9   0    0    0    0    0    0    0
#   adularia                       0   0   0   0   0    0    0    0    0    0    0
#   albite                         0   0   9   9   0    9    0    0    0    0    0
#   \#       11111111112222222222333333333344444444445555555555666666666677777777778
#   \# 45678901234567890123456789012345678901234567890123456789012345678901234567890


	if (fexist) {

		open (unit=lungeotable, file=geoorigf(1:56),
			access='sequential', form='formatted',
			status='old', iostat=ier)

		if (ier != 0) {

			geoflag = 0
			write (ttyout,187) ier, geoorigf(1:56)
187			format (' OPEN ERROR',i5,' on file: ',a)
			
			write (ttyout,*) ' '
			write (ttyout,*) '  STOP'
			stop
		} else {

			write (ttyout,*) ' OPENED file ', geoorigf(1:56)
			geoflag = 2
		}

		j = 1
		do ilin = 1, 9999 {

			read (lungeotable, 1, end=2000, iostat=ier) inputline
1			format (a)

			#write (ttyout,*) ' DEBUG1: ilin=',ilin,' ',inputline

			write (ttyout,*) inputline(1:110),'  geotable inputline'

			if (ier != 0) {
				geoflag = 0
				write (ttyout,*) ' READ ERROR ON FILE: ', geoorigf(1:56)
				write (ttyout,*) ' STOP'
				stop
			}

			if (inputline(1:2) != '\#' )  {   # not a comment line

				gmaterials(j) = inputline(1:30)   # material name for geologic origins

				iopcon(1:80) = inputline(31:110)

				i = 1
				
				# call wjfren 11 times to get the origin indices

				call wjfren(i, x, il)
				if (il == 0) {
					geoorigin(1,j) = x / 9.0
				} else {
					geoflag = 0
					write (ttyout,*) iopcon(1:80)
					call what(i)
					write (ttyout,*) ' ERROR decoding geologic origin'
					write (ttyout,*) ' o1  ilin=',ilin,' ',inputline
					write (ttyout,*) ' STOP'
					stop
				}
				call wjfren(i, x, il)
				if (il == 0) {
					geoorigin(2,j) = x / 9.0
				} else {
					geoflag = 0
					write (ttyout,*) ' o2  ilin=',ilin,' ',inputline
					call what(i)
					write (ttyout,*) ' ERROR decoding geologic origin'
					write (ttyout,*) ' o2  ilin=',ilin,' ',inputline
					write (ttyout,*) ' STOP'
					stop
				}
				call wjfren(i, x, il)
				if (il == 0) {
					geoorigin(3,j) = x / 9.0
				} else {
					geoflag = 0
					write (ttyout,*) ' o2  ilin=',ilin,' ',inputline
					call what(i)
					write (ttyout,*) ' ERROR decoding geologic origin'
					write (ttyout,*) ' o3  ilin=',ilin,' ',inputline
					write (ttyout,*) ' STOP'
					stop
				}
				call wjfren(i, x, il)
				if (il == 0) {
					geoorigin(4,j) = x / 9.0
				} else {
					geoflag = 0
					write (ttyout,*) ' o2  ilin=',ilin,' ',inputline
					call what(i)
					write (ttyout,*) ' ERROR decoding geologic origin'
					write (ttyout,*) ' o4  ilin=',ilin,' ',inputline
					write (ttyout,*) ' STOP'
					stop
				}
				call wjfren(i, x, il)
				if (il == 0) {
					geoorigin(5,j) = x / 9.0
				} else {
					geoflag = 0
					write (ttyout,*) ' o2  ilin=',ilin,' ',inputline
					call what(i)
					write (ttyout,*) ' ERROR decoding geologic origin'
					write (ttyout,*) ' o5  ilin=',ilin,' ',inputline
					write (ttyout,*) ' STOP'
					stop
				}
				call wjfren(i, x, il)
				if (il == 0) {
					geoorigin(6,j) = x / 9.0
				} else {
					geoflag = 0
					write (ttyout,*) ' o2  ilin=',ilin,' ',inputline
					call what(i)
					write (ttyout,*) ' ERROR decoding geologic origin'
					write (ttyout,*) ' o5  ilin=',ilin,' ',inputline
					write (ttyout,*) ' STOP'
					stop
				}
				call wjfren(i, x, il)
				if (il == 0) {
					geoorigin(7,j) = x / 9.0
				} else {
					geoflag = 0
					write (ttyout,*) ' o2  ilin=',ilin,' ',inputline
					call what(i)
					write (ttyout,*) ' ERROR decoding geologic origin'
					write (ttyout,*) ' o7  ilin=',ilin,' ',inputline
					write (ttyout,*) ' STOP'
					stop
				}
				call wjfren(i, x, il)
				if (il == 0) {
					geoorigin(8,j) = x / 9.0
				} else {
					geoflag = 0
					write (ttyout,*) ' o2  ilin=',ilin,' ',inputline
					call what(i)
					write (ttyout,*) ' ERROR decoding geologic origin'
					write (ttyout,*) ' o8  ilin=',ilin,' ',inputline
					write (ttyout,*) ' STOP'
					stop
				}
				call wjfren(i, x, il)
				if (il == 0) {
					geoorigin(9,j) = x / 9.0
				} else {
					geoflag = 0
					write (ttyout,*) ' o2  ilin=',ilin,' ',inputline
					call what(i)
					write (ttyout,*) ' ERROR decoding geologic origin'
					write (ttyout,*) ' o9  ilin=',ilin,' ',inputline
					write (ttyout,*) ' STOP'
					stop
				}
				call wjfren(i, x, il)
				if (il == 0) {
					geoorigin(10,j) = x / 9.0
				} else {
					geoflag = 0
					write (ttyout,*) ' o2  ilin=',ilin,' ',inputline
					call what(i)
					write (ttyout,*) ' ERROR decoding geologic origin'
					write (ttyout,*) ' o10 ilin=',ilin,' ',inputline
					write (ttyout,*) ' STOP'
					stop
				}
				call wjfren(i, x, il)
				if (il == 0) {
					geoorigin(11,j) = x / 9.0
				} else {
					geoflag = 0
					write (ttyout,*) ' o2  ilin=',ilin,' ',inputline
					call what(i)
					write (ttyout,*) ' ERROR decoding geologic origin'
					write (ttyout,*) ' o11 ilin=',ilin,' ',inputline
					write (ttyout,*) ' STOP'
					stop
				}

				j = j +1 

			}
		}

	} else {

		write (ttyout,188)  geoorigf(1:56)
188		format (' FILE DOES NOT EXIST: ', a)
		write (ttyout,*) ' '
		write (ttyout,*) '  STOP'
		stop
	}

2000	close(lungeotable)
	ngmaterials = j -1

	inquire (file='geologic-origins/geological-origin-table-by-material-verify.txt', exist=fexist)
	if (fexist) {
		write (ttyout,*) 'File geologic-origins/geological-origin-table-by-material-verify.txt already made'
		write (ttyout,*) '     skipping verify of geologic origins table'
		geoflag = 2

	} else {

		open (unit=lungeotable,
			file='geologic-origins/geological-origin-table-by-material-verify.txt',
			access='sequential', form='formatted',
			status='new', iostat=ier)
	
		if (ier != 0) {
			geoflag = 0
			write (ttyout,*) ' OPEN ERROR ON FILE: ',
				'geologic-origins/geological-origin-table-by-material-verify.txt'
			write (ttyout,*) ' STOP'
			stop
		} else {

			geoflag = 2
		}

		do ilin = 1, ngmaterials {
	
			do mm = 1, geochans {
	
			jjx(mm) =  int(geoorigin(mm, ilin) + 0.5)
			}
	
			if (ilin > 0) {
				write (ttyout,  7785) gmaterials(ilin)(1:30),
					geoorigin( 1,ilin),
					geoorigin( 2,ilin),
					geoorigin( 3,ilin),
					geoorigin( 4,ilin),
					geoorigin( 5,ilin),
					geoorigin( 6,ilin),
					geoorigin( 7,ilin),
					geoorigin( 8,ilin),
					geoorigin( 9,ilin),
					geoorigin(10,ilin),
					geoorigin(11,ilin), ilin
7785				format (a, f4.1,f4.1,f4.1,f4.1,f4.1,f4.1,f4.1,
					f4.1,f4.1,f4.1,f4.1,'     index=', i5,'   DEBUG23:')
			}
			write (ttyout,      2001)             gmaterials(ilin)(1:30), jjx, ilin
			write (lungeotable, 2001, iostat=ier) gmaterials(ilin)(1:30), jjx, ilin
2001			format (a, i3,i4,i4,i4,i4,i4,i4,i4,i4,i4,i4,'     index=', i5)
		}
		close(lungeotable)
	}

#  geologic-origins/material-classification.txt
#  \#       11111111112222222222333333333344444444445555555555666666666677777777778
#  \# 45678901234567890123456789012345678901234567890123456789012345678901234567890
#  \#                            |--|                    |--|                    |
#  acid_mine_drainage                mixture
#  actinolite
#  adularia                          tectosilicate           feldspar
#  albite                            tectosilicate           feldspar



	inquire (file=geooclasf(1:44), exist=fexist)

	if (fexist) {

		open (unit=lunmatclass, file=geooclasf(1:44),
			access='sequential', form='formatted',
			status='old', iostat=ier)

		if (ier != 0) {
			geoflag = 0
			write (ttyout,187) ier, geooclasf(1:44)
			
			write (ttyout,*) ' '
			write (ttyout,*) '  STOP'
			stop
		} else {
			geoflag = 2
		}

		j = 1
		do ilin = 1, 9999 {

			read (lunmatclass, 1, end=3000, iostat=ier) inputline

			if (ier != 0) {
				geoflag = 0
				write (ttyout,*) ' READ ERROR ON FILE: ', geooclasf(1:44)
				write (ttyout,*) ' STOP'
				stop
			}

			if (inputline(1:2) != '\#' )  {   # not a comment line

				matname(j)(1:30)      = inputline(1:30)
				matclassname(j)(1:20) = inputline(35:54)
				matgroupname(j)(1:20) = inputline(59:78)

				j = j +1 
			}

		}


	} else {

		write (ttyout,188)  geooclasf(1:44)
		write (ttyout,*) ' '
		write (ttyout,*) '  STOP'
		stop
	}

3000	nmatnames = j - 1
	close (lunmatclass)


	inquire (file='geologic-origins/material-classification-verify.txt', exist=fexist)
	if (fexist) {
		write (ttyout,*) 'File geologic-origins/material-classification-verify.txt is ALREADY MADE'
		write (ttyout,*) '     skipping verify of geologic materials table'
		geoflag = 2

	} else {
		open (unit=lunmatclass,
			file='geologic-origins/material-classification-verify.txt',
			access='sequential', form='formatted',
			status='new', iostat=ier)

		if (ier != 0) {
			geoflag = 0
			write (ttyout,*) ' OPEN ERROR ON FILE: ', 
				'geologic-origins/material-classification-verify.txt'
			write (ttyout,*) ' STOP'
			stop
		}

		do ilin = 1, nmatnames {
	
			write (ttyout,      3001, iostat=ier) matname(ilin)(1:30),
				matclassname(ilin)(1:20), matgroupname(ilin)(1:20)
	
			write (lunmatclass, 3001, iostat=ier) matname(ilin)(1:30),
				matclassname(ilin)(1:20), matgroupname(ilin)(1:20),
				ilin
	
3001			format (a, 4x, a, 4x, a,'    index= ', i5)
		}
		close (lunmatclass)
	}

	return
	end


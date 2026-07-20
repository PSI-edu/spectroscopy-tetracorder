	subroutine opengeocub (ierr) 

	implicit integer*4 (i-n)

#ccc  name:  opengeocub
#ccc  version date: 2/17/2026
#ccc  author(s): Roger N. Clark
#ccc  language: ratfor
#ccc
#ccc  short description: open new file geologic origins cube
#ccc
#ccc  algorithm description: 
#ccc  system requirements: none
#ccc  subroutines called: 
#ccc  argument list description:
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced: none
#ccc  internal variables:
#ccc  file description: none
#ccc  user command lines: none
#ccc  update information:
#ccc  NOTES:
#ccc

	include "../specpr/src.specpr/common/spmaxes"   # max parameters, must be first

	include '../specpr/src.specpr/common/lundefs'

	include "../specpr/src.specpr/common/lblvol"

	include "multmap.h"
	include "tricube.h"
	include "geocube1.h"
	include "tri1.h"

# arrays for buffering output

	include "obuffers.h"

	character*1 imch(5)
	character*80 icube   # cube file name
	character*80 chtmp
	character*200 chtmp2

	integer*4 cmdverbose   # function cmdverbose
	integer*4 ierr, ixdx

#
#     keywords defined
#
#        interleave   = : (ENVI)
#               'bil' (filorg=1)
#               'bip' (filorg=2)
#               'bsq' (filorg=3)
#               default 'BNK'

#        data_type    = : (ENVI)
#               [NOT Implemented] 1 = Byte
#                               8bit unsigned integer (0 to 255)
#               2 (numtyp=1) [or dattyp or filtype(9,x)]; Integer (Int*2)
#                               16bit signed integer  (-32768 to +32767)
#               3 (numtyp=2) [or dattyp or filtype(9,x)]; Long Integer  (Int*4)
#                               32bit signed integer (~ +/- 2 billion)
#               4 (numtyp=3) [or dattyp or filtype(9,x)]; Floating-point  (Real*4)
#                               32bit real (+/- 1e38)
#               default -1

#        byte_order   = : (ENVI)
#               0 = Intel (PC)  [LOWHI]
#               1 = IEEE                [HILOW]
#               default -1

#        data_package = : (tetracorder definition)
#               'ENVI'
#               'VICAR'
#               default 'BLANK'

        integer*4       lnb, fnb   # function lnb, fnb

        logical*4       fexist     # file exists: true, false if doesn't
        logical*4       iopened    # file already opened: true, false if doesn't


	geofilname = "geologic-origins/geologic_origins_cube.v"
#                             1111111111222222222233333333334
#                    1234567890123456789012345678901234567890

	ixdx = dx *2   # 2 times x pixels (16-bit images)

	ierr=0
	open (unit=lungeocube, file=geofilname, access='direct',
		recl=ixdx,
		form='unformatted', status='new', iostat=ierr)

        if (ierr != 0) {
		geoflag = 0
                write (ttyout,*)'ERROR on open of geologic origins cube: ',ierr
                write (ttyout,*)'unit= ',lungeocube
		write (ttyout,*) '       stopping geologic origins calculations'
                call what(-1)
                return
        } else {

		if ( geoflag >= 2 ) {
			write (ttyout,*) 'geologic origins cube opened: ', geofilname
			geoflag =3
		} else {

			write (ttyout,*) 'ERROR: geoflag= ', geoflag
			write (ttyout,*) '       but it should be 2 at this point'
			write (ttyout,*) '       that means the geologic origina files have not been read correctly'
			write (ttyout,*) '       stopping geologic origins calculations'
			geoflag = 0
		}
	}

	# now make vicar header

	# LBLSIZE=614             FORMAT='BYTE'  TYPE='IMAGE'  BUFSIZ=20262   DIM=2  EOL=0
        #  RECSIZE=614  ORG='BSQ'  NL=972  NS=614  NB=1  N1=0  N2=0  N3=0  N4=0
        # NBB=0  NLB=0  TASK='tetracorder'  USER='rclark'  DAT_TIM='Feb  4 21:47:59 2026 UT'  TITLE='Zoisite HS347.3B '
 
	do i = 1, ixdx {    # ixdx should never overflow the chdata size as checks are done earlier
				# once the header is writte, chdata can be used for other things

		chdata(i:i) = " "
	}
	write (ttyout, *) ' '
	write (ttyout, *) ' '
	write (ttyout, *) 'Geologic origins cube: Generating vicar header: ', ixdx,' max characters'

	igdy = geochans

	#write (ttyout,  100) ixdx, ixdx, dx, igdy, geochans

	write (chdata, 100) ixdx, ixdx, dy, dx, geochans
100	format ("LBLSIZE=",i6,"         FORMAT='HALF'   TYPE='IMAGE'  BUFSIZ=20262   DIM=3 EOL=0   ",
		"RECSIZE=",i6,"  ORG='BIL'   NL=",i6,"   NS=",i6,"   NB=",i2,"   N1=0  N2=0  N3=0  N4=0",
		"   NBB=0  NLB=0  TASK='tetracorder'    TITLE='Geologic Origins Cube'         ")

	ighdr = lnb(chdata)

	write (ttyout,*) 'Geologic origins cube:  vicar header size = ', ighdr,' characters'
	write (ttyout,101) chdata(1:ighdr)
101	format (a)

	write (lungeocube, rec=1, iostat=ierr) chdata(1:ixdx)

	if (ierr != 0) {   # Check for error in writing

		geoflag = 0
		write (ttyout,*) "ERROR writing vicar header to geoloigic origins cube"
		write (ttyout,*) "ERROR= ", ierr,'  logical unit=', lungeocube
		write (ttyout,*) '       stopping geologic origins calculations'
		call what (-1)
		return
	}
	return
	end

	subroutine opencube (qlun, icube, filhdr, reclen, rechdr,
			dx, dy, dz, numtyp, filorg, ier)

	implicit integer*4 (i-n)

#ccc  name:  opencube
#ccc  version date: 4/24/90
#ccc  author(s): Roger N. Clark
#ccc  language: ratfor
#ccc
#ccc  short description: open a cube check header and parse it.
#ccc                     at present only recognizes subsets of
#ccc                     vicar and ENVI image-cube formats
#ccc
#ccc  algorithm description: search strings label for
#ccc                         various fields to recognize correct
#ccc				format, then call parsing routine.
#ccc  system requirements: none
#ccc  subroutines called:       wjfren
#ccc  argument list description:
#ccc     INPUT:
#ccc        qlun:   3D logical unit number
#ccc        icube:  cube file name
#ccc     OUTPUT:
#ccc        filhdr: file header length in recorde
#ccc        reclen: record length in bytes
#ccc        rechdr: record header length in bytes
#ccc
#ccc        dx: x dimension in pixels = number of samples
#ccc        dy: y dimension in pixels = number of lines
#ccc        dz: z dimension in pixels = number of bands
#ccc
#ccc        numtyp: Data type 1= int*2
#ccc        filorg: file organization (1 = BIL)
#ccc
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

	integer*4 qlun, reclen, rechdr, idnoff, filhdr
	integer*4 dx, dy, dz, numtyp, filorg, ier

	character*1536 ch
	character*(*) icube

	character*120 ivtitl,avlab1
	integer*4 lblsiz,form,recsiz
	integer*4 ivdate,ivtime,ivdat2,ivtim2

#
#     keywords defined
#
#        interleave   = : (ENVI)
#		'bil' (filorg=1)
#		'bip' (filorg=2)
#		'bsq' (filorg=3)
#		default 'BNK'

#        data_type    = : (ENVI)
#		[NOT Implemented] 1 = Byte
#				8bit unsigned integer (0 to 255)
#		2 (numtyp=1) [or dattyp or filtype(9,x)]; Integer (Int*2)
#				16bit signed integer  (-32768 to +32767)
#		3 (numtyp=2) [or dattyp or filtype(9,x)]; Long Integer  (Int*4)
#				32bit signed integer (~ +/- 2 billion)
#		4 (numtyp=3) [or dattyp or filtype(9,x)]; Floating-point  (Real*4)
#				32bit real (+/- 1e38)
#		default -1

#        byte_order   = : (ENVI)
#		0 = Intel (PC)	[LOWHI]
#		1 = IEEE		[HILOW]
#		default -1

#        data_package = : (tetracorder definition)
#		'ENVI'
#		'VICAR'
#		default 'BLANK'


	character*3 interleave
	integer*4 data_type, byte_order
	character*40 data_package

	character*1 ichil,iquote,lowch,highch,cnull
	character*3 cm

	integer*4 lnb, fnb
	logical*4	fexist     # file exists: true, false if doesn't
	logical*4	iopened    # file already opened: true, false if doesn't

#	integer*4 ttyout
#
###### TEMPORARY:
#	ttyout = 6
#
# initialize variables
#
	iquote = char(39)  # a single quote character
	interleave = 'UNK'
	data_type = -1
	byte_order = -1
	data_package = 'BLANK'

	rechdr = 0
	numtyp = 0
	reclen = 0
	filhdr = 0
	filorg = 0

# open the cube

	itmp1 = fnb(icube)
	itmp2 = lnb(icube)
	if (itmp1 == 0 | itmp2 == 0 | itmp2 < itmp1) {
		write (ttyout,*) icube
		call what(itmp1)
		call what(itmp2)
		return
	}

# DEBUG:	write (ttyout,*)'Opening:',icube(itmp1:itmp2)

	inquire (file=icube(itmp1:itmp2),exist=fexist,
		opened=iopened,number=inum,iostat=ier)

	if (ier != 0) {
		write (ttyout,*) 'INQUIRE ERROR ',ier
		write (ttyout,*) 'FILE:',icube(itmp1:itmp2)
		call what (-1)
		return
	}

	if (!fexist) {
		write (ttyout,187) icube(itmp1:itmp2)
187		format (' ERROR: file:',a,/,
			' FILE DOES NOT EXIST')
		call what (-1)
		return
	}

	if (iopened) {
		write (ttyout,*) 'FILE:',icube(itmp1:itmp2)
		write (ttyout,*) 'ALREADY OPENED to unit',inum
		if (inum < 20) {
			write (ttyout,*) 'IT IS PROBABLY ASSIGNED IN SPECPR'
		}
		call what (-1)
		return
	}





#
# check for (and read) ENVI header
# NO ERROR CHECKING YET  (.hdr or .HDR)
#

	open (unit=qlun, file=icube(itmp1:itmp2)//'.hdr',
		access='sequential',
                form='formatted', status='old',
		iostat=ier)
	if (ier != 0) {
	#	try VICAR header parsing
		write (ttyout,*)'ERROR on opening ENVI header file', ier
		write (ttyout,*)'Note: filename should be the cube, not the .hdr file'
		close(qlun)
		goto 200
	}

#
# determine if ENVI header is recognizable
#
	read (qlun,'(a)',iostat=ier) ch

	if (ier != 0) {
	#	header not found, so try VICAR header parsing
		write (ttyout,*)'ERROR on ENVI header read', ier
		write (ttyout,*)'      will try vicar header'
		close(qlun)
		goto 200
	}

	if (ch(1:4) == 'ENVI') {
		call parseenvilabel (qlun,icube,dx,dy,dz,lblsiz,
			data_type,interleave,byte_order,ier)


		# NOTES: lblsiz here is  header_offset  in parseenvilabel
		#  filorg: BIL=1, BIP=2, BSQ=3

		if(interleave == 'bil') {
			write (ttyout,*) '  ENVI header interleave = bil'
			filorg = 1
		} else if (interleave == 'bip') {
			write (ttyout,*) '  ENVI header interleave = bip'
			filorg = 2
		} else if (interleave == 'bsq') {
			write (ttyout,*) '  ENVI header interleave = bsq'
			filorg = 3
		} else {
			numtyp = 0 # temporary
			write (ttyout,*) 'only bil, bip, & bsq formats detectable ',
				'at present'
			write (ttyout,*) 'found interleave= ', interleave
			ier = 0
			call what(-1)
			return
		}

		if ( filorg == 1 ) {         # BIL
			if (data_type == 2) {    # 16-bit signed int values
				numtyp = 1
				reclen = dx * 2
			} else if (data_type == 3) {    # 32-bit signed int values (Long Integer)
				numtyp = 2
				reclen = dx * 4
			} else if (data_type == 4) {    # 32-bit Floating point values
				numtyp = 3
				reclen = dx * 4
			} else {
				numtyp = 0 # temporary
				write (ttyout,*) 'only Int*2, Int*4, & Real*4 formats ',
					'detectable at present'
				ier = 0
				call what(-1)
				return
			}
		} else if ( filorg == 2 ) {   # BIP

			if (data_type == 2) {    # 16-bit signed int values
				numtyp = 1
				reclen = dz * 2
			} else if (data_type == 3) {    # 32-bit signed int values (Long Integer)
				numtyp = 2
				reclen = dz * 4
			} else if (data_type == 4) {    # 32-bit Floating point values
				numtyp = 3
				reclen = dz * 4
			} else {
				numtyp = 0 # temporary
				write (ttyout,*) 'only Int*2, Int*4, & Real*4 formats ',
					'detectable at present'
				ier = 0
				call what(-1)
				return
			}
		}


 		# there are multiple different headers possible
		#
		# envi binary data file with no header and separate envi header file
		#      then filhdr =0
		#
		# vicar file with envi header, then in the envi header,
		#        header_offset = the vicr label size in bytes

		# The following was erroneously commented out 10/29/2020 to 03/09/2022:
		#     thinking any file with envi header would have filhdr = 0
	
		filhdr = int(float(lblsiz)/float(reclen)+0.9999)

		if (filhdr*reclen != lblsiz) {
			write (ttyout,*) 'ERROR: filhdr*reclen=',
				filhdr*reclen,' is not =',lblsiz,
				'filhdr=',filhdr,'reclen=',reclen
			write (ttyout,*) 'header size must be a',
				' multiple of the record size'
			write (ttyout,*) 'because this is fortran I/O'
			ier =1
			call what(-1)
			return
		}

		data_package = 'ENVI'
	}
###   end ENVI parsing

#	write(*,*) 'end of ENVI: ./tetracorder4.2/opencube.r'
#	write(*,*) 'qlun,icube,samples,lines,bands,',
#		'header_offset,data_type,interleave,byte_order,ier'
#	write(*,*) qlun,icube,dx,dy,dz,lblsiz,
#		data_type,interleave,byte_order,ier
#	write(*,*) 'filorg, numtyp, reclen, filhdr, data_package = '
#	write(*,*) filorg, numtyp, reclen, filhdr, data_package
#	stop

#
# ENVI header read performed, skip VICAR header parsing
#

	goto 900

#
# test for VICAR header and call parse routine
#
200	continue

	open (unit=qlun, file=icube(itmp1:itmp2),
		access='direct',
                recl=1536,
                form='unformatted', status='old',
		iostat=ier)
	if (ier != 0) {
		write (ttyout,*)'ERROR on initial open in opencube: ',ier
		write (ttyout,*)'unit=',qlun
		call what(-1)
		return
	}

#
# determine if VICAR header is recognizable
#
	read (qlun,rec=1,iostat=ier) ch
	if (ier != 0) {
		write (ttyout,*)'ERROR on read in opencube: ',ier
		call what(-1)
		return
	}
	if (ch(1:8) == 'LBLSIZE=') {  # found basic vicar label
		call parsevicarlabel (ch,lblsiz,form,recsiz,
			filorg,dy,dx,dz,ivdate,ivtime,ivdat2,
			ivtim2,ivtitl,avlab1,ier)
		rechdr = 0

		# filorg = 1: BIL
		# filorg = 2: BIP
		# filorg = 3: BSQ

		if ( filorg == 1 ) {
			if (form == 16) {    # 16-bit int values
				numtyp = 1
				reclen = dx * 2
			} else if (form == 32) {    # 32-bit integer values
				numtyp = 2
				reclen = dx * 4
			} else if (form == 1) {    # 32-bit floating point values
				numtyp = 3
				reclen = dx * 4
			} else {
				write (ttyout,*) 'can only do 16 or 32-bit Integer or 32-bit floating point VICAR files ',
					'at present'
				write (ttyout,*) 'data format, form=', form
				numtyp = 0 # temporary
				ier = 0
				call what(-1)
				return
			}
		} else if ( filorg == 2 ) {   # BIP

			if (form == 16) {    # 16-bit int values
				numtyp = 1
				reclen = dz * 2
			} else if (form == 32) {    # 32-bit integer values
				numtyp = 2
				reclen = dz * 4
			} else if (form == 1) {    # 32-bit floating point values
				numtyp = 3
				reclen = dz * 4
			} else {
				write (ttyout,*) 'can only do 16 or 32-bit Integer or 32-bit floating point VICAR files ',
					'at present'
				write (ttyout,*) 'data format, form=', form
				numtyp = 0 # temporary
				ier = 0
				call what(-1)
				return
			}
		} else {

			write (ttyout,*) '  FILE Organization is not BIL or BIP'
			write (ttyout,*) '      EXITING'
				numtyp = 0 # temporary
				ier = 0
				call what(-1)
				return
		}

		## was ## filhdr = lblsiz / reclen
                ## now:

		filhdr = int(float(lblsiz)/float(reclen)+0.9999)

		if (filhdr*reclen != lblsiz) {
			write (ttyout,*) 'ERROR: filhdr*reclen=',
				filhdr*reclen,' is not =',lblsiz,
				'filhdr=',filhdr,'reclen=',reclen
			write (ttyout,*) 'header size must be a',
				' multiple of the record size'
			write (ttyout,*) 'because this is fortran I/O'
			ier =1
			call what(-1)
			return
		}
		data_package = 'VICAR'
	}
# end of VICAR parsing

#
# end of ENVI/VICAR header parsing routines
# now open data file for binary data reads
#

900	close (qlun, iostat=ier)
	if (ier != 0) {
		write (ttyout,*)'ERROR on close ',
					'in opencube: ',ier
		call what(-1)
		return
	}

	if(data_package == 'BLANK') {

		write (ttyout,*) 'unrecognized file header'
		call what(-1)
		ier = 0
		return
	}


	open (unit=qlun, file=icube,
		access='direct',
		recl=reclen,
		form='unformatted', status='old',
		iostat=ier)
	if (ier != 0) {
		write (ttyout,*)'ERROR on second',
				' open in opencube: ',ier
		call what(-1)
		return
	}

	return
	end

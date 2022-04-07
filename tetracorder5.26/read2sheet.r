	subroutine read2sheet(qlun,reclen,nrecshdr,
				dx,dy,dz,idnoff,scale,iptdrop,
				chfirst,chlast,iline,filorg,numtyp,
				specdat,ioerr)

	implicit integer*4 (i-n)

#ccc  name: read2sheet
#ccc  version date: April 2010
#ccc  author(s):  Roger N. Clark & Eric Livo
#ccc  language:  Ratfor
#ccc
#ccc  short description: read one cross track line of I*2 BIL or BIP
#ccc                     format imaging spectrometer data set
#ccc
#ccc  algorithm description:
#ccc  system requirements:
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

	include "../specpr/src.specpr/common/spmaxes"   # max parameters, must be first

      include "multmap.h"
#      include "tricube.h"
      include "../specpr/src.specpr/common/lundefs"

	integer*4 qlun          # logical unit
	integer*4 reclen        # record length in bytes
	integer*4 nrecshdr      # number of header records
	integer*4 dx            # number samples (cross track)
	integer*4 dy            # number of lines (down track)
	integer*4 dz            # number of spectral bands
	integer*4 idnoff        # dn offset
	real*4    scale         # dn scale
	real*4    fidnoff       # floating point version of idnoff
	integer*4 iptdrop       # deleted point value in dn

	integer*4 chfirst       # first dz channel needed
	integer*4 chlast        # last dz channel needed
	integer*4 iline         # line number to read in file
	integer*4 filorg        # data-cube channel organization (bil,bip,bsq)
	integer*4 numtyp        # data type (Int*2, Int*4, Real*4)
	integer*4 ioerr         # I/O error flag (=0 no errors)
	integer*4 itmp          # first recoed number of the sheet to be read (includes header offset)

	integer*4 isetcell_first   #
	integer*4 isetcell_last    #


#variables for data-cube reads
# image data-cube sizes set in multimap.h, tricube.h, and here (read2sheet)

# within multmap.h: 
#      parameter (imaxch=   )       # maximum channels in spectrum
#      parameter (maxpix=    )      # maximum pixels per line

#

      ### NOTE: if maxpi4 in multmap.h is increased above 131060, below needs to be
      ###       increased to match

      character*131060  charline      # buffer to read all data (maxpix*4)

      integer*2 i2line(maxpi2)        # buffer to read real*4 data (maxpix*2)
      integer*4 i4line(maxpix)        # buffer to read real*4 data (maxpix)
      real*4    r4line(maxpix)        # buffer to read real*4 data (maxpix)

      real*4 specdat(imaxch,maxpix)   # extracted spectra in BIP format (imaxch,maxpix)
      #real*4 specdat(4852,1)   # extracted spectra in BIP format

      integer*4 isetrec

      equivalence (i2line, charline)
      equivalence (i4line, charline)
      equivalence (r4line, charline)

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
#
# filorg =1  = BIL
# filorg =2  = BIP
# filorg =3  = BSQ

# numtyp =1  = 16-bit signed integers
# numtyp =2  = 32-bit signed integers
# numtyp =3  = 32-bit floating point

	fidnoff = float(idnoff)    # offset, floating point

	# how many records in to skip to read the next sheet
	if (filorg==1) {            # BIL
		itmp = dz*(iline-1) + nrecshdr

	} else if (filorg==2) {     # BIP
		itmp = dx*(iline-1) + nrecshdr
	}

	if (chfirst < 0 || chlast > dz || chfirst > chlast) {
		write (ttyout,200) chfirst, chlast, dz
200		format (/,' ERROR in read2sheet: channel range:',
			i6,i6,'  is not',/,
			'       in the range 1 to',i6,' or',
			' first channel > second')
		if (chfirst <1 || chlast > dz ) {

			write (ttyout,201)
201			format (' Because channels are out of range, check your wavelength set',/,
				'          and be sure wavelegth set units are the same as in',/,
				'          the expert system command file.')
		}
		ioerr = 1
		return
	}

#  use only the channels required from the channel set
	#if BIL then:
	if (filorg==1) {
		isetrec_first = chfirst		# BIL format
		isetrec_last = chlast
		isetcell_first = 1
		isetcell_last = dx

	#if BIP then:
	} else if (filorg==2) {
		isetrec_first = 1			# BIP format
		isetrec_last = dx
		isetcell_first = chfirst
		isetcell_last = chlast

	# if error (BSQ or something else) then:
	} else {
		write(ttyout,202) filorg
202		format (/,' ERROR in read2sheet: filorg=',
		i6,'  is not BIL (filorg=1) or BIP (filorg=2)')
		ioerr = 1
		return
	}

# Read data record into large 2d array

	do isetrec = isetrec_first, isetrec_last {

		# Increment through records in 3d input file
		key = itmp + isetrec

		# Read 3d input file record
		read (qlun,rec=key,iostat=ioerr) charline(1:reclen)
			# (NOTE: charline is equivalenced above)

		if (ioerr != 0) {
			write (ttyout,*)'ERROR on read in read2sheet: ',ioerr
			write (ttyout,*)'Input image, rec = ',key
			write (ttyout,*)'Input image, line= ',iline
		}

		do isetcell = isetcell_first, isetcell_last {

			#if BIL then:
			if (filorg==1) {            # BIL
				ichanread = isetrec
				ipixread = isetcell

			#if BIP then:
			} else if (filorg==2) {     # BIP
				ichanread = isetcell
				ipixread = isetrec
			}

			#IF Integer*2  (Integer)
			if (numtyp == 1) {         # 16-bit signed integers
            		# Scale and offset values within band of interest
				itmp2 = i2line(isetcell)
				if (itmp2 == iptdrop) {
				      # specdat(channel_number, x_pixel)
					specdat(ichanread,ipixread) = -1.23e34
				}
				else {
				#define: specdat(chan,pixel)  BIP  [chan is array inner loop]
				      # specdat(channel_number, x_pixel)
					specdat(ichanread,ipixread) = float(itmp2+idnoff)*scale
				}

			#IF Integer*4  (Long Integer)
			} else if (numtyp == 2) {         # 32-bit signed integers
	      	    	# Scale and offset values within band of interest
				itmp2 = i4line(isetcell)
				if (itmp2 == iptdrop) {
				      # specdat(channel_number, x_pixel)
					specdat(ichanread,ipixread) = -1.23e34
				}
				else {
				#define: specdat(chan,pixel)  BIP  [chan is array inner loop]
				      # specdat(channel_number, x_pixel)
					specdat(ichanread,ipixread) = float(itmp2+idnoff)*scale
				}

			#IF Real*4  (Floating-point)
			} else if (numtyp == 3) {          # 32-bit float
          			# Scale and offset values within band of interest
				rtmp2 = r4line(isetcell)
				if (rtmp2 == iptdrop) {
					specdat(ichanread,ipixread) = -1.23e34
				}
				else {
				#define: specdat(chan,pixel)  BIP  [chan is array inner loop]
					specdat(ichanread,ipixread) = (rtmp2+fidnoff)*scale
				}

				# DEBUG
				#if (iline == 1 || iline == 2 || iline == 3 || iline == 4 ||   iline == 20 || iline == 40 || iline == 80 ) {
				#	if (isetrec == 100) {                         # column 100

				#		write (ttyout,*) ' FLOAT DEBUG: iline sample=', iline, isetrec,'   ichanread =', ichanread, '   ipixread=', ipixread
				#		#write (ttyout,*) ' FLOAT DEBUG: ichanread =', ichanread, '   ipixread=', ipixread
				#		write (ttyout,*) ' FLOAT DEBUG: specdat(ichanread,ipixread), (',ichanread, ipixread,')= ', specdat(ichanread,ipixread)
				#		write (ttyout,*) ' '
				#	}
				#}

			} else {
				write(ttyout,204) numtyp
204				format (/,' ERROR in read2sheet: numtyp=',
				i6,'  image data is not Int*2, Int*4, or Real*4')
				ioerr = 1
				return
			}

		}

	}  # end do isetrec

      return
      end


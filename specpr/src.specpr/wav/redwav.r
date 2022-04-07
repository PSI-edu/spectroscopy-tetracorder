	subroutine redwav (iwidfl, ifil, iftst)
	implicit integer*4 (i-n)

#ccc  name: redwav
#ccc  version date: 4/10/85
#ccc  author(s): Roger N. Clark
#ccc  language: Ratfor
#ccc
#ccc  short description: read a standard data record up to 4852
#ccc			points, in blocks, and treat as wavelengths
#ccc
#ccc  algorithm description: same as for redfil
#ccc  system requirements:
#ccc  subroutines called:
#ccc          devsta,crtin,posfil,ertyp,dread,newsta,rewinf
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

#################################################################
#                                                               #
#       read file routine                                       #
#                                                               #
#       arguments                                               #
#         iwidfl   logical unit to read from coded as letter id #
#         ifil          record nember to be read.               #
#         iftst         error flag to calling routine           #
#                                                               #
#################################################################


	include "../common/spmaxes"   # max parameters, must be first

	include "../common/label1"
	include "../common/lundefs"
	include "../common/lblwav"
	include "../common/alphabet"

	integer*4 input(384)
	equivalence (input,iwcflg)

	real*4 codata(383)
	equivalence (codata, iowbuf(2))

	integer*4 	redtap
	integer*4	iftst
	integer*2	ibit, chkbit

	integer*4	revbytes
	
	logical tape

# ifiu is temp record # that gets incremented from irecno

	ifiu = ifil

# revbytes =1 reverses bytes on the binary data.  By default it is zero
# byte order is that on HP, Sun sparc machines.
#  - Roger N. Clark 2/18/2001

		revbytes = 0
#LINUX		revbytes = 1


# decode capital letter to a lower case letter

	iwd = 0
	if (iwidfl == ihcv) iwd = ihv
	if (iwidfl == ihcw) iwd = ihw
	if (iwidfl == ihcd) iwd = ihd
	if (iwidfl == ihcu) iwd = ihu
	if (iwidfl == ihcy) iwd = ihy
	if (iwd == 0 ) {
		write (ttyout,17)iwidfl
17		format ('ERROR: INVALID WAVELENGTH FILE ID: ', a, /)
		iftst = 1
		return
	}

# decode lower case letter to a logical unit

	call devlun (4, iwd, idev)


	iftst = 0
	call devsta (idev,ista,0,iprt)
	if (ista==-4) {
		iftst=4
		return
	}
	if (ifiu < 1) {
		print * ,' ERROR: RECORD NUMBER < 1'
		go to 12
	}
	if (ifiu>maxrec) {
		write (ttyout, 11) maxrec
11		format (' ERROR: Wavelength file request greater than ', i6, /)
		go to 12
	}
	if (iprt != -1 & ifiu>iabs(iprt)) {
		write (ttyout,16)
16		format (' ERROR: Wavelength file request ',
				'GREATER THAN PROTECTION',/)
12		iftst=4
		print *, ' Press return to EXIT'
		call crtin
		ifiu=1
		return
	}

	call posfil (idev, ifiu, tape, iftst)
	if (iftst!=0) {
		call ertyp('posfil',idev,iftst)
		if (tape) {
			call rewinf(idev,irer)
			ier = irer
			call newsta(idev,11,1)
		}
		return
	}

	if (tape) {
		key=ifiu-1
		iftst = redtap(idev,iowbuf)
		if (iftst==0) call xfti
	} else {
		key=ifiu
		read(idev,rec=key+1,iostat=iftst) iowbuf
		if (revbytes == 1) {
			call bytorder (iowbuf, 1)
		}
	}
	if (iftst != 0) {
		call ertyp('redwav ',idev,iftst)
	}
	call newsta (idev, 11, ifiu)

# if bit 1 of flag is 1 then this was a text record, so exit

	ibit=1
	if (chkbit(iowbuf(1),ibit) == 1) {
		write (ttyout, 18)
18		format (' ERROR in reading wavelength record', /,
			' TEXT BIT SET, so there are NO WAVELENGTHS to read', /)
			iftst = 1
			return
	}

# this was fisrt record of potential multiple record spectrum;
#	if bit 0 of flag is 1 then this is a continuation record,
#	not the first.  Issue error message and exit

	ibit=0
	if (chkbit(iowbuf(1), ibit) == 1) {
		write (ttyout, 20) 
20		format (' ERROR: CONTINUATION BIT SET, but this',
			' should have been the ', /,
			'        first record of requested data set',/,
			'        press return to EXIT read routine',
			/)
		call crtin
		iftst = 1
		return
	}
	
# copy input buffer to lblwav common (input)

	do i = 1, 384
		input(i) = iowbuf(i)

# if iwtchn > 256 then read in succeeding records

	irect = int((float(iwtchn) -256.0)/383.0 +0.999)

	if (irect >= 1) {
		do j = 1, irect {
			ifiu = ifiu +1
			iftst = 0
			call devsta (idev,ista,0,iprt)
			if (ista==-4) {
				iftst=4
				return
			}
			if (ifiu<1) ifiu= 1
			if (ifiu>maxrec) {
				write (ttyout, 11) maxrec
				iftst=1
				call crtin
				ifiu=1
				return
			}
			if (iprt != -1 & ifiu>iabs(iprt)) {
				write (ttyout,16)
				iftst=1
				call crtin
				ifiu=1
				return
			}

			call posfil (idev, ifiu, tape, iftst)
			if (iftst!=0) {
				call ertyp('posfil',idev,iftst)
				if (tape) {
					call rewinf(idev,irer)
					ier = irer
					call newsta(idev,11,1)
				}
				return
			}

			if (tape) {
				key=ifiu-1
				iftst = redtap(idev,iowbuf)
				if (iftst==0) call xfti
			} else {
				key=ifiu
				read(idev,rec=key+1,iostat=iftst) iowbuf
				if (revbytes == 1) {
					call bytorder (iowbuf, 1)
				}
			}
			if (iftst != 0) {
				call ertyp('redfil ',idev,iftst)
			}
			call newsta (idev, 11, ifiu)

# check continuation bit then copy data to lblwav common

			ibit=0
			if (chkbit(iowbuf(1), ibit) == 0) {
				write (ttyout, 30)
30 format (' ERROR on read of continuation record', /,
	   '       CONTINUATION BIT NOT SET, press return to EXIT')
				call crtin
				iftst = 1
				return
			}

			isegm = 256 + (j-1)*383
			do m = 1, 383
				wdata(isegm +m) = codata(m)

		}
	}			


#****************
	do i=1,iwtchn
		if (wdata(i)>-1.23001e34 && wdata(i)<-1.22999e34) wdata(i) = -1.23e34

	return
	end

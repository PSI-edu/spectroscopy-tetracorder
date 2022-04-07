	subroutine redfil (ifiu, idev, iftst)
	implicit integer*4 (i-n)

#ccc  version date: 04/05/85
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description: This routine reads a specpr format data
#ccc    file and puts the data into the label 1 common block according to
#ccc    flags.  Muldiple records of data are read and combined into one
#ccc    large spectrum.
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc          devsta,crtin,posfil,ertyp,dread,newsta,rewinf
#ccc  argument list description:
#ccc        arguments: ifiu,idev,iftst
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
#         ifiu          record nember to be read.               #
#         idev          logical unit to read from.              #
#         iftst         error flag to calling routine           #
#                                                               #
#################################################################

	include "../common/spmaxes"   # max parameters, must be first

	include "../common/label1"
	include "../common/lundefs"
	include "../common/iocontrol"
	include "../common/ioftyp"

	integer*4 input(384)
	equivalence (input,icflag)

	real*4 codata(383)
	character*1532 tdata
	equivalence (codata, iobuff(2))
	equivalence (tdata, iobuff(2))

	integer*4 	redtap
	integer*4	iftst
	integer*2	ibit, chkbit

	integer*4	revbytes
	
	logical tape

# revbytes =1 reverses bytes on the binary data.  By default it is zero
# byte order is that on HP, Sun sparc machines.
#  - Roger N. Clark 2/18/2001

		revbytes = 0
#LINUX		revbytes = 1

	#write (*,*) 'DEBUG: revbytes=',revbytes

	rferfl(2)=0
	iftst = 0
	call devsta (idev,ista,0,iprt)
	if (ista==-4) {
		iftst=4
		return
	}
         if (ifiu<1) {
         	write(ttyout,*)' ERROR: Invalid Record Number:', ifiu
        	go to 12
         }
	if (ifiu>maxrec) {
		write (ttyout, 11) ifiu, maxrec
11		format (' ERROR: File Request',i5,' GREATER THAN ', i6, /)
		go to 12
	}
#
# now check the file type.  if normal specpr file proceed, otherwise
#     call the 3d io routines then exit
#
	ifptr = ftptr(idev)
	if (filtyp(1,ifptr) == 3) {        # file type is 3d
		call read3d (idev, iftst)
		if (iftst != 0) {
			write (ttyout,*) 'ERROR at record:',key
			call ertyp('redfil ',idev,iftst)
			return
		}
		call newsta (idev, 11, ifiu)
		return
	}
#
# normal specpr data file:
#
	if (iprt != -1 & ifiu>iabs(iprt)) {
		write (ttyout,16) ifiu, iprt
16		format (' ERROR: File Request',i5,
			' GREATER THAN PROTECTION',i5,/)
12		iftst=4
		print *,' Press Return to continue'
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
		iftst = redtap(idev,iobuff)
		if (iftst==0) call xfti
	} else {
		key=ifiu
		read(idev,rec=key+1,iostat=iftst) iobuff
		if (revbytes == 1) {
			call bytorder (iobuff,1)
		}
	}
	if (iftst != 0) {
		write (ttyout,*) 'ERROR at record:',key
		call ertyp('redfil ',idev,iftst)
		return
	}
	call newsta (idev, 11, ifiu)


# this was fisrt record of potential multiple record spectrum;
#	if bit 0 of flag is 1 then this is a continuation record,
#	not the first.  Issue error message and exit

	ibit=0
	if (chkbit(iobuff(1), ibit) == 1) {
		if (rferfl(1) == 1) {
			write (ttyout, 20) ifiu
			call crtin
		}
		rferfl(2) = 1
		iftst = 1
		return
	}
	
# copy input buffer to label1 common (input)

	do i = 1, 384
		input(i) = iobuff(i)

#debug:
#	write (ttyout,402) itchan
#402	format (1x,'read segment 1: itchan=',i5)

# if itchan > 256 then read in succeeding records if data record
# if itxtch > 1476 then read in succeeding records if text record

	ibit = 1
	if (chkbit(icflag,ibit) == 0) {
		if (itchan < 1) itchan = 1
		if (itchan > maxchn) itchan = maxchn
		irect = int((float(itchan) -256.0)/383.0 +0.999)
	} else {
		if (itxtch < 1) itxtch = 1
		if (itxtch > maxtxt) itxtch = maxtxt
		irect = int((float(itxtch) -1476.0)/1532.0 +0.999)
	}

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
				write (ttyout, 11) ifiu, maxrec
				iftst=1
				call crtin
				ifiu=1
				return
			}
			if (iprt != -1 & ifiu>iabs(iprt)) {
				write (ttyout,16) ifiu, iprt
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
				iftst = redtap(idev,iobuff)
				if (iftst==0) call xfti
			} else {
				key=ifiu

# debug:
#	jseg = j+1
#	write (ttyout,401) ifiu, itchan, jseg
#401	format(1x,'reading continuation record',i5,' channels',i5,' segment=',i4)

				read(idev,rec=key+1,iostat=iftst) iobuff
				if (revbytes == 1) {
					call bytorder (iobuff,1)
				}
			}
			if (iftst != 0) {
				write (ttyout,*) 'ERROR at record:',key
				call ertyp('redfil ',idev,iftst)
			}
			call newsta (idev, 11, ifiu)

# check continuation bit then copy data to label1 common

			ibit=0
			if (chkbit(iobuff(1), ibit) == 0) {
				write (ttyout, 30) ifiu
30 			format (' ERROR on read at continuation record',i5, /,
		   '       CONTINUATION BIT NOT SET, press return to EXIT')
				call crtin
				iftst = 1
				return
			}

			ibit =1
			if (chkbit(icflag,ibit) == 0) {
				isegm = 256 + (j-1)*383
				do m = 1, 383
					data(isegm +m) = codata(m)
	
			} else {
				isegm = 1476 + (j-1)*1532
				itext(isegm:isegm+1532) = tdata
			}
		}
	}			


#****************
	ibit = 1
	if (chkbit(icflag,ibit) == 0) {
		do i=1,itchan {

			# the following checks if the number is Not a Number, NaN
			if (data(i) != data(i)) data(i) = -1.23e34

			if (data(i) > -1.23001e34 && 
				data(i)<-1.22999e34) data(i) = -1.23e34

			# the following added because linux puts out -inf
			#    - RNC 2/17/2005  further restriction 5/16/2005

			if (data(i) < -1.23e34) data(i) = -1.23e34
			if (data(i) >  1.0e36) data(i) = -1.23e34
		}

# delete points not in spectrum to clear buffer

		ii = itchan+1
		if (ii <= maxchn) {
			do i = ii, maxchn
				data(i) = -1.23e34
		}
#               check if can set wavelength automatically
		if (idwcon != 0) {
			call autowv (i)
			idwcon = 0
		}
	}

	return


20	format (' ERROR: CONTINUATION BIT SET on record',i5,' but this',
		' should have been the ', /,
		'first record of requested data set',/,
		'        press return to EXIT read routine',
		/)

	end

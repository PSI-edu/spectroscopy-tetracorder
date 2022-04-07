	subroutine wrtspr (ikey, lun, ier)
	implicit integer*4 (i-n)

#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc         This subroutine writes the file
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc         devsta,crtin,posfil,ertyp,x2ti,drite,newsta
#ccc  argument list description:
#ccc         arguments: ikey,lun,ier
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc
#ccc  Randall Dailey 05/24/2004
#ccc    Updated to call bytorder to swap bytes upon output
#ccc    if little endian platform
#################################################################
#                                                               #
#       write file routine                                      #
#                                                               #
#       arguments                                               #
#         ikey         record number to write                   #
#         lun          logical unit to write to                 #
#         ier           error flag to calling routine           #
#                                                               #
#################################################################

	include "../src.specpr/common/spmaxes"
	include "../src.specpr/common/label1"
	include "../src.specpr/common/lblprt"
	include "../src.specpr/common/lundefs"
	include "../src.specpr/common/lblg"

	integer wrttap
	integer*4 ier
	integer*4 ikey


	integer*4 input(384)
	equivalence (input,icflag)

	dimension codata(383)
	character*1532 tdata
	equivalence (codata, iobuff(2))
	equivalence (tdata, iobuff(2))

	integer*2 chkbit, ibit
	
	integer*4 revbytes

# revbytes =1 reverses bytes on the binary data.  By default it is zero
# byte order is that on HP, Sun sparc machines.
#  - Roger N. Clark 2/18/2001

                revbytes = 0
#LINUX          revbytes = 1
	


	ibit = 1
	if (chkbit(icflag,ibit) == 0) {
		call pad(ititl)
		call pad(ihist)
		call pad(mhist)

#		for (i=1; i<=3; i=i+1) {
#			call pad(cta(i))
#			call pad(ctb(i))
#			call pad(sta(i))
#			call pad(stb(i))
#			call pad(datea(i))
#			call pad(dateb(i))
#		}
	}

# code to be written

	ier= 0
	if(ikey < 1) ikey = 1
	if (ikey>maxrec) {
		write(ttyout, 11) ikey, maxrec
11		format (' file request', i5,' greater than max:', i6, /)
		ikey=1
		call crtin
		ier=4
		return
	}
	if (ikey > 0) {
	ibit = 1
	if (chkbit(icflag,ibit) == 0) {
		filno=ikey
	}

# load io buffer

	do i = 1, 384
		iobuff(i) = input(i)

# clear continuation bit

	ibit = 0
	call clrbit (iobuff(1),ibit)

# write buffer

		if (revbytes == 1) {
			call bytorder (iobuff,2)
		}

		key = ikey +1
		write(lun,rec=key,iostat=ier)iobuff

# check for io error

	if (ier!=0) {
		call ertyp('wrtsp',lun,ier)
		ier=4
		return
	}

# if itchan > 256 write additional segments  (if data)
# if itxtch > 1476 write additional segments (if text)

	ibit = 1
	if (chkbit(icflag,ibit) == 0) {
		if (itchan > maxchn) itchan = maxchn
		if (itchan < 0) itchan = 0
		irect = int((float(itchan) - 256.0)/383.0 +0.999)
	} else {
		if (itxtch > maxtxt) itxt = maxtxt
		if (itxtch < 0) itxtch = 0
		irect = int((float(itxtch) - 1476.0)/1532.0 + 0.999)
	}

	if (irect >= 1) {
		do j = 1, irect {

			ikey = ikey +1

# load next segment of data into io buffer
			ibit = 1
			if (chkbit(icflag,ibit) == 0) {
				isegm = 256 + (j-1)*383
				do m = 1, 383
					codata(m) = data(isegm +m)
			} else {
				isegm = 1476 + (j-1)*1532
				tdata = itext(isegm:isegm+1532)
			}

# set continuation bit

			ibit = 0
			call setbit(iobuff(1), ibit)

			ier= 0
			if (ikey>maxrec) {
				write(ttyout, 11) ikey, maxrec
				ikey=1
				call crtin
				ier=4
				return
			}
			filno=ikey

# write buffer

				if (revbytes == 1) {
					call bytorder (iobuff,2)
				}

				key=ikey +1
				write(lun,rec=key,iostat=ier)iobuff
			}
# check for io error
			if (ier!=0) {
				call ertyp('wrtsp',lun,ier)
				ier=4
				return
			}
		}
	}

#**********
	return
	end

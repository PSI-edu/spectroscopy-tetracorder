	subroutine f11dfl(ic)
	implicit integer*4 (i-n)

#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc         This subroutine reads the last used values for
#ccc         the object, standard, and solar data file from
#ccc         record 28 of the title file.
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc          dread,filinp
#ccc  argument list description:
#ccc    arguments: ic
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
#       This routine reads the last used values for the         #
#       object, standard, and solar data files from record      #
#       28 of the title file.                                   #
#                                                               #
#       Author: JAH     4/19/83                                 #
#                                                               #
#       Arguments:                                              #
#           ic          ic is set to  'e' or 'x'  if the        #
#                       user types  e or x. It is set to -1     #
#                       if end of file is encountered or if     #
#                       the data in the title file record is    #
#                       invalid.  Otherwise it is set to zero.  #
#                                                               #
#################################################################

	include "../common/spmaxes"   # max parameters, must be first

	include "../common/blank"
	include "../common/therml"
	include "../common/label1"
	include "../common/lbl3"
	include "../common/lundefs"
	include "../common/alphabet"

	real*4 rsarry(SPMAXCHAN)
	integer*4 jdumy(64)

	integer*4 ier

	equivalence (rsarry,datab)
	equivalence (jdumy(7),xjdumy),(jdumy(9),yjdumy)


	read(ttllun,rec=28,iostat=ier) jdumy
	if (ier==-1 || ier==27) {
		ic = -1
		return
	} else if (ier!=0) {
		call ertyp('f11: ',15,ier)
		ic = ihx
		return
	}
	irodev=jdumy(1)
	irofil=jdumy(2)
	irsdev=jdumy(3)
	irsfil=jdumy(4)
	isodev=jdumy(5)
	isofil=jdumy(6)
	ronorm=xjdumy
	rsnorm=yjdumy
	if (irodev == 0) {
		ic = -1
		return
	}
	itmp = irofil
	call filinp(irodev,itmp,err,ic)
	if (ic==ihx) return
	write(ttyout,20) irodev,irofil,ititl,ronorm

	itmp = irsfil
	call filinp(irsdev,itmp,err,ic)
	if (ic==ihx) return
	write(ttyout,30) irsdev,irsfil,ititl,rsnorm
	do i=1,maxchn
		rsarry(i) = data(i)
	if (ictrl==ihe) {
		ifil = itmp+1
		call filinp(irsdev,ifil,err,ic)
		if (ic==ihx) return
		do i=1,maxchn
			error(i) = data(i) * ronorm
	}

	itmp = isofil
	call filinp(isodev,itmp,err,ic)
	if (ic==ihx) return
	write(ttyout,40) isodev,isofil,ititl

	ic = 0
	return

20 format(' last setup:',/,
	  ' last object   = ',a,i4,':',a,' albedo=',f7.4)
30 format(' standard      = ',a,i4,':',a,' albedo=',f7.4)
40 format(' solar flux/pi = ',a,i4,':',a)
	end

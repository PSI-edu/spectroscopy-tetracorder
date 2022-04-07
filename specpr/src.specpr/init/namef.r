	subroutine namef(igo,ititle,idevc,iwdgt,isavt,iwrkt,inmu,inmy)
	implicit integer*4 (i-n)
########################################################
#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language: Ratfor
#ccc
#ccc  short description:
#ccc    this routine assigns the tape names for the various
#ccc    devices.
#ccc
#ccc  algorithm description: none
#ccc  system requirements: none
#ccc  subroutines called:
#ccc            crtin,getstr,devsta,rewinf,newsta,chkdev
#ccc  argument list description:
#ccc    arguments:
#ccc      igo           kludge to pick which device
#ccc      ititle        kludge to determine whether to assign all
#ccc                            tape names.
#ccc      idevc         kludge not used??????
#ccc      iwdgt         tape name of device w.
#ccc      isavt         tape name of device v.
#ccc      iwrkt         tape name of device d.
#ccc      inmu          tape name of device u.
#ccc      inmy          tape name of device y.
#ccc  parameter description:
#ccc  common description: none
#ccc  message files referenced: none
#ccc  internal variables:
#ccc            i       input character counter
#ccc            ier     error return indicator
#ccc		irer	return error from call rewinf
#ccc            ista    file status
#ccc            iprt    file protection
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc
######################################################################
#                                                                    #
#                                                                    #
#                                                                    #
######################################################################

	character*40    ititle
	integer*4         idevc(4)
	character*8     iwdgt,isavt,iwrkt,inmu,inmy
	logical         tape

	if (igo == 0 | igo == 35) {
		print *,' type in the tape name of the file w.'
		call crtin
		i=1
		call getstr(i,iwdgt,ier)
		call devsta(9,ista,1,iprt)
		if (ista >= 1  &  ititle(1:2) == 'zx') {
			idev=9
			ifiu=1
			call posfil (idev, ifiu, tape, iftst)
			if (iftst!=0) {
		                call ertyp('posfil',idev,iftst)
       				if (tape) {
					call rewinf(idev,irer)
					ier = irer
		                        call newsta(idev,14,1)
		                }
		        }
		}
		if (ititle(1:2) != 'zx') goto 1

	} else if (igo == 30) {
1		print *,' type in the tape name of the file v.'
		call crtin
		i = 1
		call getstr(i,isavt,ier)
		call devsta(8,ista,1,iprt)
		if (ista >= 1  &  ititle(1:2) == 'zx') {
			idev=8
			ifiu=1
			call posfil (idev, ifiu, tape, iftst)
			if (iftst!=0) {
		                call ertyp('posfil',idev,iftst)
       				if (tape) {
					call rewinf(idev,irer)
					ier = irer
		                        call newsta(idev,14,1)
		                }
		        }
		}
		if (ititle(1:2) != 'zx') goto 2

	} else if (igo == 45) {
2		print *,' type in the tape name of the file d.'
		call crtin
		i = 1
		call getstr(i,iwrkt,ier)
		call devsta(7,ista,1,iprt)
		if (ista >= 1  &  ititle(1:2) == 'zx') {
			idev=7
			ifiu=1
			call posfil (idev, ifiu, tape, iftst)
			if (iftst!=0) {
		                call ertyp('posfil',idev,iftst)
       				if (tape) {
					call rewinf(idev,irer)
					ier = irer
		                        call newsta(idev,14,1)
		                }
		        }
		}
		if (ititle(1:2) != 'zx') goto 3

	} else if (igo == 150) {
3		print *,' type in the tape name of the file u.'
		call crtin
		i = 1
		call getstr(i,inmu,ier)
		call devsta(3,ista,1,iprt)
		if (ista >= 1  &  ititle(1:2) == 'zx') {
			idev=3
			ifiu=1
			call posfil (idev, ifiu, tape, iftst)
			if (iftst!=0) {
		                call ertyp('posfil',idev,iftst)
       				if (tape) {
					call rewinf(idev,irer)
					ier = irer
		                        call newsta(idev,14,1)
		                }
		        }
		}
		if (ititle(1:2) != 'zx') goto 4

	} else if (igo == 60) {
4		print *,' type in the tape name of the file y.'
		call crtin
		i = 1
		call getstr(i,inmy,ier)
		call devsta(4,ista,1,iprt)
		if (ista >= 1  &  ititle(1:2) == 'zx') {
			idev=4
			ifiu=1
			call posfil (idev, ifiu, tape, iftst)
			if (iftst!=0) {
		                call ertyp('posfil',idev,iftst)
       				if (tape) {
					call rewinf(idev,irer)
					ier = irer
		                        call newsta(idev,14,1)
		                }
		        }
		}
	}
	call chkdev
	return
	end

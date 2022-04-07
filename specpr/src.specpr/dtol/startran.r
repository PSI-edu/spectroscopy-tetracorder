	integer*4 function startr(idev,ifiln,iflnu)
	implicit integer*4 (i-n)

#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc      This subroutine transfers starpack data from
#ccc      label1 common to blank common and writes it to dfisc.
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc                    redfil,wristr
#ccc  argument list description:
#ccc     arguments: idev,ifiln,iflnu
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
#       this routine transfers starpack data from label1        #
#       common to blank common andwrites it to disk             #
#                                                               #
#       extracted form disv.r 04-08-83                          #
#                                                               #
#       the return value of this function is zero unless        #
#       an abnormal condition arises                            #
#################################################################

	include "../common/spmaxes"   # max parameters, must be first

	include "../common/blank"
	include "../common/lbl7"
	include "../common/lbl6"
	include "../common/lbl4"
	include "../common/label1"
	include "../common/labl2"
	include "../common/lbl3"
	include "../common/label3"
	include "../common/labelf"
	include "../common/info"
	include "../common/lblg"
#
#     put goodness of fit data of starpack from tape into unlabeled
#     common
#
	mhistb = mhist
	ihista = ihist
	ititl1 = ititl
	do i= 1, SPMAXCHAN {
		dataa(i)= data(i)
		if (i>3) next 1
		iraa(i)=ira(i)
		ideca(i)=idec(i)
	}

	itmcha=itimch
	nruna=nruns
	iwtrna=iwtrns
	xnrma=xnrm
	sctma=scatim
	tmint=timint
	revs= revs1
	irmasa= irmas
#
#     read intercepts data of starpack from tape and put in unlabeled
#     common
#
	iflnu= iflnu+ 1
	call redfil (iflnu, idev, iftst)
	if (iftst!=0) {
		startr=-1
		return
	}

	mhistb = mhist
	ihistb = ihist
	ititl2 = ititl
	do i= 1, SPMAXCHAN {
		datab(i)= data(i)
		if (i>3) next 1
		idecb(i)=idec(i)
		irab(i)=ira(i)
	}
	irmasb= irmas
	iwtrnb=iwtrns
	xnrmb=xnrm
#
#     read log intensity data of starpack from tape and put in unlabeled
#     common
#
	iflnu= iflnu+ 1
	call redfil (iflnu, idev, iftst)
	if (iftst!=0) {
		startr=-1
		return
	}

	ihistc = ihist
	ititle = ititl
	do i= 1, SPMAXCHAN {
		datac(i) = data(i)
	}
	ifl2= ifiln
	revs2= revs1
#
#     write starpack on disk
#
	call wristr(ierr,ifiln)
	if (ierr!=0) {
		startr=-1    
		return 	
	}
	startr=0
	return

	end

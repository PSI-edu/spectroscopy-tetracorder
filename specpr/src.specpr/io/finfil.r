	subroutine finfil(ifilx,is,df,ier)
	implicit integer*4(i-n)

#ccc  version date: 06/01/83
#ccc  author(s): roger clark & jeff hoover
#ccc  language:  fortran
#ccc
#ccc  short description:
#ccc                   this subroutine puts data into datab if df=2 and
#ccc                   into dataa array if df=1
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc                    redfil
#ccc  argument list description:
#ccc        arguments: ifilx,is,df,ier
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  notes:
#ccc

	include "../common/spmaxes"   # max parameters, must be first

	include	"../common/blank"
	include	"../common/lbl7"
	include	"../common/lbl8"
	include	"../common/label1"
	include	"../common/labl2"
	include	"../common/lbl3"
	include	"../common/label3"
	include	"../common/labelf"
	include "../common/lundefs"

	integer*4	df
#
#     read data into label 1 common
#
	call redfil(ifilx,is,ier)
	if (ier!=0)
		ier = 4
	else
#
#     put data into datab array if df= 2
#
	 if (df<1||df>3) {
		write(ttyout,10)df
		ier = 4
	}
	else if (df==1) {
#
#     put data into dataa array if df= 1
#
		do j = 1,maxchn
			dataa(j) = data(j)
		ititl1 = ititl
		revs1 = revs
		mhista = mhist
		itmcha = itimch
		nruna = nruns
		iwtrna = iwtrns
		xnrma = xnrm
		sctma = scatim
		tminta = timint
		irmasa = irmas
		ihista = ihist
		mcta1  = iscta
		mctb1  = isctb
		mstb1  = istb
		mdatea1= jdatea
		mdateb1= jdateb
	} else if (df==2) {
		do j = 1,maxchn
			datab(j) = data(j)
		ititl2 = ititl
		revs2 = revs
		mhistb = mhist
		iwtrnb = iwtrns
		xnrmb = xnrm
		tmintb = timint
		irmasb = irmas
		ihistb = ihist
		mcta2  = iscta
		mctb2  = isctb
		mstb2  = istb
		mdatea2= jdatea
		mdateb2= jdateb
	} else {
		do j=1,maxchn
			datac(j) = data(j)
		ititle = ititl
		ihistc = ihist
	}
	return
10  format(" illegal argument in finfil ",i6)
	end

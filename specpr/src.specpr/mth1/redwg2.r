	subroutine redwg2(inext)
	implicit integer*4 (i-n)
#################################################################
#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc                   This subroutine performs math operations,
#ccc                   decodes call operations and calls band
#ccc                   normalization rouitine
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc      er,whedr,mathin,crtin,wjfren,rundiv,opdcod,ixecut,
#ccc      sb,mthwrt,titles,wrifil,wriout,closenr,bkrec
#ccc  argument list description:
#ccc        argument: inext
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
#       this is the math operations routine.                    #
#                                                               #
#       AUTHOR  Roger N. Clark                                  #
#       Modified: JAH 04-12-83  convert to ratfor               #
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
	include "../common/alphabet"
	include "../common/lundefs"

#RED
	integer*4 ihchar     # function ihchar

	character*80 ipcn
	character*10 iopsav
	character*45 ipa
	character*14 file
	logical		 tapelu
	integer*2 ibit

	data idm, idd, ida, ids         /1h*, 1h/, 1h+, 1h-/
	data ih                         /1h /
	data ihcoln                     /1h: /

	do i=1,10
		ipc(i) = ichar(' ')

	ispfcn=0
	iprodp = 0

	itl = ' '

	if (ictrl==2 || ictrl==3) go to 149

1   call eralph
	call whedr
	idad= 0
	inext= 0
	idisu= 0
	is=0
	is1=0
	is2=0
	ic=0
	idad2= 0
	ictrl=0
	ispfcn=0
	df=0
	ixit= 0
#
#     write instructions
#
	call mathin (infmth,ibncon,iopcon,ipcn)
	idad = 1
	istarp(1)= ih0
	istarp(2)= ih0
#
#     write previous operation
#
	ierror= ih
	id=0
	call crtin
	call alias(iopcon)
	iiii=1
	call wjfren(iiii,x,illl)
	if (iiii>=80) goto 1
	if (illl==ihe || illl==ihx) {
		inext = ihx
		goto 9000
	}
	if (iopcon(1:2)=='t ') {
		inext=iht
		goto 9000
	}
	if (iopcon(1:1)=='l') {
		inext=ihchar(iopcon(2:2))
		goto 9000
	}
	if (iopcon(1:2) == 'bn') ibncon = 0
    if (iopcon(1:2) == 'b ') ibncon = 1
	if (iopcon(1:2) == 'i ') infmth=1
	if (iopcon(1:2) == 'in') infmth=0
	if (iopcon(1:1) == 'i') go to 1
128 i=1
	lic1= 1
	lic= 1
#
#     store iopcon array into ipcn array
#

	ipcn = iopcon

	go to 303
110 continue
	if (i>=80 || lic>=80 || id==ida) go to 1
	i= lic+1
#**********************************************************
#
#     call operations decode routine
#
#**********************************************************
#
303 igo=0
#
# make sure text bit is not set
#
	ibit = 1
	call clrbit (icflag,ibit)

	ictrl=0
	idad=0
	idad2=0
    call opdcod(i,lic,lic1,id,ipcn,iopcon,ixx,igo,is,ictrl,
     	        ifila,idv1,df,ier,ifl1,is1,is2,x,il,ispfcn,idv2,
		ifilb,ifl2,ierror,istarp,cx,xc,xx,iops,icln,maxchn,datab)
#
	do lk= 1,maxchn
		datac(lk)=0.0

	if (idv2==ihc) xnrmb=1.0
	if (igo==1101) {
1101    inext=ihe
9000    continue
		return
	}
	if (igo==110 ) go to 110
	if (igo==1   ) go to 1
	if (igo==148 ) go to 148
#
#     call operation routines requested c
#
#     check band normalization enter flag
#         option bn = no band normalization
#         option b  = band normalization
#

	if (index(iops,'b') != 0) ibncon = 1
	if (index(iops,'bn') != 0) ibncon = 0

#       production mode

	if (index(iops,'p') != 0) iprodp = 1
	if (index(iops,'pn') != 0) iprodp = 0
	else iprodp = 0

	if (ierror==ihe) ictrl=ihe
	if (id==ids) call runsub (ic,cx,iprodp)
	if (id==idd) {
		idisu= 2
		call rundiv(ic,cx,iprodp)
		if (ictrl==ihe) {
			idad=2
			idad2=2
		}
		ictrl=0
		if (ic==ihe) go to 110
		if (ic==ihx) go to 1
		go to 150
	}
	if (id!=ihf) {
		if (id==ihcoln) {
			write(ttyout,1011)
#			call wfcn(icln,ifcn,ifl1,iprodp)
			call wfcn(icln,ifcn)
		}
		if (ifcn==ihe) go to 110
		if (id==idm) call mulrun (ic,cx,iprodp)
		if (ic==ihe) go to 110
		if (ic==ihx) go to 1
		if (id==idm) idisu= 2
		if (ictrl==ihe) idad=2
		if (ictrl==ihe) idad2=2
		go to 150
	}

# next line is lower and upper limits of fcns

	if (ispfcn < 1 || ispfcn > 50) {
		write (ttyout, 1013) ispfcn    # write invalid fcn
		call crtin
		go to 1
	}

# next checks for fcns not available within above limits

	if (ispfcn > 25 && ispfcn < 36) {
		write (ttyout, 1013) ispfcn    # write invalid fcn
		call crtin
		go to 1
	}
	if (ispfcn == 41 || ispfcn == 99) {
		write (ttyout, 1013) ispfcn    # write invalid fcn
		call crtin
		go to 1
	}


	write(ttyout,1010) ispfcn


	if (ispfcn==1) call f1(ic)
	if (ispfcn==2) call f2(ic)
	if (ispfcn==3) call f3(ic,ipcn,cx,ispfcn,constx,iopsav)
	if (ispfcn==4) call f4(ic,ipcn,constx,ispfcn,iopsav)
	if (ispfcn==4 && ic==4) goto 128
	if (ispfcn==5) call f5(ic)
	if (ispfcn==6) call f6(ic)
	if (ispfcn==7) call f7(ic)
	if (ispfcn==8) call f8(ic)
	if (ispfcn==9) call f9(ic)
	if (ispfcn==10) call f10(ic)
	if (ispfcn==11) call f11(ic)
	if (ispfcn==12) call f12(ic)
	if (ispfcn==13) call f13(ic)
	if (ispfcn==14) call f14(ic)
	if (ispfcn==15) call f15(ic,iprodp)
	if (ispfcn==16) call f16(ic)
	if (ispfcn==17) call f17(ic,idv1,ifl1)
	if (ispfcn==18) call f18(ic)
	if (ispfcn==19) call f19(ic)
	if (ispfcn==20) call f20(ic)
	if (ispfcn==21) call f21(ic)
	if (ispfcn==22) call f22(ic)
	if (ispfcn==23) call f23(ic)
	if (ispfcn==24) call f24(ic)
	if (ispfcn==25) call f25(ic)
#if (ispfcn==26) call f26(ic)
    if (ispfcn==26) call system("rogue")
	if (ispfcn==27) call f27(ic)
	if (ispfcn==28) call f28(ic)
	if (ispfcn==29) call f29(ic)
	if (ispfcn==36) call f36(ic)
	if (ispfcn==37) call f37(ic)
	if (ispfcn==38) call f38(ic)
	if (ispfcn==39) call f39(ic)
	if (ispfcn==40) call f40(ic)
	if (ispfcn==41) call f41(ic)
	if (ispfcn==42) call f42(ic)
	if (ispfcn==43) call f43(ic,idv1,ifl1)
	if (ispfcn==44) call f44(ic)
	if (ispfcn==45) call f45(ic)
	if (ispfcn==46) call f46(ic)
	if (ispfcn==47) call f47(ic)
	if (ispfcn==48) call f48(ic)
	if (ispfcn==49) call f49(ic)
	if (ispfcn==50) call f50(ic)
#
# save last operation in restart file
#
	if (ic==ihx) go to 1
	if (ic==ihe) go to 110
	if (ictrl==ihe) idad=2
	go to 150

148	call sb(0)
	if (ierror==ihe) ictrl= ihe
#
#     call addition routines
#
	write(ttyout,1012)
	call wegadd(id)

	if (id==ihe || id==ihx) goto 1

149	idisu=2
	if (ictrl==2) ierror= ih
	if (ictrl==3) ierror= ihe
	ictrl= ih
	idad = 2
	id = ida
	idad2= 2
150	write(ttyout,724)
	if (ictrl==ihe) idad2=2
	call mthwrt(igo,is,ifilnu,iprodp)
	if (igo==150) go to 150
	if (igo==1101) go to 1101
	if (igo==110) go to 110
#
# set time data last processed
#
	call sptime
#
# set userid
#
	call fuser(usernm)
#
# select title
#
	call titles (ititl1,iopcon,ititl,itl,ier,iprodp,ibncon,bbnd,ubnd)
	if (ier==ihx) go to 1
	if (ier==ihe) go to 110
#
#     call band normalization routine if the operation was +, /, or *
#     the flag is idisu and the enter flag (ibncon) is set to 1
#
1841    if (idisu==2 && ibncon==1) call banorm

	do j= 1, maxchn
		data(j) = datac(j)

	ird = 0
	iauto = 0
#
#
#     call plot on crt routine
#
	ibncn2= 0
	filno= ifilnu
	if (iprodp!=1) call wriout
	if (ixit==ihx) go to 1
	if (ibncn2==ihb) {
		ibncon= 1
		idisu= 2
		go to 1841
	}
#
# set wavelength pointer if wavelength set is in the same file as data
#
	call setwpt (idv1, itrol(1), itrol(2))
#
	if (idad2==2) ieros=1
155     continue
#
# set number of channels and write results
#
	itchan = nchans
	call wrifil (ifilnu, is, ierr)
	call rstart(1)

#  force close for EOF on mt
	if (tapelu(is)) {
		call closnr(is)
#  rewind will fail on disk files
		call bkrec(is,1,ier)
	}

	if (ierr==4) go to 1
	if (ierr==2 && idad2==2) go to 170
	if (ierr==2) go to 110
	if (ierr!=2) go to 1
	if (idad2==2) go to 170
	if (idad!=2) go to 110
	if (ird==1) go to 110
170     do j= 1, maxchn
		data(j) = error(j)

	if (ird==1) go to 110
#
#     write errors in following file if involved
#
	write (ititl,185) ifilnu
	ifilnu= ifilnu+ 1
	filno= ifilnu
	if (inext==2) write(ititl(15:20),184)
	write(ihist,188)
	call eralph
	write(ttyout,186) ifilnu
	call crtin
	iii=1
	call wjfren(iii,xxx,idpp)
	ird = 1
	ieros=0
	if (idpp!=ihx) go to 155
	go to 110
31      format(1x,i4,2x,f12.6,5x,i4,2x,f12.6,5x,i4,2x,f12.6)
184     format (' sigma mean ')
185     format ('errors to previous file', i5,12(1h ))
186     format (2x, 'writing errors in file', i5, 5x,
			'type c to continue, x  to exit')
188     format ('errors', 54(1h ))
724     format (1x, 72(1h*))
1010    format(' Transfering to special function',i3)
1011    format(' Transfering to Trig. and Algebraic routines')
1012    format(' Transfering to addition routine')
1013	format(' ERROR: f',i3,'  is NOT AVAILABLE',//,
		' Press return to continue')
      end

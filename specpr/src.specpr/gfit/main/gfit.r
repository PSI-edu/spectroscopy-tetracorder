# 12.11.81 GFIT main routine modified by Rodney Kam ratfor.
#
#  this routine is the monitor for the 1980 version of gfit. its
#  responsibilities are :
#
#      1: collects userid from terminal
#      2: opens gstart and specpr restart files
#      3: reads initial status from record 0 of gstart
#      4: collects commands from terminal and calls appropriate
#         subroutines, or prints an error message.
#
#  that's all.
#-----------------------------------------------------------------------
#
    implicit integer*4 (i-n)
	common/input/ line(80)

	include "../status"
	include "../../common/label1"
	include "../../common/lbl3"
	include "../../common/lbl7"
	include "../../common/lbl6"
	include "../../common/lbl4"
	include "../../common/label3"
	include "../../common/labelf"
	include "../../common/lblg"
	include "../../common/info"
	include "../../common/lblvol"
	include "../../common/hptrm"
	include "../../common/cmd"


	dimension gerror(66), id(3)
	dimension igtmp(252)
	equivalence (label,igtmp)

    character*40 igrfl
	character*40 igfl
	character*45 cmd
	character*2 acmd

	logical good,hfo
	logical ireset

	integer*2 lunin
	integer*2 icmd(2)
    character*2 icom(1:24)

%	  data ibl/'  '/
    data lungf,me/1,6/
	data maxcom/24/

	hfo = .false.
    icom(1) = 'pe'
	icom(2) = 'pf'
	icom(3) = 'ep'
	icom(4) = 'eg'
	icom(5) = 'fp'
	icom(6) = 'fg'
	icom(7) = 'sa'
	icom(8) = 're'
	icom(9) = 'rf'
	icom(10) = 'ra'
	icom(11) = 'fi'
	icom(12) = 'pr'
	icom(13) = 'wr'
	icom(14) = 'sp'
	icom(15) = 'e'
	icom(16) = 'li'
	icom(17) = 'es'
	icom(18) = 'se'
	icom(19) = 'xe'
	icom(20) = 'fs'
	icom(21) = 'sf'
	icom(22) = 'xf'
	icom(23) = 'fe'
	icom(24) = 'lf'


	hfo = .false.

	call hreset(1)
	label = ' '
	ititlr = ' '
    ititlw = ' '

	idr = ibl
	ifiler = 0
	ierror = .false.
	iwavel = 0
	idw = ibl
	ifilew = 0

	do i = 1,4 {
		itnamr(i) = 0
	    itnamw(i) = 0
	}

	ifilnr = 0
	ifilnw = 0

	igaus = 0
	icont = 0
	iter = 0
	acc = 0.
	invx = .false.
	log = .false.
	do i = 1,10 {
		iptdel(1,i) = 0
		iptdel(2,i) = 0
	}

	do i = 1,66 {
		params(i) = 0.
	}

	xmin = 0.
	ymin = 0.
	xmax = 0.
	ymax = 0.
	invxp = .false.
	logp = .false.

	do i = 1,252 {
		ifit(i) = igtmp(i)
		iscrat(i) = igtmp(i)
	}

	do i = 1,12 {
		idum(i) = 0
	}

	open(2,file='.trans')
    rewind 2
	
	write(6,100)
100 format(////,70('-'),/,25(' '),'Gaussian FITting program',/,
	28(' '),'Version 10/28/82',/,70('-'),///)
	call rstart(2)

	igfl = igrfl
    igfl(index(igrfl,' '):40) = ".gfrf"
	call gstart(2,igfl)
#
#
#----------------------------------------------------------
#  duplicate info for writing into all 3 fields
#----------------------------------------------------------
	call glist
	if (ifilew .eq. 0) ifilew = iwprot + 1

	ifit(25) = idw
	ifit(26) = ifilew
	iscrat(25) = idw
	iscrat(26) = ifilew

	do j = 51,77 {
		ifit(j) = igtmp(i)
		iscrat(j) = igtmp(i)
	}

	repeat {
		repeat {
#
			write(6,200)
%200		format(' MAIN',$)
			call crtin
			ic = 1
			for(i=1;i<=2;i=i+1) {
				call wjfren(ic,x,il)
				icmd(i) = il
    		}
			acmd = iopcon(1:2)
			good = .false.
			do k = 1,maxcom {
				if ( acmd .eq. icom(k)) {
					mcom = k
					good = .true.
				}
			}

			if (.not. good) {
				write(me,250)
250				format(' no!'/)
			}

		} until (good)

		call rstart(1)
		call gstart(1,igfl)
		if (mcom == 1) {
			call gfplot
			write(6,260)
260			format(1x,'Returned From Plotting',/)
		} else if (mcom == 2) {
			call gswap(1,2)
			call gfplot
			write(6,260)
			call gswap(1,2)
		} else if (mcom == 3) {
			call chgest
		} else if (mcom == 4) {
			call chgues
		} else if (mcom == 5) {
			call gswap(1,2)
			call chgfit
			call gswap(1,2)
		} else if (mcom == 6) {
			call gswap(1,2)
			call prtfit(6)
			call gswap(1,2)
		} else if (mcom == 7) {
			write(6,280)
280			format(' record # ?'/)
			call crtin
			nchar = 1
			call wjfren(nchar,x1,il)
			ir = x1
			call save(lungf,ir)
		} else if (mcom == 8) {
			write(6,280)
			call crtin
			nchar = 1
			call wjfren(nchar,x1,il)
			ir = x1
			call recall(lungf,ir,1)
		} else if (mcom == 9) {
			write(6,280)
			call crtin
			nchar = 1
			call wjfren(nchar,x1,il)
			ir = x1
			call recall(lungf,ir,2)
		} else if (mcom == 10) {
			write(6,280)
			call crtin
			nchar = 1
			call wjfren(nchar,x1,il)
			ir = x1
			call recall(lungf,ir,3)
		} else if (mcom == 11) {
			call send(1,2)
			call trnsfr(1,gerror,b,xmean,sigma,igfl)
			call gstart(1,igfl)
			call rstart(1)
			write(6,1234)
1234                    format(1x,'Transfering to the fit routine...',/)
                        do i = 1,4
				close(i,iostat=ier)
                        do i = 7,20
				close(i,iostat=ier)
                        cmd = 'fits ' // igrfl
			ii=system(cmd // char(0))
			write(6,1235)
1235                    format(1x,'Returning from the fit routine...',/)
			call rstart(2)
			call gstart(2,igfl)
			call trnsfr(2,gerror,b,xmean,sigma,igfl)
			call prtfit(6)
			call gswap(1,2)
		} else if (mcom == 12) {
			call gswap(1,2)
			call prtfit(12)
			call gswap(1,2)
		} else if (mcom == 13) {
			call gswap(1,2)
			if (.true.) {
				call shuffl(igaus,params)
				call wrtfit(gerror,b,xmean,sigma,iprot)
				call shuffl(igaus,params)
			} else {
				write(6,260)
			}
			call gswap(1,2)
			iwprot = iprot
			ifilew = iwprot + 1
		} else if (mcom == 14) {
		} else if (mcom == 15) {
			call rstart(1)
			call gstart(1,igfl)
			stop
		} else if (mcom == 16) {
			call glist
		} else if (mcom == 17) {
			call send(1,3)
		} else if (mcom == 18) {
			call send(3,1)
		} else if (mcom == 19) {
			call gswap(1,3)
		} else if (mcom == 20) {
			call send(2,3)
		} else if (mcom == 21) {
			call send(3,2)
		} else if (mcom == 22) {
			call gswap(3,2)
		} else if (mcom == 23) {
			call send(2,1)
		} else if (mcom == 24) {
			write(6,400)
400			format(2x,'record #',3x,'label'/)
			do j = 1,20 {
				call recall(lungf,j,1)
				if (j < 0 ) break
				write(6,410) j,label
410				format(4x,i2,3x,20a2)
			}
			call recall(lungf,0,3)
		}
      } until (hfo)
      stop
      end

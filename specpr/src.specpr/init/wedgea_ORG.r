	subroutine wedgea
	implicit integer*4(i-n)

#ccc  version date: 06/01/83
#ccc  author(s): roger clark & jeff hoover
#ccc  language:  fortran
#ccc
#ccc  short description:
#ccc
#ccc  algorithm description: none
#ccc  system requirements: none
#ccc  subroutines called:
#ccc  	wdgsb1,wdgsb2,wdgsb3,wdgsb4,wdgsb5,dfasin,namef,
#ccc  	whedr,wjfren,crtin,initt,er
#ccc  argument list description:
#ccc        argument: none
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

	include "../common/blank"
	include "../common/lbl7"
	include "../common/lbl6"
	include "../common/lbl4"
	include "../common/label1"
	include "../common/labl2"
	include "../common/lbl3"
	include "../common/label3"
	include "../common/labelf"
	include "../common/lblg"
	include "../common/lblvol"
	include "../common/lblprt"
	include "../common/cmd"
	include "../common/alphabet"
	include "../common/filenames"

character*9 anull
logical iwdflg

data anull/NULL/
data ibell/7/,ihgt/">"/

if (ititle(1:2)!="zx") {
	if (ititle(1:2)=="xz")
		go to 30
#
	call wdgsb1(iwdflg)
	if (ititl(1:1)=="a")
		return
	if (!iwdflg) {
		call whedr
		go to 20
	}
}
repeat {
	call eralph
	call whedr
	if (ixit!=2) {
		i = 1
		call wjfren(i,x,il)
		if (il==ihb)
			call wjfren(i,x,il)
		if (i<80&&i>2)
			go to 10
	}
	call wdgsb5    # print information for user
	i = 1
	call crtin
	call wjfren(i,x,il)
10  if (i>=80)
		break 1
	ixit = 2
	if (il!=iha) {       #note a option no longer active
		if (il!=iho)
			if (il==ihw) {
				igo = 35
				go to 40
			} else if (il==ihv) {
				igo = 30
				go to 40
			} else if (il==ihd) {
				igo = 45
				go to 40
			} else if (il==ihu) {
				igo = 150
				go to 40
			} else if (il==ihy) {
				igo = 60
				go to 40
			} else if (il==ihf) {
				call prochk
				next 1
			} else if (il==ihg) {
				call wjfren(i,x,il2)
				ix = x
				if (x==0&&i>=80)
					ix = 4
				call initt(ix)
				if (ix == -1) {
					write(6,*) 'Cannot initialize.'
					write(6,*) 'Hard Exit'
					stop
				}
				next 1
			} else {
				if (il==ihr)
					go to 30
				if (il==ihb) {
					if (iprom==ibell)
						iprom = ihgt
					else
						iprom = ibell
					next 1
				} else {
					call wjfren(i,x,il2)
					if (il!=ihc||il2!=ihp)
						break 1
20					call wdgsb2(iwdflg)
					if (iwdflg)
						next 1
					call eralph
					call whedr
				}
			}
		call wdgsb4(iwdflg)
		if (iwdflg)
			next 1
		ivfl = anull
		iwfl = anull
		idfl = anull
		iufl = anull
		iyfl = anull
30		call dfasin
		if (ititle(1:2)=="xz")
			return
		if (ititle(1:2)=="zx")
			next 1
		igo = 0
40		call namef(igo,ititle,idevc,iwdgt,isavt,iwrkt,inmu,inmy)
		if (ititle(1:2)=="zx")
			next 1
	}
	if (ititle(1:2)!="zx")
		return
}
ititle(1:1) = " "
call sb(0)
call rstart(1)
return
end

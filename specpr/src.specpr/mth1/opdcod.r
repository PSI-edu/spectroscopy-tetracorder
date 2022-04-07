subroutine opdcod(i,lic,lic1,id,ipcn,iopcon,ixx,igo,is,ictrl,
		ifila,idv1,df,ier,ifl1,is1,is2,x,il,ispfcn,idv2,
		ifilb,ifl2,ierror,istarp,cx,xc,xx,iops,icln,maxchn,datab)
    implicit integer*4(i-n)
#ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
#cc  version date: 06/01/83
#cc  author(s): Roger Clark & Jeff Hoover
#cc  language:  fortran
#cc
#cc  short description:
#cc       This subroutine decodes the math operation sequence.
#cc  algorithm description: none
#cc  system requirements:   none
#cc  subroutines called:
#cc                    decod1,finfil,wjfren,nwav,erored
#cc  argument list description:
#cc       arguments: i,lic,lic1,id,ipcn,iopcon,igo,is,ictrl
#cc              ier,ifl1,is1,is2,x,il,ispfcn,idv2,
#cc              ierror,istarp,cx,iops,icln,datab
#cc  parameter description:
#cc  common description:
#cc  message files referenced:
#cc  internal variables:
#cc  file description:
#cc  user command lines:
#cc  update information:
#cc  notes:
#cc
#cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

	include "../common/spmaxes"   # max parameters, must be first

include "../common/lundefs"
include "../common/alphabet"

#RED
integer*4 ihchar       # function ihchar

character*80 iopcon,ipcn
integer*4 istarp(2)
character*40 iops
real*4       datab(SPMAXCHAN)
integer*2 xc
integer*4 ier,df
#data ihcoln/":"/
ihcoln = ihchar(':')
#
#     this subroutine decodes the math operations sequence.
#
    kludge = 0
	kldge2 = 0
    repeat {
		kludge = kludge + 1
		if (kludge > 10) {
			igo=110
			return
		}
    	lic1 = 0
    	ierror = 0
#
#     set options to zero
#
    	iops = " "
#
#     restore iopcon array from ipcn array
#
    	iopcon = ipcn
    	itemp = i
#
#     do operations between lic1 to lic
#
    	do i = itemp,80 {
    		if (iopcon(i:i)!=" "&&lic1==0)
    			lic1 = i
    		lic = i
    		if (iopcon(i:i)==",")
    			go to 10
    		if (iopcon(i:i)=="+")
    			go to 910
    		if (iopcon(i:i)==" ")
    			ixx = ixx+1
    	}
#
    	if (ixx==80)
    		go to 140
    	lic = 80
    	if (lic1<=0)
    		next 1
10   i = lic1
	    if (lic1>=lic)
	    	go to 140
    	if (lic1>=80)
    		go to 140
    	if (lic1<=0)
    		go to 140
    	itemp = i
    	do i = itemp,80 {
    		if (lic==i)
    			go to 900
    		if (iopcon(i:i)!=" ")
    			break 1
   		}
    	is = 0
    	repeat {
#
#     decode file id and number
#
    		if (iopcon(i:i)=="v")
    			is = 8
    		if (iopcon(i:i)=="w")
    			is = 9
    		if (iopcon(i:i)=="d")
    			is = 7
    		if (iopcon(i:i)=="u")
    			is = 3
    		if (iopcon(i:i)=="y")
    			is = 4
#
#     if a function is requested here, no file has been read, so
#     set ictrl to -1 as the flag.
#
    		if (iopcon(i:i)=="f") {
    			ictrl = -1
    			go to 20
			}
    		if (i>=80)
    			go to 110
    		if (is==0)
    			i = i+1
            kldge2 = kldge2 + 1
			if (kldge2 > 10) {
			   igo = 110
			   return
			}
	} until(is!=0)
    	idv1 = ihchar(iopcon(i:i))
    	is1 = is
    	i = i+1
    	ifila = 0
    	m = 0
    	itemp = i
    	do i = itemp,80 {
    		if (lic==i)
    			go to 900
#
#     determine operation and decode first file number
#
    		if (iopcon(i:i)!=" ") {
    			if (iopcon(i:i)=="-")
    				break 1
    			if (iopcon(i:i)=="/")
    				break 1
    			if (iopcon(i:i)=="f")
    				break 1
    			if (iopcon(i:i)==":")
    				break 1
    			if (iopcon(i:i)=="m")
    				break 1
    			if (iopcon(i:i)<"0"||iopcon(i:i)>"9")
    				break 1
    			call decod1(iopcon(i:i),m)
    			ifila = ifila*10+m
#debug:				print *,"ifila = ",ifila
			}
		}
    	if (ifila==0)
    		go to 110
#
#     find first file and read in data
#
    	df = 1
    	ifl1 = ifila
    	call finfil(ifila,is,df,ier)
    	if (ier==4)
    		go to 90 
    	is = is1
#
#     if an error occurs take proper action
#
    	if (ier!=0)
    		go to 140
    	if (ier==1)
    		go to 110
    	is2 = 0
    	itemp = i
    	do i = itemp,80 {
    		if (lic==i)
    			go to 900
#
#     decode operation
#
    		if (iopcon(i:i)=="-")
    			go to 20
    		if (iopcon(i:i)=="*")
    			go to 20
    		if (iopcon(i:i)=="/")
    			go to 20
    		if (iopcon(i:i)==":")
    			icln = i+1
    		if (iopcon(i:i)==":")
    			go to 20
    		if (iopcon(i:i)=="f")
    			go to 20
		}
    	if (i>80)
    		go to 110
20   id = ihchar(iopcon(i:i))
#timj
	    if (id==ihcoln)
	    	go to 120
    	if (iopcon(i:i)=="f")
    		go to 30
    	i = i+1
    	itemp = i
    	do i = itemp,80 {
    		if (lic==i)
    			go to 900
#
#     if starpack is involved, go to statement 340
#
    		if (iopcon(i:i)=="s")
    			go to 100
#
#     if a number occurs here, second file id is
#     missing and defaults to the first
#
    		if (iopcon(i:i)>="0"&&iopcon(i:i)<="9")
    			go to 60 
    		if (iopcon(i:i)!=" ")
    			break 1
		}
    	is = 0
    	repeat {
#
#     decode second device number
#
    		if (iopcon(i:i)=="v")
    			is = 8
    		if (lic==i)
    			break 1
    		if (iopcon(i:i)=="w")
    			is = 9
    		if (iopcon(i:i)=="d")
    			is = 7
    		if (iopcon(i:i)=="u")
    			is = 3
    		if (iopcon(i:i)=="y")
    			is = 4
    		if (iopcon(i:i)=="c")
    			go to 50
    		if (i>80)
    			go to 110
    		if (is==0)
    			i = i+1
    		if (is!=0)
    			go to 40
	}
900   continue
    }
910   continue
    i = i+1
    j = 0
    itemp = i
    do i = itemp,80 {
    	if (lic==i)
    		break 1
    	if (iopcon(i:i)!=" ") {
    		j = j+1
    		if (j>40)
    			break 1
    		if (iopcon(i:i)=="e")
    			ierror = ihe
    		iops(j:j) = iopcon(i:i)
		}
	}
#debug:	print *,"iops = |",iops,"|"
#debug:	print *,"ierror = ",ierror
    igo = 148
    write(iopcon,170)iops
    ipoa = 1
    ipob = 14
    go to 130
30  i = i+1
    x = 0.0
    call wjfren(i,x,il)
    if (x==0.0&&il!=0)
    	go to 140
    if (il!=0)
    	i = i-1
    ispfcn = x+0.5
    go to 80 
#
#     decode file id to device
#
40  idv2 = ihchar(iopcon(i:i))
    go to 70 
50  i = i+1

    call wjfren(i,cx,il)
	if (il!=0) i=i-1

    if (i<=80) {
    	j = 0
    	itemp = i
    	do i = itemp,80 {
    		if (lic==i)
    			break 1
    		if (iopcon(i:i)!=" ") {
    			j = j+1
#
#     decode options
#
    			if (j>40)
    				break 1
    			if ((iopcon(i:i)=="V")|(iopcon(i:i)=="W")|(iopcon(i:i)
				=="D")|(iopcon(i:i)=="U")|(iopcon(i:i)=="Y"))
    					call nwav(i)
    			iops(j:j) = iopcon(i:i)
    			if (iops(j:j)=="e")
    				ierror = ihe
			}
		}
	}
    do j = 1,maxchn
	    datab(j) = cx
    is = -1
    idv2 = ihc
    ifilb = 0
    go to 120
60  i = i-1
    write(ttyout,150)
70  is2 = is
    if (is2==is1)
    	idv2 = idv1
    i = i+1
    ifilb = 0
#
    call wjfren(i,xfilb,il)
	ifilb=xfilb
#debug:	print *," xfilb = ",xfilb
#debug:	print *," ifilb = ",ifilb
	if (il != 0) i=i-1

    if (ifilb==0) go to 110
#
#     read second file
#
    df = 2
    ifl2 = ifilb
    call finfil(ifilb,is,df,ier)
    if (ier==4)
    	go to 90 
    if (ier!=0)
    	go to 140
80  j = 0
    itemp = i
    do i = itemp,80 {
    	if (lic==i)
    		break 1
    	if (iopcon(i:i)!=" ") {
    		j = j+1
#
#     decode options
#
    		if (iopcon(i:i)=="e")
    			ierror = ihe
    		if (j>40)
    			break 1
    		iops(j:j) = iopcon(i:i)
#
#     if an  a  is encountered, decode new wavelength file.
#
    			if ((iopcon(i:i)=="V")|(iopcon(i:i)=="W")|(iopcon(i:i)
				=="D")|(iopcon(i:i)=="U")|(iopcon(i:i)=="Y"))
    			call nwav(i)
		}
	}
    go to 120
90  igo = 1
    return
#
#     decode starpack
#
100 i = i+1
    istarp(1) = ihchar("s")
    istarp(2) = 0
    j = i+6
    m = 0
    call wjfren(i,xtarp,il)
	istarp(2) = xtarp
	if(il != 0) i=i-1

    if (istarp(2)>0) {
    	idv2 = ihchar("s")
    	ifilb = istarp(2)
    	ifl2 = istarp(2)
    	if (lic==i)
    		go to 120
    	j = 0
    	itemp = i
    	do i = itemp,80 {
    		if (lic==i)
    			go to 120
    		if (iopcon(i:i)!=" ") {
    			j = j+1
    			if (j>40)
    				go to 120
    			if (iopcon(i:i)=="e")
    				ierror = ihe
    			if ((iopcon(i:i)=="V")|(iopcon(i:i)=="W")|(iopcon(i:i)
				=="D")|(iopcon(i:i)=="U")|(iopcon(i:i)=="Y"))
    				call nwav(i)
    			iops(j:j) = iopcon(i:i)
			}
		}
    	if (istarp(2)>0)
    		go to 120
	}
110 igo = 110
    return
120 igo = 400
#
#     write requests to file for storage for later recall as previous
#     operation.
#
    ipoa = lic1
    ipob = lic
    if (ipoa<1)
    	ipoa = 1
    if (ipob>80)
    	ipob = 80
    if (ipob<=ipoa)
    	ipoa = 1
    if (ipob<=ipoa)
    	ipob = 80
    if (ispfcn==4)
    	ipoa = 1
    write(iopcon,160)iopcon(ipoa:ipob)
130 ipoc = ipob-ipoa+1
    if (ipoc<80)
    	do j = ipoc,80
    		iopcon(j:j) = " "
    icnt = 128
    key = 26
    ier = 0
    write(ttllun,rec = key+1,iostat = ier)iopcon
    call erored(icnt,ier,128)
    iopcon = ipcn
    return
140 igo = 1101
    return
150 format(2x,"second file id defaults to the first")
160 format(a)
170 format("+",a10,69(" "))
end




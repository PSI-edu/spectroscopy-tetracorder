    subroutine extnta(igo)
    implicit integer*4(i-n)
#ccc  name:
#ccc  version date:
#ccc  author(s):
#ccc  language:
#ccc
#ccc  short description:
#ccc
#ccc  algorithm description:
#ccc  system requirements:
#ccc  subroutines called:
#ccc  argument list description:
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
	include "../common/lbl4"
	include "../common/label1"
	include "../common/lblg"
	include "../common/labelf"
	include "../common/xtnct"
	include "../common/alphabet"
	include "../common/lundefs"

#RED
	integer*4 ihchar       # function ihchar

	character*180 temp
	character*180 xhist
	equivalence (xhist,ihista)

    integer*4 ier
	
    data ihcoma/', '/


    if (igo==15) {
	repeat {
	    write(ttyout,80)
	    do i = 1,20 {
			ifile(i) = 0
			is(i) = 0
	    }
	    j = 0
	    repeat {
#
#     decode file id's and record numbers
#
			i = 1
			call crtin
			if (j!=0) {
				if (iopcon(i:i)>='0' && iopcon(i:i)<='9') go to 20
				if (iopcon(i:i)==',') go to 10
				if (iopcon(i:i)==' ') go to 10
			}
			repeat {
				if (j>20) go to 30
				while (iopcon(i:i)!='w' &&
				   iopcon(i:i)!='v' &&
				   iopcon(i:i)!='d') {
					if (iopcon(i:i)=='c') go to 30
					if (i==80) go to 930
					i = i+1
				}
				j = j+1
				is(j) = ihchar(iopcon(i:i))
				repeat {
					i = i+1
					if (i>80) go to 930
					while (iopcon(i:i)!=' ') {
						if (iopcon(i:i)==',') break 1
						if (iopcon(i:i)<'0' || iopcon(i:i)>'9') go to 920
						mx = 0
						call decod1(iopcon(i:i),mx)
						ifile(j) = ifile(j)*10+mx
						if (ifile(j)>maxrec) go to 940
						i = i+1
						if (i>80) go to 930
						if (iopcon(i:i)==' ' || iopcon(i:i)==',') {
10							repeat {
								i = i+1
								if (i>80) go to 930
								if ((iopcon(i:i)<'0' || iopcon(i:i)>'9') &&
								  iopcon(i:i)!=' ' &&
								  iopcon(i:i)!=',') go to 920
								if (iopcon(i:i)!=' ' &&
								  iopcon(i:i)!=',') break 1
							}
							if (j==20) go to 30
20							is(j+1) = is(j)
							j = j+1
						}
					}
910                                     continue
				}
920                             continue
			}
930                     continue
	    }
940         continue
	    write(ttyout,90)
	    next 1
30		nfile = j
	    if (nfile<2) go to 70
	    call er
	    write(ttyout,100)
	    call crtin
	    ititl1(17:40) = iopcon(1:48)
	    write(ttyout,110)
	    xhist = ' '
		mhista = ' '
		mhistb = ' '
	    tst = 5768.43
	    xtst = alog10(tst)
#
#     decode history into arrays: ihista, ihistb, ihistc
#
#   k=next free character in xhist
#	j=next free character in temp
	    xhist(1:8) = iwdgt
		k = 9
	    iauto = ihw
	    istarp(2) = ihcoma
	    repeat {
			l = 1
# RED			Initialize to 0
			iflag = 0
			repeat {
				j = 1
				temp = ' '
				do i = l,20
					if (is(i)==iauto) {
						write(temp(j:j+4),605)ifile(i)
						j = j+5
						iflag = i
					}
				if (j==1) break 1		#no characters in temp
				xhist(k:k+j-1) = temp(1:j-1)
				k = k+j
				if (iflag>19) break 1	#no more files to do
				l = iflag+1
			}
			if (iauto==ihd) break 1
			if (iauto==ihv) {
				xhist(k:k+7) = iwrkt
				l = 1
				k = k+8
				iauto = ihd
			} else {
				xhist(k:k+7) = isavt
				l = 1
				k = k+8
				iauto = ihv
			}
	    }
#
#     decode 3 titles
#

	    ititl2(17:40) = ititl1(17:40)
	    ititle(17:40) = ititl1(17:40)

	    ititl1(1:16) = 'goodness of fit'
	    ititl2(1:16) = 'starpack;slopes'
	    ititle(1:16) = 'intercepts'

	    ier = 0
	    xnrmt = 0.
#******************************************************
#
#     read requested files, convert spectra to base 10 logarithms, and
#     write results to a workfile.
#
#******************************************************
	    do i = 1,nfile {
			if (is(i)!=ihw&&is(i)!=ihv&&is(i)!=ihd) go to 40
			if (is(i)==ihw) is(i) = 9
			if (is(i)==ihv) is(i) = 8
			if (is(i)==ihd) is(i) = 7
			if (ifile(i)>0) {
				call redfil(ifile(i),is(i),ier)
				if (ier==4) go to 70
				if (ier!=0) go to 40
				air(i) = irmas/1000.0
				write(ttyout,130)i,ititl,filno,air(i)
#------------------------------------------------------------------
				xnrmt = xnrmt+xnrm
				do jit = 1,nchans
				if (data(jit)<=0.1e-36) data(jit) = 100000.0
				else {
					txa = data(jit)
					txb = alog(txa*1.0)*1.0
					txb = txb*0.4342945
					data(jit) = txb
				}
#------------------------------------------------------------------
#
#     write data to a workfile
#
				write(addlun,rec=i+1,iostat=ier)data
				call ertyp('extnta',10,ier)
				if (i==1) revs1 = revs
				if (revs1!=revs) write(ttyout,140)i
				revs1 = revs
			}
	    }
	    break 1
40		write(ttyout,120)is(i),ifile(i)
	}
#******************************************************
	write(ttyout,150)
#
#     execution time: approx. 7 seconds per run
#
	xnrma = xnrmt/float(nfile)
	do i = 1,20
	    do j = 1,11
			idel(i,j) = 0
#
#     do a least squares analysis of channel versus airmass
#
	do m = 1,nchans {
	    mm = m
	    call linfit(mm)
	}
	igo = 2100
	return
    } else if (igo==361) {
		repeat {
#
#     read a starpack
#
	    call er
	    j = 0
	    write(ttyout,160)
	    call crtin
	    n = 1
	    call wjfren(n,x,il)
	    if (il==ihx || il==ihe) go to 70
	    if (il==0 && x>=0 && x<=50) break 1
	}
	i = x
	if (i==0) j = 1
	if (i==0) i = 1
	repeat {
	    iraa(1) = 0
	    if (i!=0) {
			call reastr(ier,i)
			if (ier!=0) break 1
			if (i!=25) go to 60
			go to 50
		}
	    repeat {
			i = i+1
			if (j==1&&i<50) go to 1000
			if (j!=0||i>=50) go to 1010
50			continue
				write(ttyout,170)
				call crtin
				if (j==0) go to 1010
				if (j>=50) go to 1010
				call er
60				if (iraa(1)>(-100)&&iraa(1)<100)
				write(ttyout,180)ititl1(17:40)
			if (.not. i<50) go to 50
	    }
1000       continue
	}
1010  continue
    } else if (igo==520) {
	repeat {
#
#     write a starpack
#
	    write(ttyout,190)
	    call crtin
	    n = 1
	    call wjfren(n,x,il)
	    if (il==0) {
		ifl2 = x
		if (ifl2>0&&ifl2<=50) goto 1020
	    }
	}
1020    continue
	ah = 0.001
	al = 100.0
	do i = 1,nfile {
	    if (al>air(i)) al = air(i)
	    if (ah<air(i)) ah = air(i)
	}
	irmasa = al*1000.
	irmasb = ah*1000.
	ifl1 = nfile
	iraa(1) = 1
	call wristr(ier,ifl2)
	if (ier!=0) write(ttyout,200)ifl2
	else call er
     } else if (igo==700)
	repeat {
#
#     enter manual history
#
	    call er
	    write(ttyout,210)mhista(1:74),mhista(75:148),
			mhista(149:222),mhista(223:296),
			mhistb(1:74),mhistb(75:148),
				mhistb(149:222),mhistb(223:296)
	    call crtin
	    i = 1
	    call wjfren(i,x,il)
	    if (il==ihe) call er
	    if (il!=0) break 1
	    ix = x+0.5
	    if (ix>=1&&ix<=8) {
		write(ttyout,220)ix
		j = (ix-1)*37+1
		j2 = j+36
		call crtin
		mhista(2*j-1:2*j2) = iopcon
	    } else {
		write(ttyout,240)
		do n = 1,8 {
		    ix = n
		    j = (ix-1)*37+1
		    j2 = j+36
		    call crtin
		    mhista(2*j-1:2*j2) = iopcon
		}
	    }
	}
70	igo = 1
	return


80      format (' files for extinction correction:',/,
'  type in file id and record number; when all are read in type  c', /)

90      format (' record number greater than 2048. start again')

100     format (' type in starpack title, 24 characters',/,1x,23('-'),'i',/)

110     format(1x,39('*')/,16x,'working',/,1x,39('*')/)

120     format (' error in finding file: device ',
		a,2x,'file',i5,5x,'try again!',/)

130     format (i3,2x,a,2x,'file',i4,2x,'airmass=',f7 .3)

140     format (1x,72('*')/,' revs not equal in run ',i2)

150     format (' *** phase 1 complete: required spectra found correctly',/,
' *** begin phase 2: least squares analysis',/)

160     format (' type in starpack number, type 0 to list'/)

170     format (' press return to continue')

180     format (1x,a,5x,i2)

190     format (' type in starpack number'/)

200     format (' error in writing starpack ',i2)

210     format (' the current manual history is:',/,
8(1x,a74,/),1x,74('-'),/,
' type in the line number you wish to change, or 0 for all lines',/,
' type e to exit routine',/)

220     format (' type in one line (73 characters) to replace line',i2,'.',/)

230     format (a)

240     format (' type in 8 lines (at 73 characters per line) ',
		'for the manual history',/)
605     format (i4,',')

	end

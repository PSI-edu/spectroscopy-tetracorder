	subroutine disv(idev)
	implicit integer*4 (i-n)

#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc       This subroutine handles all file transfer requests.
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc       devlun,er,listfl,whedr,inftrd,crtin,wjfren,filid,
#ccc       decod1,devok,devol,namdev,devlun,maw,sb,reastr,
#ccc       redfil,tcheck,wriout,wrifil
#ccc  argument list description:
#ccc     arguments: idev
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc
#ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
#
#       this routine handles all file transfer requests.
#
#       AUTHOR: Roger N. Clark
#       modified:       04-07-83     JAH  convert to ratfor
#ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

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
	include "../common/lundefs"
	include "../common/hptrm"
	include "../common/alphabet"
	include "../common/ioftyp"
	include "../common/overlys"
	include "../common/sp3pfeat"
	include "../common/tetfeat"
	include "../common/wavemarks"
	include "../common/spcolors"

# RED
	integer*4 startr    # function startr
	integer*4 ihchar    # function ihchar

	character*8     inamr, inamt
	character*80    ipn
	character*80    outline        # for X-window writes
	character*7     ovcolor
	character*7     wmcolor
	character*3     tbox3

	integer*2 ibit, chkbit
	logical			tapelu

#	data ihplus/'+'/
	ihplus = ihchar('+')

100	if (idev!=iht) {
	call devlun(4,idev,idev2)
		idev=idev2
		iftst=2
	} else idev=0
101	call eralph
	ird=0
	lin=0
	iauto=0
	iconl=0
	ictrl=0
	iconl=0
#
#     if the requested device is 0, go to file display and control
#     section, otherwise list titles of requested device on crt and, if
#     wanted, on printer
#
	if (idev==0) iconl= iht
	if (idev==0 | idv2==iht) goto 399
	call listfl (idev)
	if (idev==iht) go to 399
	goto 9800

#***************************************************************
#       main processing loop. clear screen and get input line
#***************************************************************

399	call eralph
	ixit= 0
	ictrl= 0
	i = 1
#
#     write instructions for file transfer and controls on crt display
#
	call whedr
	idv2=1
	call  inftrd (inftrn)

#	if (ovrflg(1) > 0 ) {
#		write (ttyout,*) "overlay1 enabled, channels=",ovrchn(1)
#		
#	}
#	if (ovrflg(2) > 0 ) {
#		write (ttyout,*) "overlay2 enabled, channels=",ovrchn(2)
#	}
#	if (ovrflg(3) > 0 ) {
#		write (ttyout,*) "overlay3 enabled, channels=",ovrchn(3)
#	}
#	if (ovrflg(4) > 0 ) {
#		write (ttyout,*) "overlay4 enabled, channels=",ovrchn(4)
#	}
#	if (ovrflg(5) > 0 ) {
#		write (ttyout,*) "overlay5 enabled, channels=",ovrchn(5)
#	}
#	if (ovrflg(6) > 0 ) {
#		write (ttyout,*) "overlay6 enabled, channels=",ovrchn(6)
#	}
#
#  write overlays defined to screen
#
	for (iov=1; iov<=6; iov=iov+1) {
		if (ovrflg(iov) > 0) {

                        if (iov == 1) ovcolor='red    '
                        if (iov == 2) ovcolor='blue   '
                        if (iov == 3) ovcolor='green  '
                        if (iov == 4) ovcolor='orange '
                        if (iov == 5) ovcolor='cyan   '
                        if (iov == 6) ovcolor='magenta'
			write (ttyout,1234) iov, 
				ovfil(iov), ovrec(iov), 
				ovwfil(iov), ovwrec(iov), 
				ovops(iov), ovcolor,
				ovtitle(iov), ovrchn(iov)
		}
	}
1234	format(1x,'ov', i1,'=',a1,i6,' ',a1,i6,' ',a2,' ',a,' ',a,i5)

#  write wavelength markers in use to screen
	for (iwm=1; iwm<=6; iwm=iwm+1) {
		if (wmrflg(iwm) > 0) {

                        if (iwm == 1) wmcolor='red    '
                        if (iwm == 2) wmcolor='blue   '
                        if (iwm == 3) wmcolor='green  '
                        if (iwm == 4) wmcolor='orange '
                        if (iwm == 5) wmcolor='cyan   '
                        if (iwm == 6) wmcolor='magenta'
			write (ttyout,1235) iwm, 
					wmops(iwm),
					wmcolor,
					wmrwav(1:wmnum(iwm),iwm)
		}
	}
1235	format(1x,'wm', i1,'=',a4,' ',a7,1x,6f12.5)

#  write 3-point band depths in use to screen
#  e.g.: bd1= 1.04um   W  0.958   0.986   1.02     1.05      1.080   1.110
#        bd2= 1.25um   W  1.12    1.16    1.25     1.29      1.34    1.38
	for (ipt=1; ipt<=imax3pt; ipt=ipt+1) {
		tbox3="   "
		if (spcbox(ipt) == 1) tbox3="box"
		if (sfmode(ipt) == 1) {        # channels

			write (ttyout, 1236) ipt, sfeatname(ipt),
				sleftchan(1,ipt), sleftchan(2,ipt),
				sctrchan(1,ipt),  sctrchan(2,ipt),
				srightchan(1,ipt), srightchan(2,ipt),
				tbox3
		}
		if (sfmode(ipt) == 2) {       # wavelengths
			write (ttyout, 1237) ipt, sfeatname(ipt),
				sleftwave(1,ipt), sleftwave(2,ipt),
				sctrwave(1,ipt),  sctrwave(2,ipt),
				srightwave(1,ipt), srightwave(2,ipt),
				tbox3
		}
	}
1236	format (1x, "bd",  i1,'=', a, "  C", 6(1x, i8),"  ", a)
1237	format (1x, "bd",  i1,'=', a, "  W", 6(1x, f8.4),"  ", a)

# tetracorder features:
# tf1= Clinochlore   f1a DLw 2.189   2.218   2.404   2.432 
# tf2= Clinochlore   f2a DLw 2.189   2.218   2.267   2.287 
# tf3= Clinochlore   f3a WLw 0.512   0.542   1.696   1.726 
# curved continuaa
# tf1= test1        f1a DCw 1.785 1.815  1.845 1.870  1.925 1.955  1.985 2.015

	for (itf=1; itf<=imaxtet; itf=itf+1) {

		if (tfmode(itf) > 0) {            # 0 means undefined

		    if (tfcont(itf) == 1) {            # linear
			write (ttyout,1238) itf,
				tfeatname(itf),
				tetfna(itf),
				tetfnc(itf),
				tleftwave(1,itf),
				tleftwave(2,itf),
				trightwave(1,itf),
				trightwave(2,itf),
				tbdepth(itf)

		    } else if (tfcont(itf) == 2) {              # curved
			write (ttyout,1239) itf,
				tfeatname(itf),
				tetfna(itf),
				tetfnc(itf),
				tleftcwave(1,itf),
				tleftcwave(2,itf),
				tleftwave(1,itf),
				tleftwave(2,itf),
				trightwave(1,itf),
				trightwave(2,itf),
				trightcwave(1,itf),
				trightcwave(2,itf),
				tbdepth(itf)
		    } else {

			write (ttyout,*) "Error: tetracorder continuum mode, not L or C"
			write (ttyout,*) "        tfcont(",itf,") = ", tfcont(itf)
			call what(0)
		    }
		}
	}
1238	format(1x,'tf', i1,'=',1x,a,1x, a4,1x, a4,
		f7.4,1x,f7.4,1x,f7.4,1x,f7.4,1x, f8.4)

1239	format(1x,'tf', i1,'=',1x,a,1x, a4,1x, a4,
		f7.4,1x,f7.4,1x,f7.4,1x,f7.4,1x,f7.4,
                  1x,f7.4,1x,f7.4,1x,f7.4,1x, f8.4)



#  finally read the user input:

	call crtin
	call wjfren(i,x,il)
	if (i>80) {
		call what(i)
		write (ttyout, 807)
		call crtin
		goto 399
	}
	if (il==ihx) goto 9700

#       check if overlay defined, form= ov1= v23 V22 color
#       added 12/22/2009
	#write (*,*) "DEBUG: overlay point 1"
	if (i < 73 && il == iho && iopcon(i:i)=='v') {
		iero = 0
		if(iopcon(i-1:i+2) == "ov1=" ) {
			#write (*,*) "DEBUG: overlay point 2 ov1="
			i=i+3
			itmpo=1
			call getoverly(i,itmpo,iero)
			#call crtin   # DEBUG
			go to 399   # go to beginning
		}
		if(iopcon(i-1:i+2) == "ov2=" ) {
			#write (*,*) "DEBUG: overlay point 2 ov2="
			i=i+3
			itmpo=2
			call getoverly(i,itmpo,iero)
			go to 399   # go to beginning
		}
		if(iopcon(i-1:i+2) == "ov3=" ) {
			i=i+3
			itmpo=3
			call getoverly(i,itmpo,iero)
			go to 399   # go to beginning
		}
		if(iopcon(i-1:i+2) == "ov4=" ) {
			i=i+3
			itmpo=4
			call getoverly(i,itmpo,iero)
			go to 399   # go to beginning
		}
		if(iopcon(i-1:i+2) == "ov5=" ) {
			i=i+3
			itmpo=5
			call getoverly(i,itmpo,iero)
			go to 399   # go to beginning
		}
		if(iopcon(i-1:i+2) == "ov6=" ) {
			i=i+3
			itmpo=6
			call getoverly(i,itmpo,iero)
			go to 399   # go to beginning
		}
		if (iero > 0) {
			call what (i)
			go to 399
		}

	}

#       check if wavelength marker defined, form= wm1= 1.234 2.31 4.56 ...
#       added 9/20/2013

	#write (*,*) "DEBUG: wavelength mark point 1"
	if (i < 73 && il == ihw && iopcon(i:i)=='m') {
		iero = 0
		if(iopcon(i-1:i+2) == "wm1=" ) {
			#write (*,*) "DEBUG: wavelength mark point 1 wm1="
			i=i+3
			itmpo=1
			call getwavmark(i,itmpo,iero)
			#call crtin   # DEBUG
			go to 399   # go to beginning
		}
		if(iopcon(i-1:i+2) == "wm2=" ) {
			#write (*,*) "DEBUG: wavelength mark point 2 wm2="
			i=i+3
			itmpo=2
			call getwavmark(i,itmpo,iero)
			go to 399   # go to beginning
		}
		if(iopcon(i-1:i+2) == "wm3=" ) {
			i=i+3
			itmpo=3
			call getwavmark(i,itmpo,iero)
			go to 399   # go to beginning
		}
		if(iopcon(i-1:i+2) == "wm4=" ) {
			i=i+3
			itmpo=4
			call getwavmark(i,itmpo,iero)
			go to 399   # go to beginning
		}
		if(iopcon(i-1:i+2) == "wm5=" ) {
			i=i+3
			itmpo=5
			call getwavmark(i,itmpo,iero)
			go to 399   # go to beginning
		}
		if(iopcon(i-1:i+2) == "wm6=" ) {
			i=i+3
			itmpo=6
			call getwavmark(i,itmpo,iero)
			go to 399   # go to beginning
		}
		if (iero > 0) {
			call what (i)
			go to 399
		}

	}

#	check if 3-point band depth calculaator defined:
#             label      left1   left2  center1  center2   right1  right2
#       bd1= 1.04um C|W  0.958   0.986   1.02     1.05      1.080   1.110 box
#       added 11/23/2018 - R. Clark

	#write (*,*) "DEBUG: 3-point band depth point 1"
	if (i < 73 && il == ihb && iopcon(i:i)=='d') {
		iero = 0
		if(iopcon(i-1:i+2) == "bd1=" ) {
			#write (*,*) "DEBUG: band depth bd1="
			i=i+3
			itmpo=1
			call get3ptbd(i,itmpo,iero)
			#call crtin   # DEBUG
			go to 399   # go to beginning
		}
		if(iopcon(i-1:i+2) == "bd2=" ) {
			#write (*,*) "DEBUG: band depth bd2="
			i=i+3
			itmpo=2
			call get3ptbd(i,itmpo,iero)
			#call crtin   # DEBUG
			go to 399   # go to beginning
		}
		if(iopcon(i-1:i+2) == "bd3=" ) {
			#write (*,*) "DEBUG: band depth bd3="
			i=i+3
			itmpo=3
			call get3ptbd(i,itmpo,iero)
			#call crtin   # DEBUG
			go to 399   # go to beginning
		}
		if(iopcon(i-1:i+2) == "bd4=" ) {
			#write (*,*) "DEBUG: band depth bd4="
			i=i+3
			itmpo=4
			call get3ptbd(i,itmpo,iero)
			#call crtin   # DEBUG
			go to 399   # go to beginning
		}
		if(iopcon(i-1:i+2) == "bd5=" ) {
			#write (*,*) "DEBUG: band depth bd5="
			i=i+3
			itmpo=5
			call get3ptbd(i,itmpo,iero)
			#call crtin   # DEBUG
			go to 399   # go to beginning
		}
		if(iopcon(i-1:i+2) == "bd6=" ) {
			#write (*,*) "DEBUG: band depth bd6="
			i=i+3
			itmpo=6
			call get3ptbd(i,itmpo,iero)
			#call crtin   # DEBUG
			go to 399   # go to beginning
		}
		if(iopcon(i-1:i+2) == "bd7=" ) {
			#write (*,*) "DEBUG: band depth bd7="
			i=i+3
			itmpo=7
			call get3ptbd(i,itmpo,iero)
			#call crtin   # DEBUG
			go to 399   # go to beginning
		}
		if(iopcon(i-1:i+2) == "bd8=" ) {
			#write (*,*) "DEBUG: band depth bd8="
			i=i+3
			itmpo=8
			call get3ptbd(i,itmpo,iero)
			#call crtin   # DEBUG
			go to 399   # go to beginning
		}
		if(iopcon(i-1:i+2) == "bd9=" ) {
			#write (*,*) "DEBUG: band depth bd9="
			i=i+3
			itmpo=9
			call get3ptbd(i,itmpo,iero)
			#call crtin   # DEBUG
			go to 399   # go to beginning
		}
	}

# tetracorder feature definitions

#	tf1= Snow.H2O f1a DLw 0.958   0.986   1.080   1.110  ct 0.08
#	tf2= Snow.H2O f2a DLw 1.150   1.178   1.315   1.345  ct 0.08 lct/rct> 0.9 1.1

# curved continuaa

# tf1= test1    f1a DCw 1.785 1.815  1.845 1.870  1.925 1.955  1.985 2.015



	#write (*,*) "DEBUG: checking for tetracorder feature definition"
	if (i < 73 && il == iht && iopcon(i:i)=='f') {
		iero = 0
		#write (*,*) "DEBUG: tetracorder tf found"
		if(iopcon(i-1:i+2) == "tf1=" ) {
			#write (*,*) "DEBUG: tetracorder feature tf1="
			i=i+3
			itmpo=1
			call gettetf(i,itmpo,iero)
			#call crtin   # DEBUG
			go to 399   # go to beginning
		}
		if(iopcon(i-1:i+2) == "tf2=" ) {
			#write (*,*) "DEBUG: tetracorder feature tf2="
			i=i+3
			itmpo=2
			call gettetf(i,itmpo,iero)
			#call crtin   # DEBUG
			go to 399   # go to beginning
		}
		if(iopcon(i-1:i+2) == "tf3=" ) {
			#write (*,*) "DEBUG: tetracorder feature tf3="
			i=i+3
			itmpo=3
			call gettetf(i,itmpo,iero)
			#call crtin   # DEBUG
			go to 399   # go to beginning
		}
		if(iopcon(i-1:i+2) == "tf4=" ) {
			#write (*,*) "DEBUG: tetracorder feature tf4="
			i=i+3
			itmpo=4
			call gettetf(i,itmpo,iero)
			#call crtin   # DEBUG
			go to 399   # go to beginning
		}
		if(iopcon(i-1:i+2) == "tf5=" ) {
			#write (*,*) "DEBUG: tetracorder feature tf5="
			i=i+3
			itmpo=5
			call gettetf(i,itmpo,iero)
			#call crtin   # DEBUG
			go to 399   # go to beginning
		}
		if(iopcon(i-1:i+2) == "tf6=" ) {
			#write (*,*) "DEBUG: tetracorder feature tf6="
			i=i+3
			itmpo=6
			call gettetf(i,itmpo,iero)
			#call crtin   # DEBUG
			go to 399   # go to beginning
		}
		if(iopcon(i-1:i+2) == "tf7=" ) {
			#write (*,*) "DEBUG: tetracorder feature tf7="
			i=i+3
			itmpo=7
			call gettetf(i,itmpo,iero)
			#call crtin   # DEBUG
			go to 399   # go to beginning
		}
		if(iopcon(i-1:i+2) == "tf8=" ) {
			#write (*,*) "DEBUG: tetracorder feature tf8="
			i=i+3
			itmpo=8
			call gettetf(i,itmpo,iero)
			#call crtin   # DEBUG
			go to 399   # go to beginning
		}
		if(iopcon(i-1:i+2) == "tf9=" ) {
			#write (*,*) "DEBUG: tetracorder feature tf9="
			i=i+3
			itmpo=9
			call gettetf(i,itmpo,iero)
			#call crtin   # DEBUG
			go to 399   # go to beginning
		}
	}


###############################################

	call wjfren(i,x,il2)
	if (il==ihi) {
		inftrn = 1
		if (il2==ihn) inftrn = 0
		goto 399
	}
	if(il==ihm & il2==0) {
		idev= ihm
		idv2=ihm
		inext = ihm
		return
	}
	if (il==ihl) {
		idev = il2
		goto 100
	}
#
#     iopcon= temporary working array for free format input.
#     ipn= storage array for free format input

	ipn = iopcon

	lic=1
	ipm= 10
	ipct= 0
	iov= 0
	ixv= -96
	iyv= 0
#
#     decode file id and record number
#
402	continue
	call filid(lic, iflnu, ifilid)
	
	if (iops(1:1)=='x') {
		il = ihx
		goto 9700
	}
	if (iflnu<=0) {
		write(ttyout, 805)
		call crtin
		goto 399
	}
	ixit= 0
	ir= 0
	icp= 0
	ict= 0
	ijix= 0
	iauto= 0
	ictrl= 0
	ntofil= 0
#
#     decode options
#
	do j= 1, 40 {
		itmp = ihchar(iops(j:j))

		if (itmp == ihc) {
			iauto= ihc

		} else if (itmp == iho) {
			ictrl= iho

		} else if (itmp == ihe) {
			ir= ihe

		} else if (itmp == ihp) {
			ipct= ihp

		} else if (itmp == ihx) {
			goto 9700

		} else if (itmp == iht) {
			ict= iht

		} else if (itmp == ihi) {
			ictrl=ihi

#
#     decode wavelength space type:
#
		} else if (itmp == iha | itmp == ihh | itmp == ihn) {
			itrol(3) = itmp
#
#     decode wavelength file. 
#

		} else if (itmp == ihcv | itmp == ihcw | itmp == ihcd | 
				itmp == ihcu | itmp == ihcy | itmp == ihcc) {
			itrol(1) = itmp
			iwfl = 0
			jj2 = j +1

			while (jj2<=40 & iops(jj2:jj2)>='0'
			    & iops(jj2:jj2)<='9'  ) {
				call decod1 (iops(jj2:jj2),niwfl)
				iwfl= iwfl*10+ niwfl
				jj2= jj2+1
			}
			itrol(2) = iwfl
#
#     set auto-wavelength control
#
		} else if (itmp == ihandp) {
			itrol(1) = ihandp
#
#

		} else if (itmp == ihca) {		# capital A = auto scale
#				          iauto = "c" no auto scale
#				          iauto = "a" autoscale
#				          iauto = "B" autoscale with min=0
#				          iauto = "A" autoscale +2% range
			iauto = iha
			inumauto=-1
			call decod1(iops(j+1:j+1),inumauto)
			if (inumauto == 0) iauto = ihcb
			if (inumauto == 2) iauto = ihca

		} else if (itmp == ihcl) {		# capital L = select line color
			spcolor=0  # default to black if a number not found
			call decod1(iops(j+1:j+1),spcolor)

		} else if (itmp == ihl) {
			call decod1(iops(j+1:j+1),iline)

		} else {
			xjunk =0 # filler
		}
	}
	if ((ict!=iht) & (iauto!=iha) & (iauto!=ihcb) & (iauto!=ihca)) iauto= ihc
	if (ictrl==iho) iov= iov+ 1
	if (ictrl!=iho) iov= 0
	if((ictrl==iho) & (iov==1)) call er

	for (j=1; j<=40; j=j+1)
		if (iops(j:j)=='+') break

	if (j!=11) {
		j= j+ 1
		ntofil= 0
#
#     for successive file requests, determine the number of successive
#     files requested (=ntofil)
#
		icp= ihplus
		ifiop= 0
		jtmp = j
		do j= jtmp, 40 {
			if (iops(j:j)==' ') next
			if ((iops(j:j)<'0') | (iops(j:j)>'9')) break
			call decod1 (iops(j:j),ifiop)
			ntofil= ntofil*10 + ifiop
		}
	}
	if ((iauto==iha) & (ipct==ihp)) iauto= ihb
	ifiln=0
	if (ict==iht) call filid (lic, ifiln, ifildt)
	idev= 0
#
#     decode device number
#
	inames=0
	idv1= ifilid
	call devok (0,ifilid,iflnu,idev,ier)
	if (ier!=0) {
		write (ttyout, 807)
		call crtin
		goto 399
	}
	call devol (idev,ivolr)
	call namdev (ifilid,inamr)
	if (ict!=iht) go to 699
	call devlun (0,ifildt,lunt)
	call devsta (lunt,ista,0,iprt)
	if (ista<=0) {
		write (ttyout, 807)
		call crtin
		goto 399
	}
	if ((iprt >= 0) & (ifiln == 0) & (ict==iht)) {
		ifiln = iprt +1            # set record number to protection +1
	}
	if ((ifiln<=0) & (ict==iht)) {
		write(ttyout, 806) ifiln
		call crtin
		go to 399
	}
	if(icp==ihplus)  ifiln= ifiln- 1
	call devol (lunt,ivolt)
	call namdev (ifildt,inamt)
#
#     if the file names are equal and the input device (file) is different
#     from the device (file) to transfer to, set inames = 1.
#
	if (  (ifilid!=ifildt)  &
	   (inamr==inamt) ) inames=1


699	if (ict==iht) write(ttyout, 700)
	ntofil = iflnu+ ntofil
	iln= 0
#
#     do the successive file requests from number iflnu to ntofil
#
	jtmp = iflnu
	do iflnu= jtmp, ntofil {

#  KEL  do loop is not terminating properly: Linux gfortran 4.4 (Intel)
#  KEL  (iflnu is incremented within loop by specpr code)
#  KEL  so next if statement is required to trap run-away code
		if (iflnu > ntofil) goto 901

		iln= iln+ 1
		idad = 1
		if (ictrl==iho) {
			if((iov==1) & (ictrl==iho)) {
				write(outline, 461)
				call gwrite(outline)
			}
			if (iov==1) iyv= 740*2
			ixv= ixv+ 96*2
			if (ixv>900*2) iyv= iyv- 40*2
			if (ixv>900*2) ixv= 0
			iov= iov+ 1
#
#     position cursor to write file id's and numbers on crt display in
#     overlap routine
#
			call maw (ixv, iyv)
			call sb(0)
			write(outline, 462) ifilid, iflnu
			call gwrite(outline)
			if (iflnu==ntofil) {
				write(outline, 463)
				call gwrite(outline)
			}
		}
		if (idev==17) {
#
#     read a starpack from disk, put the data in label 1 common and
#     write the starpack into 3 specpr data records.
#
			if (ict!=iht) go to 399
			ifx= ifilex
#
#     read in starpack
#
			call reastr (ier, iflnu)
			if (ier!=0) {
				write (ttyout, 807)
				call crtin
				goto 399
			}
			ifilex= ifx
			mhist = mhista
			ihist = ihista
			ititl = ititl1
			do i= 1, maxchn {
				data(i)= dataa(i)
				if (i>3) next
				ira(i)=iraa(i)
				idec(i)=ideca(i)
			}

			revs= revs1
			itimch=itmcha
			nruns=nruna
			iwtrns=iwtrna
			xnrm=xnrma
			scatim=sctma
			timint=tminta
			irmas= irmasa
			ijix= 1
			go to 204

735			mhist = mhistb
			ihist = ihistb
			ititl = ititl2
			do i= 1, maxchn {
				data(i)=datab(i)
				if (i>3) next
				idec(i)=idecb(i)
				ira(i)=irab(i)
			}
			irmas= irmasb
			iwtrns=iwtrnb
			xnrm=xnrmb
			ijix= 2
			if (icp!=ihplus) ifiln= ifiln+ 1
			go to 204

712			ihist= ihistc
			ititl= ititle
			do i= 1, maxchn {
				data(i)= datac(i)
			}
			irmas= 0
			ijix= 3
			if (icp!=ihplus) ifiln= ifiln+ 1
			go to 204
		}
		ifptr = ftptr(idev)  # get pointer so we can check file type
#
#               if user selected errors included (ir = ihe) and
#                       the file is not 3d type (=3) then read in errors
#                       (if it is a 3d file and block extraction is done,
#                       then errors automatically computed.
#
		if(ir==ihe & filtyp(1,ifptr) != 3) {
#
#     read in errors
#
			ifiu = iflnu
			call rederr (ifiu, idev, iftst)

			if (iftst!=0) {
				write (ttyout, 807)
				call crtin
				goto 399
			}
			do k= 1, maxchn
				error(k) = data(k)

			idad =2
		}
#
#     read in data
#
		ifl1 = iflnu
		call redfil (iflnu, idev, iftst)

		if (iftst!=0) break
		do k= 1, maxchn
			datac(k) = data(k)


		idv1=ifilid


		if (ipct==ihp | iauto==ihc | iauto==iha | iauto==ihca | iauto==ihcb | ictrl==iho) {
#
#     plot data on crt
#
			if (igrmod == 50 | igrmod == 51) {
				call eralph  # X-windows case
			}
			call wriout
			iopcon=ipn
			if (ictrl==ihx | ixit==ihx) go to 399
		}
		if (ict!=iht) next
204		if (icp==ihplus) ifiln= ifiln+ 1
#
#     check for illegal file transfers.
#
		igo=0
		call tcheck (igo,ifilid,ifildt,ifiln,iflnu,
			iln,lunt,inames)
		if (igo==399) go to 399
		ird= 0
		if (ifiln<=0) go to 402
		if (ir==ihe) ird= 1
		ibit = 1           # if text mode, do not set record number
		if (chkbit(icflag,ibit) == 0) {
			filno= ifiln
		}
		if (lunt==17) {
#
#     read in starpack from tape and put data in unlabeled common to
#     write a starpack to disk.
			inext = startr(idev,ifiln,iflnu)
			if (inext!=0) goto 399
			next
		}
#
#     write a specpr data file
#
155		call wrifil  ( ifiln, lunt, ierr)
		if (ierr!=2) {
			write (ttyout, 807)
			call crtin
			goto 399
		}
# RED - moved comments to own line
			# starpack part 1 complete
		if (ijix==1) go to 735
			# starpack part 2 complete
		if (ijix==2) go to 712
			# starpack part 3 complete
		if (ijix==3) next 
		if((ierr==2)&(ir==ihe)) {
#
#     if involved, write errors in data file following the regular data
#     file
#
			do j= 1,maxchn {
# RED
#				if (ird==0) next 2
				if (ird==0) go to 900
				data(j) = error(j)
			}
			ibit = 1     # if text mode, do not set record number
			if (chkbit(icflag,ibit) == 0) {
				filno= ifiln
			}
			write(ititl,185) ifiln
			ird= 0  # after the next write, don't do errors again

#                       check protection

			call devsta (lunt,ista,0,iprt)
			if (ista<=0) {
				write (ttyout, 807)
				call crtin
				goto 399
			}
			if ((iprt >= 0) & (ifiln == 0) & (ict==iht)) {
				ifiln = iprt +1      # set record number
							#  to protection +1
			} else {
				ifiln = ifiln + 1  # normal record increment
			}
			if ((ifiln<=0) & (ict==iht)) {
				write(ttyout, 806) ifiln
				call crtin
				go to 399
			}

			go to 155  #go do the write of error bars.
		}
900     continue
	}

#KEL  jump out of do loop
901	continue
	idev = 0

#
#       force close on output file (to write EOF on mt)
#       then backspace over eof mark.
#
	if (lunt!=0) write(ttyout,800)
	if (tapelu(lunt)) {
		call closnr(lunt)
		call bkrec(lunt,1,ier)
	}
	lunt = 0
	if (tapelu(ifiu) & iftst==-1) {		#eof on tape input
		call bkrec(ifiu,1,ier)
	}
	if (lic<80) go to 402
	go to 399

9800    il=idev
9700	idev=il
	return

184     format(a)
185     format('errors to previous data', i6, 8(1h ))
461     format ( 20x, '***overlap routine***')
462     format (1x,a1,i5,',')
463     format (1x, 'last rec')
700     format (' record transfer (copy) in progress')
800		format (/)
805     format (' ***illegal record number***', //,
		' press return  to continue')
806     format (' illegal record number (',i7,
		') in a transfer specification', //,
		' press return  to continue')
807	format (' ERROR: press return to exit')
	end

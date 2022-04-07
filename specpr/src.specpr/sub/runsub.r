subroutine runsub(ic,cx,iprodp)
implicit integer*4	(i-n)	
	
	include "../common/spmaxes"   # max parameters, must be first

	include "../common/blank"
	include "../common/lbl6"
	include "../common/lbl4"
	include "../common/label1"
	include "../common/labl2"
	include "../common/lbl3"
	include "../common/label3"
	include "../common/labelf"
	include "../common/lundefs"
	include "../common/alphabet"

character*6 itt
character*1 idummy

#
#     this subroutine subtracts two files, decodes the right ascension
#     and declination from the title and then calculates the equivalent
#     air mass at which the observation was taken
#
jhn = 0
call eralph
call whedr2
write(ttyout,60)
write(ttyout,70)ititl1,revs1,idv1,ifl1
write(ttyout,80)
if (idv2==ihc) {
    jhn = ihn
    write(ttyout,110)cx
    write(ititl2(14:40),32)cx
} else {
    write(ttyout,90)ititl2,revs2,idv2,ifl2
    if (revs1!=revs2) write(ttyout,100)
}
if (iprodp!=1) {
    write(ttyout,120)
    call crtin
    ii = 1
    call wjfren(ii,xx,ic)
}
if (ic!=ihx) {
    if (ic==ihe) write(ttyout,130)
    if (ic!=ihe) {
#
#     subtract two files
#
		call runsb1
		pi2 = 0.62831853d+01
		pihaf = 0.15707963d+01
		iradc = 0
		ist = 0
		do j = 1,10 {
			if (jhn==ihn || iops(j:j)=='n') go to 40
			if (iops(j:j)=='r') iradc = 1
			if (iops(j:j)=='t') ist = 1
		}
		if (ictrl==ihe) {
			write(ttyout,140)
			ictrl = 0
		}
		if (iradc!=1) {
#
#     decode ra and dec
#
			call radec(ititl1,ih,im,is,jd,jm,js)
			if (ih!=25) go to 10
		}
		repeat {
			write(ttyout,150)
			call crtin
			i = 1
			call wjfren(i,x,il)
			if (il==ihx) go to 30
			if (il==ihe) go to 50
			if (il==0) {
				ih = x
				call wjfren(i,x,il)
				if (il==0) {
					im = x
					call wjfren(i,x,il)
					if (il==0) {
						is = x
						isav = i
						call getstr(i,idummy,ierr)
						if (ierr!=2) {
							isign = 1
							if (idummy=='-') isign = -1
							else i = isav
							call wjfren(i,x,il)
							if (il==0) {
								jd = x*isign
								call wjfren(i,x,il)
								if (il==0) {
									jm = x*isign
									call wjfren(i,x,il)
									if (il==0) {
										js = x*isign
10										xih = ih
										xim = im
										xis = is
										ra = xih*3600.+xim*60.+xis
										ira(1) = ih
										ira(2) = im
										ira(3) = is
										ra = (ra/3600.)*0.2617994
										xjd = jd
										xjm = jm
										xjs = js
										dec = xjd*3600.+xjm*60.+xjs
										idec(1) = jd
										idec(2) = jm
										idec(3) = js
										dec = (dec/3600.)*0.01745329
										if (ra>0 && ra<=pi2 &&
											dec>=-pihaf &&
											dec<=pihaf) break 1
									}
								}
							}
						}
					}
				}
			}
		}
		if (ist!=1) {
#
#     decode the siderial time
#
			write(itt,121)stb
			its = 6
			do i = 1,6
				if (itt(i:i)<'0' || itt(i:i)>'9') its = its-1
			if (its==6) {
				read(itt,105)ith,itm,its
				go to 20
			}
		}
		repeat {
			write(ttyout,160)stb
			call crtin
			i = 1
			call wjfren(i,x,il)
			if (il==ihx) go to 30
			if (il==ihe) go to 50
			if (il==0) {
				ith = x
				call wjfren(i,x,il)
				if (il==0) {
					itm = x
					call wjfren(i,x,il)
					if (il==0) {
						its = x
#
#
#     this section changes the siderial time at the request of the
#     operator by specifying a  t  in the options specification.
#
#
20              	    isid = 0
						ist = 0
						if (ist!=0) {
							isid = -52
							im = itm
							ihh = ith
							imb = itm
							ihb = ith
							im = im+isid
							imb = imb+isid
							repeat {
								if (im<0) ihh = ihh-1
								if (im<0) im = im+60
							} until(im>=0)
							repeat {
								if (im>59) ihh = ihh+1
								if (im>59) im = im-60
							} until(im<=59)
							repeat {
								if (imb<0) ihb = ihb-1
								if (imb<0) imb = imb+60
							} until(imb>=0)
							if (imb>59) {
								ihb = ihb+1
								imb = imb-60
							}
							write(sta(1),2181)ihh
							write(stb(1),2181)ihb
							write(stb(2),2181)imb
							write(sta(2),2181)im
							ith = ihh
							itm = im
							write(ttyout,170)isid
							call crtin
							if (iopcon(1:1)=='t') {
								repeat {
									write(ttyout,180)
									call crtin
									i = 1
									call wjfren(i,x,il)
								} until(il==0)
								isid = x
							}
						}
						xith = ith
						xitm = itm
						xits = its
						sid = xith*3600.+xitm*60.+xits
#
#     0.2617993878= pi *2./ 24.
#
						sid = (sid/3600.)*0.2617993878
						if (sid>=0 && sid<=pi2) break 1
					}
				}
			}
		}
#
#     calculate the air mass
#
		airmas = sin(alat)*sin(dec)+cos(alat)*cos(dec)*cos(sid-ra)
		if (airmas>100000.) airmas = 100000.
		if (airmas<0.000001) airmas = 0.000001
		airmas = 1.0/airmas
		x = airmas
		airmas = x-0.001*(x**3-x)+0.72e-05*(x**5-x)
		if (airmas<0.0001) airmas = 0.0001
		if (airmas>32.0) airmas = 32.0
#
#     computed airmas function good to a zenith angle of 84 degrees.
#     at 84 deg: airmas=9.3 but should be 8.9
#
		ha = sid-ra
		irmas = airmas*1000.
		write(ttyout,190)ih,im,is,jd,jm,js,airmas,ith,itm,its
		go to 40
30      ic = ihx
		return
#
#     decode a title and recomend it as the the title for the
#     subtracted data. this is option p in the main routine where
#     the user selects a title
#
40      i = 0
		ierr = 0
		if (ictrl==ihe) call ercomp(i,ierr,cx)
		if (ierr!=ihe) {
			ititle = ititl1(15:26) // ' -' // ititl2(15:40)
			j = 0
			ititl1 = ititle
			call comprs(ititl1)
#
#     decode history
#
			call whistr("-",cx)
			return
		}
50      ic = ihe
    }
}
return

32  format('const',f15.7)

60  format(30x,'subtraction routine',/)

70  format(10x,'form:',//,2x,a,2x,'no.of revs=',f5.0,2x,'file:',1x,a,i5)

80  format(19x,5('*'),/,19x,'minus',/,19x,5('*'))

90  format(2x,a,2x,'no.of revs=',f5.0,2x,'file:',1x,a,i5,/)

100  format(1x,'note: the number of revolutions of each data run are not equal')

110  format(2x,'the constant',f15 .7)

120  format(10x,'press return to continue or type  e  to exit routine.')

130  format(20x,'routine aborted',//)

140  format(1x,'cannot include errors in this airmasscomputation mode')

150  format(1x,'type in right ascension and declination.',' or e or x to exit',/)

121  format (3a2)

160  format(1x,'sid time=',3(a,1x),5x,'type in sid time: it is not correct. or e or x to exit.',/)

105  format (3i2)

2181 format(i2)

170  format(1x,79('*'),/,1x,'note:',/,5x,'the siderial time isbeing changed an amount of',i7,1x,'minutes',/,5x,
  'type  t  to change amount')

180  format(1x,'type in error in siderial time in minutes')

190  format(1x,'ra=',3i3,2x,'dec=',3i3,2x,'airmas=',f7 .3,5x,'sid time=',2(i2,':'),i2)


end

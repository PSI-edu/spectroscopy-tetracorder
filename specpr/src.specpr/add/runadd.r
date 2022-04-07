		subroutine runads(id)
		implicit integer*4(i-n)
#ccc  version date: 06/01/83
#ccc  author(s): roger clark & jeff hoover
#ccc  language:  fortran
#ccc
#ccc  short description:
#ccc         this subroutine adds files
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc          er,hreset,whedr,crtin,wjfren,finfil,dlterr,
#ccc          dstev,stdev
#ccc  argument list description:
#ccc     arguments:id
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

	include "../common/lundefs"
	include "../common/blank"
	include "../common/lbl6"
	include "../common/lbl4"
	include "../common/label1"
	include "../common/labl2"
	include "../common/lbl3"
	include "../common/label3"
	include "../common/labelf"
	include "../common/lblg"
	include "../common/alphabet"
	include "../common/dscrch"

#RED 
	integer*4 manhst    #   function manhst

	character*4	isvg
	character*6	inm
	logical*4	lopen
	integer*4	rcl

	integer*4 ier

	integer*4       idsum(128),ifels(128)
	integer*4       inumd(SPMAXCHAN),inume(SPMAXCHAN)
	real*4		error2(SPMAXCHAN)
	equivalence (inumd,datsc1),(inume,datsc2),(error2,datsc3)

#	data    isp/'  '/
	isp = ihchar(' ')

	idltz = 0
	idstdv = 0
	idltfg = 0
	constx = 0.0
	ilimit = 0
	call hreset(1)
	call whedr
	write(ttyout,70)
	do j = 1,30 {
		idsum(j) = isp
		ifels(j) = isp
	}
	do i = 1, SPMAXCHAN {
		inume(i) = 0
		error2(i) = 0.0
		inumd(i) = 0
		error(i) = 0.0
		dataa(i) = 0.0
		datab(i) = 0.0
		datac(i) = 0.0
	}
	isumav = 0
	do i = 1,10 {
		if (iops(i:i)=='a')
			isumav = iha
		if (iops(i:i)=='s')
			isumav = ihs
	}
#
# check that the file is opened properly
#
	inquire (addlun,OPENED=lopen,RECL=rcl,IOSTAT=ier)
	if (ier != 0) {
		write (ttyout,*) 'INQUIRE ERROR on unit addlun (',
					addlun,')=',ier
	}
	irecl=maxchn*4
	if ((rcl != irecl & lopen) | (.not. lopen)) {
		write (ttyout,*) 'addlun: unexpected rocerd size on scratch file, reopening with recl=', irecl
		close (addlun)
		open(addlun,status='scratch',access='direct',
			recl=irecl,form='unformatted',iostat=ier)
		if (ier != 0) {
			write (ttyout,*) 'ERROR opening add scratch file'
			write (ttyout,*) '      open error:', ier
			write (ttyout,*) 'press return to exit'
			call crtin
			id = ihx
			return
		}
	}

	if (isumav==0) {
5			write(ttyout,130)
			call crtin
			j = 1
			isumav = 0
			call wjfren(j,x,il)
			if (j>=80) {
				call what(j)
				go to 5
			}
			if (il == ihe || il == ihx) {
				id=ihe
				return
			}
			if (il != iha && il != ihs) {
				call what(j)
				go to 5
			}
			if (il == iha) isumav = iha
			if (il==ihs) isumav = ihs
	}
	if (isumav == 0) go to 5

# delete zeros and or set limits
6	if (ictrl == ihe) {
		write(ttyout,132)
	} else {
		write(ttyout,131)
	}
	call crtin
	j = 1
	call wjfren(j,x,il)
	if (j>=80) {
		# call what(j)		# This line seemed wrong.  deleted, 11/7/94, NsG
		go to 12       # skip these options
	}
	if (il == ihe || il == ihx) {
		id=ihe
		return
	}
	if (il==ihz) idltz = 1
	if (ictrl != ihe) {
		if (il==ihv) ilimit = 1
		if (il==ihs) idstdv = 1
	}
	if (il != ihz && il != ihv && il != ihs) {
		write(ttyout,*) 'ERROR: You must enter z, v, s, or nothing'
		call what(j)
		go to 6
	}
	call wjfren(j,x,il)                # get a second option
	if (il == ihe || il == ihx) {
		id=ihe
		return
	}
	if (il==ihz) idltz = 1
	if (ictrl != ihe) {
		if (il==ihv) ilimit = 1
		if (il==ihs) idstdv = 1
	}

	if (il != ihz && il != ihv && il != ihs && il !=0) {
		call what(j)
		go to 6
	}

	call wjfren(j,x,il)                # get a third option
	if (il == ihe || il == ihx) {
		id=ihe
		return
	}
	if (il==ihz) idltz = 1
	if (ictrl != ihe) {
		if (il==ihv) ilimit = 1
		if (il==ihs) idstdv = 1
	}

	if (il != ihz && il != ihv && il != ihs && il !=0) {
		call what(j)
		go to 6
	}

# get limits if limit flag set
	if (ilimit==1) {
		repeat {
			write(ttyout,140)
			call crtin
			j = 1
			call wjfren(j,x,il)
			xlolim = x
			call wjfren(j,x,il)
			if (j<80) {
				hilim = x
				if (xlolim<hilim)
					break 1
			}
		}
		write(ttyout,240)xlolim,hilim
	}	
	if (idstdv == 1) {
10		write(ttyout,180)
		call crtin
		j = 1
		call wjfren(j,d,id)
		if (id==ihe)
			return
		if (id==ihx)
			return
		if (d < 1.0) {
			write (ttyout,*) d, 'is TOO SMALL'
			go to 10
		}
		deviat = d
	}

# including existing error bars
12	if (ictrl==ihe) {
		idad = 2
		write(ttyout,120)
	}

20  write(ttyout,80)
	id = 0
	mab = 0
	totint = 0.0
	tnruns = 0.0
	twtrns = 0.0
	airmst = 0.0
	xnrmt = 0.0
	revst = 0
#
#     read data into the array adat. the file id's are stored in idsum
#     and the record numbers in ifels
#
	jjj = 0
	do jj = 1,32767 {
		jjj = jjj +1
		if (jjj == 128) break
		repeat {
			call crtin
			idelet = 0
			ier = 2
			is = 0
			iji = 1
			ixr = 0
#
#     decode file id
#
			call wjfren(iji,d,id)
			if (id==ihe)
				return
			if (id==ihx)
				return
			if (id==ihb)
				go to 900
			if (id!=ihc) {
				if (id==ihr) {
					mab = mab-1
					if (mab<0)
						mab = 0
					jjj = jjj-2
					id = idsum(jjj)
					totint = totint-timint
					tnruns = tnruns-nruns
					twtrns = twtrns-iwtrns
					xnrmt = xnrmt-xnrm
					airmas = airmas-irmas
					revst = revst-revs
					ixr = 1
					if (id==ihc)
						break 1
				}
				call devlun(4,id,lun)
				call devsta(lun,ista,0,iprt)
				if (ista<=0) {
					write(ttyout,150)
					next 1
				} else if (ixr==1)
					go to 30
			}
			if (id==ihc)
				is = -1
			if (id!=0) {
				if (lun!=0) {
#
#     decode record number
#
					call wjfren(iji,d,ij)
					if (iji<80) {
						if (ij==ihe)
							id = ihe
						if (ij==ihx)
							id = ihx
						if (ij==ihe||ij==ihx)
							return
						if (ij==ihd)
							idelet = 1
						if (ij==ihd)
							ij = 0
						if (ij==0) {
							ifel = d
							if (ifel>iabs(iprt)&&iprt!=-1 && is != -1 && d < 1.0) {
								write(ttyout,170)
								next 1
							} else {
								idsum(jjj) = id
								ifels(jjj) = ifel
								id = is
								if (idelet==0)
									call wjfren(iji,x,ij)
								if (ij==ihd)
									idelet = 1
#
#     find and read the requested file
#
#     if is = 1 then constant
#
								if (is!=-1)
									go to 30
								do i = 1, SPMAXCHAN {
									dataa(i) = d
									data(i) = d
								}
								constx = d
								xnrm = 1.0
								irmas = 0.0
								revs = 0
								timint = 0.0
								nrums = 0
								iwtrns = 0
								write(ititl,160)d
								revs = 0
								ifel = 0
								ifels(jjj) = 0
								go to 40
							}
						}	
					}
				}
			}
			write(ttyout,150)
			next 1
#
#     find the requested file
#
30			call finfil(ifel,lun,1,ier)
			if (ier != 0) {
				write(ttyout,90)
				call crtin
				id = ihx
				return
			}
			if (ixr==1)
				go to 60
40			revst = revst+revs
#
#     delete requested channels
#
			if (idelet==1)
				call dlterr(iji,nchans,idltz,is,inume,data)
			if (ilimit==1)
				do i = 1,nchans
					if (data(i)<xlolim||data(i)>hilim) {
						write(ttyout,190)i,data(i)
						data(i) = -1.23e34
					}
			if (idad==2)
				do i = 1,nchans {
#
#     the data array contains spectral data; error values not yet read in
#                       idltz=1 means delete zeros.
#
					if (idltz==1&&data(i)!=0.0&&data(i)!=-1.23e34)
						inume(i) = inume(i)+1
					if (idltz!=1&&data(i)!=-1.23e34)
						inume(i) = inume(i)+1
					if (idltz==1&&data(i)==0.0||data(i)==(-1.23e34))
						data(i) = -1.23e34
				}
			totint = totint+timint
			tnruns = tnruns+nruns
			twtrns = twtrns+iwtrns
			xnrmt = xnrmt+xnrm
			airmst = airmst+irmas
			ititl1 = ititl
#
#     write data values to scratch file, record = mab*2-1 (rec=1,3,5,...)
#           (errors will go into 2,4,6,...)
#
			mab = mab+1
			mdr = mab*2 -1
			write(addlun,rec = mdr,iostat = ier)data
			call ertyp('runadd',addlun,ier)
#
#     if idad = 2 get error values
#
			if (idad!=2) {
#                               fill error record, so end of file advances:
#                                    even though they are not used in this mode
				write(addlun,rec = mdr+1,iostat = ier)data
				call ertyp('runadd',addlun,ier)
				go to 60
			}
#
#     if constant dont read errors, they are zero (0)
#
			if (is==(-1))
				go to 50
#
#     read in the existing error bars if idad = 2
#
			ifel1 = ifel+1
			if (idad==2) {
				call finfil(ifel1,lun,2,ier)
				if (ier!=0) {
					write(ttyout,90)
					id = ihx
					call crtin
					return
				}
			}
			if (ier==0) go to 50
		}
		write(ttyout,200)jjj,constx
		next 1
#
#               for constant, set error bars to zero
#
50		if (is == (-1)) {
			do itmp = 1, nchans {
				data(itmp) = 0.0
			}
		}
#
#     put error value in scratch file at record mdr+1 (right after data values)
#
		write(addlun,rec = mdr+1,iostat = ier)data
		call ertyp('runadd',addlun,ier)
#
60		write(ttyout,110)jjj,ititl1,revs,ifel
	}
900   continue
##### now starts the computation loop
	repeat { # this is for doing deletion outside limits.
#
#     begin the analysis of the data,
#
		write(ttyout,100)
		revs = revst
		iwtrns = twtrns
		timint = totint
		nruns = tnruns
		js = mab
		xn = js
		if (xnrmt==0)
			xnrmt = xn
		if (xn<0.1e-36)
			xn = 0.1e+36
		xnrm = xnrmt/xn
		irmas = airmst/xn+0.5
#
#     initialize arrays
#
		do i = 1, SPMAXCHAN {
			datab(i) = 0.0
			dataa(i) = 0.0
			datac(i) = 0.0
			inumd(i) = 0.0
		}
#
#     find the average or sum
#
		do j = 1,js {
			mdr = j*2 -1
			read(addlun,rec = mdr,iostat = ier)data
			call ertyp('runadd',addlun,ier)
#
#     note datab array is used for the sum variable
#
			do jsum = 1,nchans {
				if (idltz!=1||data(jsum)!=0.0&&data(jsum)!=-1.23e34) {
					if (data(jsum)!=-1.23e34) {
						datab(jsum) = datab(jsum)+data(jsum)
						inumd(jsum) = inumd(jsum)+1
					}
				}
			}
#
#       and errors if individual errors included
#
			if (idad==2) {
				read(addlun,rec = mdr+1,iostat = ier)dataa
				call ertyp('runadd',addlun,ier)
				do iii = 1,nchans
					if (idltz!=1||data(iii)!=0.0&&data(iii)!=-1.23e34)
						if (data(iii)!=-1.23e34)
							error2(iii) = error2(iii)+dataa(iii)*dataa(iii)
			}
		}
#### data and error bars (if idad==2) have now been summed
		if (idad==2) {
			do iii = 1,nchans {
				if (inumd(iii)!=0) {
					error(iii) = sqrt(error2(iii))/float(inumd(iii))
				} else {
					error(iii) = 0.0
				}
			}
		}
#
#     find the average.
#
		do i = 1,nchans {
			sum = datab(i)
			xn = float(inumd(i))
			if (xn<=0.1e-36)
				sum = -1.23e34
			else
				sum = sum/xn
			datac(i) = sum
		}
#
#     find the standard deviation if errors not included
#
		if (idad != 2) {
			call stdev(isumav,idltz,inumd,error2,js)
			if (idstdv!=1) break 1
			if (idltfg==1) break 1
			call dstdev(js,nchans,error2,datac,idltfg,
					idsum,ifels,inumd,deviat)
		} else {
			break 1
		}
	} # end of repeat 
#
#     load datac array with data that the user specified (sum or ave.)
#
	if (isumav==ihs)
		do i = 1,nchans {
			xn = float(inumd(i))
			if (xn<=0.1e-36) xn = 1
			datac(i) = datac(i)*xn
			if (idad == 2) {
				error(i) = error(i)*xn
			}
		}
	ititl2 = ititl1
#
#     determine history
#
	if (isumav==ihs)
		isvg = 'sum:'
	if (isumav==iha)
		isvg = 'avg:'
	if (idstdv==1)
		write(ihist,210)isvg,js,deviat
	if (idstdv!=1)
		write(ihist,220)isvg,js
	if (isumav==ihs)
		inm = 'sum   '
	if (isumav!=ihs)
		inm = 'avg   '
	iflag = 0
	intchar = manhst(inm,6,idsum,ifels,js,mhist,iflag,constx)
	if (intchar<=40) {
		write(ihist,230)isvg,js,mhist(5:40)
		mhist = ' '
	}
	idad = 2
	ictrl = 2
#	call eralph
	return

70  format('                  ADDITION routine',/)

80  format (' ',
 'A total of 128 data sets may be ADDED or SUMMED.',//,
' TYPE in the filid and the record number',/,
' when  continue  is printed, type in the ',
			'next file id and record number.',/,
' when all files are entered, type  b  to ',
			'BEGIN ANALYSIS on these data,',/,
' or TYPE  e  or  x   to EXIT routine.',//,' continue',/)

90  format(' ERROR: data NOT READ, press return to exit',/)

100  format(/,' BEGIN ANALYSIS')

110  format(i4,'.',4x,a,2x,'revs=',i5,2x,'file',i5,/,' continue')

120  format(' ** note: individual errors bars from ',
		'each spectrum will be included',/,
		'    in the analysis',/)

130  format( ' Type  s  to SUM data or',/,
'       a  to AVERAGE')
131  format(' Type  z  to DELETE ZEROS or',/,
'       v  to DELETE POINTS outside a certain DATA VALUE LIMIT',/,
'       s  to DELETE POINTS outside a STANDARD DEVIATION LIMIT',/,
'          Limits will be prompted separately,',/,
'          all 3 options are possible.',/,
' Press return to ignore these options.')

132  format(' Type  z  to DELETE ZEROS or',/,
' Press return to ignore this option.')

140  format(' TYPE LOWER and UPPER bounds of ACCEPTABLE DATA.',/)

150  format('          INCORRECT ENTRY:  retype.',/,1x,72('-'),/)

160  format(' constant',f15 .7)

170  format(' ILLEGAL RECORD NUMBER. reenter',/)

180  format(' TYPE the number of standard deviations ',
		'(1.0-5.0) to DELETE',/,
		'      points OUTSIDE this LIMIT, ',/,
		' press RETURN TO CONTINUE (no standard ',
		'deviation limit)',/)

190  format(' channel ',i3,'  data= ',f13 .6,'  OUTSIDE LIMITS!')

200  format(i4,'.','   constant-',f12 .6)

210  format(a4,i4,'spectra,',f4.2,' sig limit,see mhist for details')

220  format(a4,i4,' spectra, see manual history for details')

230  format(a4,i4,' spectra,',a)

240  format(' DELETING POINTS OUTSIDE ',f13 .6,' and ',f13 .6,/)

	end

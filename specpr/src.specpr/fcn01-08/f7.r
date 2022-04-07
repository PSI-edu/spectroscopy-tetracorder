	subroutine f7(ic)
	implicit integer*4 (i-n)

#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc         This subroutine smooths data by averaging n
#ccc         channels on each side of each data point. the
#ccc         routine possess data in wavelength space
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc          eralph,crtin,wjfren,devlun,devsta,finfil, wavlng,
#ccc          redhed,dtsort,rstart,namdev,namdwv
#ccc  argument list description:
#ccc     arguments: none
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc
	include "../common/spmaxes"   # max parameters, must be first

	include "../common/blank"
	include "../common/lbl7"
	include "../common/lbl6"
	include "../common/lbl4"
	include "../common/label1"
	include "../common/lbl3"
	include "../common/lblg"
	include "../common/lundefs"
	include "../common/alphabet"
	include "../common/lblwav"

	character*8	inm,inmwav
	character*74	idlsav
	real*4          xfact, xxik, xx
	integer*4  isort(SPMAXCHAN),idelet(SPMAXCHAN)

    equivalence (isort(1),datab(1)),(idelet(1),datab(124))


	call eralph
	call whedr2
	write(ttyout,1)
5   write(ttyout,2) itrol(1), itrol(2)
        	newwav=0

	idlsav = ' '
	do i= 1,maxchn {
		datac(i)=0.0
		error(i)=0.0
	}
	call crtin
	xmin=-1.e+34
	xmax= 1.e+34
	xfact=2.0

	idlt=0
	do j= 1, maxchn
		idelet(j)= 0

	i=1
	call wjfren (i,x,il)
	if (il==ihx) go to 1002
	if (il==ihe) go to 1000
	if (i>=80) go to 5
	if (il!=0) go to 5
	ixx=nchans/2
	if (x<=0 || x>ixx) go to 5
	n=x

	call wjfren (i,x,il)    # xfactor (should be > 1, >1 decreases adjacent channels
	if (il==ihx) go to 1002
	if (il==ihe) go to 1000
	if (i>=80) go to 5
	if (il!=0) go to 5
	if (x<=0.0001 || x>1.0) go to 5
	xfact=x

	write(ttyout,111) n, xfact
111	format('debug:  n=',i6,' xfact=',f8.5,/)

9       call wjfren (i,x,il)
	if (il==ihe) go to 1000
	if (il==ihd) idlt=1
	if (il==ihl) go to 20
	if ((il==ihcv)|(il==ihcw)|(il==ihcd)|(il==ihcu)|
		(il==ihcy)|(il==ihcc)) go to 25
	if (il!=0 && idlt!=1) go to 5
	if (idlt==1) go to 30
        if (i>=80) go to 35
        else go to 5

20      call wjfren (i,x,il)
	if (i>=80) go to 5
	if (il!=0) go to 5
	xmin=x
	call wjfren (i,x,il)
	if (i>=80) go to 5
	if (x!=0) go to 24
	if (il!=0) go to 5
24      xmax=x
	if (xmax<=xmin) go to 5
	if (il!=0) i=i-1
	go to 9

25	iwtmpf = il
        newwav=1
        call wjfren (i,x,il)
        if(i>=80) go to 5
        iwrec=x
        if (il!=0) i=i-1
        go to 9
30      write(ttyout,31) nchans
	j=1
32      call crtin
	idlsav = iopcon(1:74)  # save first part of deleted points
	i=1
	do j= j,maxchn {
		call wjfren (i,x,il)
		if (i>=80) go to 32
		if (x>0 && il==ihc) go to 33
		if (x==0 && il==ihc) break
		if (il!=0) go to 30
33              m=x
		if (m>0 && m<=maxchn) idelet(j)=m
		if (il==ihc) break
	}
35      iwfl= itrol(2)
        if(newwav == 0) {
        	iwtmpf=itrol(1)
        	iwrec=itrol(2)
         }
         call wavlng (iwtmpf,iwrec,ier)
         if (ier !=0) go to 5
	call dtsort (wdata,isort,nchans)


	write(ttyout,11) itrol(1), itrol(2)
	if (ictrl!=ihe) go to 37
	write(ttyout,70)
	idad=2

	is = 0
	call devlun (4,idv1,is)
	call devsta (is,ista,0,iprt)
	if (ista<=0) go to 1002
	if (is==0) go to 1000
	itmp = ifl1
	call finfil(itmp,is,1,ier)  #position before errors
	ifler= itmp+1
	ifla1= ifl1
	call finfil (ifler,is,1,ier) #get errors
	if (ier!=0) go to 1000
	ifl1= ifla1
	do i= 1,maxchn
		error(i)=dataa(i)
	goto 10
37      call devlun (4,idv1,is)
	call devsta (is,ista,0,iprt)
	if (ista<=0) go to 1002
	if (is==0) go to 1000
10	itmp = ifl1
	call finfil (itmp,is,1,ier)
	if (ier!=0) go to 1000

	do i= 1, maxchn
		dataa(i)=0.0

	do i= 1,nchans {
		ib= isort(i)
#		write(ttyout,112) i, ib
#112		format(' i=',i6,' ib=', i6)
		if (ib<1 || ib>nchans) next
		j= i-n
		if (j<1) j=1
		l= i+n
		if (l>nchans) l=nchans
#
#     use dataa array for error computation.
#
		xx=0
		xm=0
		do k= j,l {
			ia= isort(k)
			if (ia<1 || ia>nchans ||
				data(ia)>=xmax || data(ia)<=xmin) next
			do ii= 1,nchans
				if (ia==idelet(ii)) next
			ixx= i-k
			xxik= ixx
			xx= 1.0/(xfact**(abs(xxik)))
			datac(ib)= datac(ib) +data(ia)*xx
			xm= xm+ xx
			if (ictrl!=ihe) next
			if (error(ia)<1.e-16) error(ia)= 0.0
			dataa(ib)= error(ia)*error(ia)*xx + dataa(ib)
		}
		if (xm>0) datac(ib)= datac(ib)/xm
		if (xm<=0) datac(ib)= -1.23e34
		if (ictrl!=ihe) next
		if (dataa(ib)<=0 || xm<=0)
			dataa(ib)= 0.0
		else
			dataa(ib) = (dataa(ib)**0.5)/xm
	}
	if (ictrl!=ihe) go to 199
	do i= 1,nchans {
		ib= isort(i)
		error(ib)= dataa(ib)
	}
	do i= 1,nchans {
                data(i)= dataa(i)
        }
#
#     determine history
#
199     ititl1 = ' '
	call namdev (idv1,inm)
	call namdwv (itrol(1),inmwav)
	write(ihist,210) inm,ifl1,n,xfact
	write(mhist(1:74),211) xmin, xmax
	write(mhist(75:119),212) inmwav, itrol(2), nchans
	if (idlt ==0) {                    # no deleted points
		write(mhist(120:148), 213)
		mhist(149:222) = ' '
	}
	if (idlt ==1) {                    # deleted points
		write(mhist(120:148), 214)
		mhist(149:222) = idlsav(1:74)
	}
	mhist(223:296) = ' '
	ic=0
	return
1000    ic=ihe
	return
1002    ic= ihx
	return
1       format (' Special Function 7', /)
2       format (' ',
 'This function smooths data by averaging n channels on each side of', /,
' each data point.  the routine processes data in wavelength space', //,
'      Type in the NUMBER OF CHANNELS, WEIGHT Factor followed by the options:',/,
'        WEIGHT Factor = 1/WEIGHT**n, n-adjacent channel', /,
'        WEIGHT Factor = 1 = box filter', /,
'        the Wavelength File ID and record number', /,
'               if the wavelength set is not  ',a1, i5,/,
'        l  and the MIN and MAX data numbers to be considered', /,
'        d  to DELETE points.', /)
11      format (' *** WORKING ***',2x, '(wavelength file',a1, i5,')')
31      format (' Type in channels to be DELETED (', i5, ' max)', /)
70      format (' *** Error Analysis included ***')
210     format ('function f7: ',a8,i4,' smooth n=',
		i4,'.  xfactor=',f8.4, '  ')
211	format ('f7: rejection limits=',1pe14.6, ', ',1pe14.6,
			'                       ')
212	format ('wave set: ',a,' rec',i6,' channels=',i5,
		', ')
213	format (' No deleted points           ')
214	format (' 1st line of deleted points: ')
215	format (a)
	end

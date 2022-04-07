	subroutine f25(ic)
	implicit integer*4 (i-n)
#**********************************************************************
#
#   Special Function 25
#
#   Least Square Solution of Two(2) Component Optically Isolated
#       Mixture of Spectra
#
#  Function 25 takes two spectra (end members thought to be in
#  optically isolated patches on a surface) and a spectrum of the
#  surface, then finds the scaling factor which gives the best match
#  (least squares).
#
#  Input spectra  dataa array = spectrum of the surface 
#		(input from math funct.)
#                 datab array = end point spectrum
#                 data array  = end point spectrum
#
#       find x for the least squares solution to
#               x*a + (1-x)*b = c
#       where the arrays correspond to:
#               dataa = c
#               datab = a
#               data  = b
#

	include "../common/spmaxes"   # max parameters, must be first

	include "../common/blank"
	include "../common/label1"
	include "../common/lbl6"
	include "../common/lblg"
	include "../common/lbl3"
	include "../common/alphabet"
	include "../common/lundefs"
	include "../common/dscrch"

	double precision factx,factc

	character*8	idevn1,idevn2
	integer*4 idlt(SPMAXCHAN)
	equivalence (idlt,datsc1)

	call hreset(1)
	write(ttyout,10)

#**
#***** Check for input file
#**

	if (ictrl == -1) {
		write(ttyout,20)
		call crtin
		ic = ihx
		goto 9000
	}

#**
#***** Header Information
#**

	call whedr2
	write(ttyout,30) idv1,ifl1,ititl1
	call crtin
	ich = 1
	call wjfren(ich,x,il)
	if (il == ihe | il == ihx) {
		ic = il
		goto 9000
	}
	indev = idv1
	infil = ifl1

# NOTE: this reading of error bars will only work for spectra <= 256 channels

	ifile = ifl1+1
	call devok(4,idv1,ifile,lun,ier)
	itmp = ifile
	call redfil(itmp,lun,ier)
	if(ier!=0){
		ic=ihx
		write(ttyout,255)
		call crtin
		goto 9000
	}
	do i= 1,nchans {
		error(i)=data(i)
		if ( error(i) <= 1.0e-10 ) error(i)=1.0e-10
	}

#**
#***** Deleted Points
#**
	write(ttyout,35)
	call crtin
	ich = 1
	call dltpts( ich,ipts,idlt,nchans,ic)
	if (ic == ihe | ic == ihx) {
		goto 9000
	}
	if (ipts != 0)
		for(i=1;i<=ipts;i=i+1)
			dataa(idlt(i)) = -1.23e34

#**
#***** Read in end point spectra
#**
	write(ttyout,40)
#	for(;;) {
900     continue
		call crtin
		ich = 1
#       **** decode file id of first end point ****
		call wjfren(ich,x,iedev1)
		if (ich >= 80) {
			write(ttyout,45)
			goto 900
		}
		if (iedev1 == ihe | iedev1 == ihx) {
			ic = iedev1
			goto 9000
		}
		if (iedev1 == 0) {
			write(ttyout,50)
			goto 900
		}
#       **** decode record number of the first end point ****
		call wjfren(ich,efil,il)
		if (ich >= 80) {
			write(ttyout,55)
			goto 900
		}
		if (il != 0) {
			write(ttyout,60)
			goto 900
		} else iefil1 = efil
#       **** decode file id or if number assume same file id as ****
#            first end point, of the second end point
		call wjfren(ich,efil,iedev2)
		if (ich >= 80) {
			write(ttyout,62)
			goto 900
		}
		if (iedev2  == 0 & efil != 0.) {
			iedev2 = iedev1
			iefil2 = efil
		} else if (iedev2 != 0 & efil == 0) {
#       **** decode record number of second end point ****
				call wjfren(ich,efil,il)
				if (ich >= 80) {
					write(ttyout,65)
					goto 900
				}
				if (il != 0) {
					write(ttyout,70)
					goto 900
				} else {
					iefil2 = efil
				}
		}
		call devlun(4,iedev1,ielun1)
		ire = 0
		call devsta( ielun1,ista,ire,iprt)
		itmp = iefil1
		call finfil(itmp,ielun1,2,ier)
		if (ier != 0) {
			write(ttyout,80)
			goto 900
		}
		call devlun(4,iedev2,ielun2)
		ire = 0
#RED devlun changed to devsta
		call devsta(ielun2,ista,ire,iprt)
		itmp = iefil2
		call redfil(itmp,ielun2,ier)
		if (ier != 0) {
			write(ttyout,90)
			goto 900
		} else goto 910
#	}
	goto 900
910     continue
	write(ttyout,91) ititl2
	write(ttyout,92) ititl

	do i=1,nchans {
		if(error(i)==0.0){
			write(ttyout,81)i
81			format(1x,'error(',i3,')=0.0')
		}
	}

#**
#***** Calculations Least Squares
#**

	write(ttyout,256)
256     format(1x,' Do you want the INPUT spectrum to be SCALED',/,
		  '  for best fit?',/,
		  '  Type y for YES,  RETURN for NO.',/)
	call crtin
	nchar=1
	call wjfren(nchar,x,is)

	if(is==ihy) {
		call lsqft(dataa,datab,data,nchans,error,factx,factc,n,ictrl)


	} else {
		n=0
		for (i=1; i<= nchans; i=i+1) {
			if (dataa(i) == -1.23e34 | datab(i) == -1.23e34 |
				data(i) == -1.23e34) {
				dataa(i) = -1.23e34
				write(ttyout,95)i
				next
			}
			if (datab(i) - data(i) == 0) {
				write(ttyout,100)i
				next
			}
			up = dataa(i) - data(i)
			down = datab(i) - data(i)
			factx = factx + up/down
			n = n+1
		}
		if (n <= 0) {
			write(ttyout,105)
			call crtin
		}
		factx = factx/float(n)
		factc = 1.0
	}
	write(ttyout,300) factx,n
300	format(/,' Least Square Solution = ',1pg13.6,' for ',i8,'channels')
	for (i=1;i <= nchans; i=i+1) {
		if (dataa(i) == -1.23e34 | datab(i) == -1.23e34 |
	       	    data(i) == -1.23e34) {
			datac(i) = -1.23e34
			next
		}
		datac(i) = factx*datab(i) + (1.0 - factx)*data(i)
	 }
	sumsq = 0.0
	do i=1,nchans {
		if (datac(i)!=-1.23e34) {
			sumsq = sumsq + (factc*dataa(i)-datac(i))**2
                }
	}
			sumsq = sumsq/float(n)
#**
#***** HISTORY
#**
#*** Read input data to get appropriate header
#
	call devlun(4,indev,inlun)
	ire = 0
	call devsta(inlun,ista,ire,iprt)
	if (ire != 0) {
		write(ttyout,110) indev,infil
		ic = ihx
		goto 9000
	}
	call namdev(indev,idevn1)
	write(ihist,200)idevn1,infil
	call namdev(iedev1,idevn1)
	call namdev(iedev2,idevn2)
	write(mhist(1:74),220) idevn1,iefil1,idevn2,iefil2
	write(mhist(75:148),231) factx,1.0-factx,factc
	write(mhist(149:222),250) sumsq,n
9000	return
	
10      format(///)
20      format(' ERROR...No Input File ')
30      format(' Function F25: Least Square Solution of 2 Component',/,
	       '               Isolated Mixture of Spectra',//,
	       ' Input File: ',a2,i4,10x,a//,
	       ' Press Return to Continue -or-  e  or  x  to EXIT',/)
35      format(' Enter DELETED Points...then  c  to continue',/)
40      format(' Enter File Ids and Record Numbers of the End Point',
	       ' Spectra',/,' which are to be processed',/)
45      format(' ERROR...REENTER')
50      format(' ERROR...NO FILE ID of 1st End Member Spectrum')
55      format(' ERROR...NO RECORD NUMBER of 1st End Member Spectrum')
60      format(' ERROR...INVALID INPUT')
62      format(' ERROR...NO FILE ID of 2nd End Member Spectrum')
65      format(' ERROR...No RECORD NUMBER for 2nd End Member Spectrum')
70      format(' ERROR...INVALID INPUT')
80      format(' ERROR...Cannot Read 1st End Member spectrum')
90      format(' ERROR...Cannot Read 2nd End Member spectrum')
91      format(' 1st End Member > ',a)
92      format(' 2nd End Member > ',a)
95      format(' Channel ',i3, ' is a deleted point')
100     format(' Channel ',i3,' ZERO DIVIDE CHECK')
105     format(' ERROR...No points calculated')
110     format(' ERROR...Cannot read input file: ',a2,i4)
200     format ('f25: Least squares areal mix fit to ',a8,
		' f',i4,',See Man H')
220     format ('** solution to: x*a+(1-x)*b=(factc)*c; a=',a8,' f',i4,
		' b=',a8,' f',i4)
231     format ('** x=',1pg13.6,',1-x=',1pg13.6,' factc = ',1pg13.6)
250     format ('** sum of the squared residuals = ',1pg12.6,' for ',
		i3,' channels')
255     format ('***error***: no file input')
	end

	subroutine f21(ic)
	implicit integer*4 (i-n)

	include "../common/spmaxes"   # max parameters, must be first

	include "../common/blank"
	include "../common/lbl6"
	include "../common/lbl4"
	include "../common/label1"
	include "../common/lbl3"
	include "../common/lblg"
	include "../common/lbl7"
	include "../common/lundefs"
	include "../common/alphabet"
 	include "../common/dscrch"

#RED
	integer*4 iwidok    # function

	character*8 inam,inam2
	real*4 x(SPMAXCHAN),c(SPMAXCHAN)
	equivalence (datsc1,x),(datsc2,c)



	call hreset(1)
	call whedr2

	if (ictrl == -1) {
		ic = ihe
		write(ttyout,2)
		goto 1000
	}

1       write(ttyout,10) idv1,ifl1,ititl

	write(ttyout,20)

	iwtmpf=itrol(1)
	iwrec =itrol(2)

	call crtin
	i=1
	call wjfren(i,xx,id)
	if (id == ihe || id == ihx) goto 1000
	if (iwidok(id) == 1) {
		iwtmpf = id
		call wjfren(i,xx,id)
        	if (xx<1.0 | xx>maxrec | id!=0) {
			call what(i)
			goto 1
		} else iwrec = xx
        } else if (i<80) goto 1


	call wavlng(iwtmpf,iwrec,ier)
	if (ier != 0) go to 1


	for (ii=1; ii<=maxchn; ii=ii+1) x(ii) = dataa(ii)

######## decode number of terms ########
5       write(ttyout,25)
	call crtin
	i = 1
	call wjfren(i,xxx,il)
	if (il == ihe || il == ihx) goto 1000
	if (il != 0) {
		call what(i)
		goto 5
	}
	if (i >= 80 ) {
		nterms = 0
	} else nterms = xxx
	if (nterms>maxchn || nterms<0) {
		write(ttyout,30)
		id = ihx
		goto 1000
	}

####### clear datac,  arrays #######

	for (ii=1;ii<=maxchn;ii=ii+1) datac(ii) = 0.0

	call devlun(4,idv1,lun)
	call devsta(lun,ista,0,ier)
	if (ista<=0 | lun==0) {
		id = ihx
		goto 1000
	}
	itmp = ifl1
	call finfil(itmp,lun,2,iflag)
	if (iflag!=0) {
		id=ihx
		goto 1000
	}

	for (ii=1;ii<=maxchn;ii=ii+1) c(ii) = datab(ii)

	if (nterms==0) {
		for (nterms=1; nterms<=maxchn & c(nterms)!=0.0; nterms=nterms+1)
			{}
		nterms = nterms-1
	}

	write(ttyout,90) nterms

######## Compute polynomial #######


	for (jj=1;jj<= nchans; jj=jj+1) {
		sum = 0.0
		if (x(jj)!=-1.23e34) {
			for (ii=1;ii<=nterms;ii=ii+1) {
				k = ii-1
				if (ii>1) {
					sum = sum + c(ii)*x(jj)**k
				} else {
					sum = c(ii)
				}
			}
			datac(jj) = sum
			data(jj) = sum
		} else {
			datac(jj) = -1.23e34
			data(jj) = -1.23e34
		}
	}

######## History ########

	call namdev(idv1,inam)

	write (ihist,100) itrol(1),itrol(2),inam,ifl1,nterms

1000    if (id == ihe || il == ihe) ic = ihe
	if (id == ihx || il == ihx) ic = ihx
	return
2   format (' **** ERROR -- no input file. ')
10  format (' ',
     'Function f21: Calculate N term Polynomials',//,
    ' This function calulates N term polynomials for the current',/,
    ' wavelength set using the coeffcients contained in the input data:',//,
    ' Operating on: ',a,i5,':',a)
20      format (' to SELECT a different WAVELENGTH SET:',/,
    '         Type in the WAVELENGTH FILE ID ',
			'(upper case) and record number',/,
    ' or carriage RETURN to use the CURRENT ',
			'WAVELENGTH SET,',/,
    ' Type  e  or  x  to EXIT')
25      format(' Type in the NUMBER OF COEFFICIENTS, ',/,
	       ' or carriage RETURN to find number of ',
						'non-zero terms',
					'from the input data set.')
30      format(' INVALID NUMBER OF TERMS.')
90      format(' Using ',i3,' terms.')
100     format('f21:input=',a1,i5,' coeff=',a8,' rec ',i5,' ',i4,' terms')
	end

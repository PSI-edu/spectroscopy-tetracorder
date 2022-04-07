	subroutine f18(ic)
	implicit integer*4 (i-n)
#
#     block averages and statistics routine
#
	include "../common/spmaxes"   # max parameters, must be first

	include "../common/blank"
	include "../common/lbl6"
	include "../common/lbl3"
	include "../common/label1"
	include "../common/lbl7"
	include "../common/lblg"
	include "../common/lundefs"
	include "../common/alphabet"
#RED
	integer*4 iwidok     # function

	character inam*8
	real*4 high,low,nq,mxwv,maxnm,bloksz,tmp
	integer*4 j,k,i,nyqst,x
	character wvchl*4,nyqdc*11

#
#*************** data definitions 1a   **************************
# j- subscript of output data array                          	*
# i- indicates how many times loop is to be done            	*
# x- number of data items averaged                           	*
# bloksz- number of channels per block                          *
# high-  top value of block margins 				*
# low- bottom value of block margins				*
#****************************************************************
#
#
#*********************************************************************
#  This program takes an input file from specpr, and averages	*
#  the data into blocks of size defined by the user.		*
#  If errors have not been read in, then that is done and the 	*
#  standard deviation is calculated in #2a loop			*
# if they were read in, then loop #2b is used for standard deviation.*
#  dataa is input file datac is output file                          *
#*********************************************************************


	if (ictrl==-1) {
		ic=ihx
		write(ttyout,55)
		return
	}

	do i=1,maxchn {
		data(i)=0.0
		datab(i)=dataa(i)
		datac(i)=0.0
		error(i)=0.0
	}


	call eralph
	call whedr2
	write(ttyout,115) idv1,ifl1,ititl

140	call crtin
	bloksz = 0
	i=1
150	call wjfren (i,a,il)
	if (iwidok(il) == 1) {
		iwtmpf = il
		call wjfren (i,b,ik)
		if (b<=0 || b>maxrec) {
			call what (i)
			write(ttyout,165)
			go to 140
		} else {
			irecw=b
			call wavlng (iwtmpf,irecw,ier)
			if (ier != 0) {
				write (ttyout,165)
				go to 140
			}
			call whedr2
		}
		if (ik!=0) i=i-1
		go to 150
	} else  if (il==ihx || il==ihe)  {
		go to 600
	} else {
			irecw=itrol(2)
			iwtmpf = itrol(1)
			call wavlng (iwtmpf,irecw,ier)
			if (ier != 0) {
				write (ttyout,165)
				go to 140
			}
	}
#    Wavelength, or channel?
408	write (ttyout,401)
	call crtin
	i=1
	call wjfren (i,tmp,il)
	if (il==ihw) {
		wvchl='wave'
# RED Initialize maxnm to 0
		maxnm=0
		do i=1,nchans {
			if (maxnm < dataa(i)) maxnm=dataa(i)
		}
	}
	if (il==ihc) {
		wvchl='chan'
		maxnm=nchans
		do i=1,nchans {
			dataa(i)=i
		}
	}
	if (il==ihx || il==ihe) {
		go to 600
	}
	if (il != ihw & il != ihc) {
		call what(i)
		goto 408
	}
#    Block size?
405	write (ttyout,400)
	call crtin
	i=1
	call wjfren (i,b,ik)
	bloksz=b
	if (ik==ihx || ik==ihe) {
		go to 600
	}	
	if (bloksz <= 0) {
		write(ttyout,700) maxchn
		go to 405 
	}
	if (wvchl=='chan' & bloksz != float(int(bloksz))) {
		write (ttyout,701)
		go to 405 
	}
	if (wvchl=='chan' & bloksz > maxnm) {
		write (ttyout,702)
		go to 405
	}
	if (bloksz <= 0) {
		write (ttyout, 700) maxchn
		go to 405
	}
#    Full or half nyquist?
	write (ttyout,402)
	call crtin
	i=1
	call wjfren (i,tmp,il)
	nyqst=il
	nq=1.0
	nyqdc='1/2 nyquist'
	if (nyqst==ihf) { 
		nq=0.5
		nyqdc='nyquist    '
	}
	if (wvchl=='chan' & nyqst==ihf & bloksz/2.0!=float(int(bloksz/2.0))) { 
		write (ttyout,403)
		go to 405
	}	
	else if(nyqst==ihx || nyqst==ihe) {
		go to 600
	}

#	Print 'Doing What?'
	write(ttyout,500) inam,ifl1,bloksz,wvchl,nyqdc
#
#******************* start loop #1b *******************************
 
 	high = bloksz
 	low = 0.0
 	j=1
 420		x=0
 		high = bloksz*nq * float(j-1) + bloksz
 		low = high - bloksz
 		do i = 1,nchans {
 			if (dataa(i) > low & dataa(i) <= high) {
				if (datab(i)!=-1.23e34) {
					datac(j)=datac(j)+datab(i)
					x=x+1
				}
 			}
 		}
 		if (x==0) {
 			write(ttyout,285) j 
 			datac (j) = -1.23e34
 		} else {
 			datac(j)=datac(j)/float(x)
 		}
 		j=j+1
 	if (low < maxnm) go to 420
##  ************ start error computaion loop #2a ******************
	if (ictrl==ihe) {
		write(ttyout,302)
		ifile=ifl1
		id=idv1
		call devok (4,id,ifile,lun,ier)
		if (ier==0)
			call rederr (ifile,lun,ier)
		else {
			write(ttyout,305)
			ic=ihe
			return
		}


	j=1
 479		x=0
		if (datac(j) == -1.23e34) {
			error(j) = -1.23e34
		} else {			
			high = bloksz*nq * float(j-1) + bloksz
			low = high - bloksz
			do i = 1,nchans {
				if (dataa(i) > low & dataa(i) <= high) {
					error(j)=error(j)+data(i) **2
					x=x+1
				}
			}
			if (x==0) {
				error (j) = 0.0 
			} else {
				error(j)=error(j) **.5/float(x)
			}
		}
		j=j+1
 	if (low < maxnm) go to 479
#
#****************** end loop #2a ***************************************
#
	} else {
		ictrl=ihe

	j=1
 480		x=0
		if (datac(j) == -1.23e34) {
			error(j) = -1.23e34
		} else {
			high = bloksz*nq * float(j-1) + bloksz
			low = high - bloksz
			error(j)=0.0
			do i = 1,nchans {
				if (dataa(i) > low & dataa(i) <= high) {
				   if (datab(i)!=-1.23e34 && datac(j)!=-1.23e34) {
					   error(j)=error(j)+(abs(datab(i)-datac(j)))**2
						x=x+1
					}
				}
			}
			if (x==1 || x==0) {
				error(j)=0.0
			} else {
				error(j)=(error(j)/float(x-1)) **.5
				error(j)=error(j)/bloksz **.5
			}
		}
 		j=j+1
 	if (low < maxnm) go to 480
 	}
#********************** end loop #2b *********************************
#
#
#
#     ************* determine history **********************************
	call namdev (idv1,inam)
	write(ihist,500) inam,ifl1,bloksz,wvchl,nyqdc
	write(mhist(1:74),801) itrol(1),itrol(2)
#
#
#     ******************** program end *********************************
600     call rstart(1)
	return
55      format (' *** ERROR -- INVALID FILE STATUS ***',/,
'     program will EXIT.',/)

115     format (' Function f18: Block Averages and Statistics',//,
'     This function reduces data to blocks of size defined by user.', //,
'     Operating on: ',a,i4,':',a,//,
'     Enter new wavelength file id and record number, (e.g. V23),',//,
'     or press return, or "e", or "x" to exit:',/)

165     format (' *** ERROR -- INVALID INPUT re-enter ***:',/)

285     format (' *** WARNING *** : DIVISION BY ZERO has occured.', /,
'     output channel ',i4,'deleted.',/)

302     format (' Calculations started using previous errors:',/)

305     format (' *** ERROR -- ',
		'INVALID FILE PROTECTION for reading of errors ***',/)

400 	format (' What Block Size? ',/)

401 	format (' Compute using Channels, or Wavelengths? (c,w) ',/) 

402 	format (' Using HALF or FULL NYQUIST? (h,f) ',/)

403	format (' *** ERROR -- CANNOT have FULL NYQUIST ',
		'on ODD number of channels',/)

500     format ('f18:',a8,' r',i7,' blcksz = ',
		f6.2,' ',a4,' at ',a11)

700     format (' "n" MUST be > 1 and <= ',i4)

701	format (' "n" MUST be an INTEGER when using channels')

702	format (' "n" MUST be less than the number of channels')

801	format ('**wavelength set: ',a6,',',i6)



	end

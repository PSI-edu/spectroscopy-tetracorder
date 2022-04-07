	subroutine f44(ic)
	implicit integer*4 (i-n)
#ccc  name:  f44 
#ccc  version date:  May 27, 1987
#ccc  author(s):  Noel Gorelick
#ccc  language:  Ratfor
#ccc
#ccc  short description: 
#ccc		This program will find a convex hull fitting the uppermost
#ccc		points, on an X,Y array.  The user may input for a number of
#ccc		iterations to do, causing lower peaks to be found.  The user
#ccc		can also request that the continuum be removed, and used as
#ccc		data for the next iteration.
#ccc
#ccc  algorithm description
#ccc  system requirements:
#ccc  subroutines called:
#ccc
#ccc		conanl == call subroutines
#ccc		hull == finds the min/max peaks for given set of data
#ccc			(usually between ilast,ilow, or ilow,inext) 
#ccc		pfind == finds the next peak using idat
#ccc		lowp == find's lowest point on continuum removed data
#ccc		confix == fixes lower-than-continuum values.
#ccc		remove == removes the continuum from given data (x/y)
#ccc		fill == linear interpolation with idat, to fill in points
#ccc			on continuum.
#ccc
#ccc  argument list description:
#ccc
#ccc		idat = an integer array containing ones (1's) for the
#ccc			channels with relative peaks.
#ccc		datar = a real array containg the continuum removed data
#ccc		tol = real = user input of noise tolerance
#ccc		ilast, inext, ilow = two peaks, and the lowest point between
#ccc			 	      them, relative to the removed data.
#ccc		i,j,k,im = local counters
#ccc		g,p,q = local constants
#ccc
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
	include "../common/lbl6"
	include "../common/lbl3"
	include "../common/label1"
	include "../common/lbl7"
	include "../common/lblg"
	include "../common/lundefs"
	include "../common/alphabet"
	include "../common/dscrch"
#RED
	integer*4 iwidok     # function

	integer*4 idat(SPMAXCHAN),g,q,p,l,rc
	real*4 datar(SPMAXCHAN),odat(SPMAXCHAN)
	equivalence (idat(1),datsc1(1)),(datar(1),datsc2(1)),(odat(1),datsc3(1))
	character cdat*4,inam*8,ddat*4,inamwi*8,edat*6
	cdat='orignl'

#	load data and wavelengths
	if (ictrl==-1) {
		ic=ihx
		write(ttyout,55)
		call crtin
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
			itrol(1) = iwtmpf
			itrol(2) = irecw
			call whedr2
		}
		if (ik!=0) i=i-1
		go to 150
	} else  if (il==ihx || il==ihe)  {
		ic=il
		return
	} else {
			irecw=itrol(2)
			iwtmpf = itrol(1)
			call wavlng (iwtmpf,irecw,ier)
			if (ier != 0) {
				write (ttyout,165)
				go to 140
			}
	}


	call namdwv(itrol(1),inamwi)  #name of input wavelength set

#	How many iterations
	write (ttyout,300)
	call crtin
	i=1
	call wjfren (i,a,il)
		if (il==ihx || il==ihe) {
			ic=il
			return
		}
		num=a
		if (num==0) num=1
		if (num==1) go to 3

#	Do iterations on original data, or on continum removed data
	write (ttyout,301)
	call crtin
	i=1
	call wjfren (i,a,il)
		if (il==ihx || il==ihe) {
			ic=il
			return
		}
		iter=il
		cdat='cont'
		it=1
		if (iter==iho) {
			it=0
			cdat='orig'
		}
#	Enter noise tolerance
3	if (ictrl!=ihe) {
		write (ttyout,302)
		call crtin
		i=1
		call wjfren (i,a,il)
		if (il==ihx || il==ihe) {
			ic=il
			return
		}
		tol=a
		if (i>79 & tol==0.0) tol=.01
	}
#	Remove final continuum?
		write (ttyout,307)
		call crtin
		i=1
		call wjfren (i,a,il)
		if (il==ihx || il==ihe) {
			ic=il
			return
		}
		rc=1
		ddat='remv'
		if (il==ihn) {
			rc=0
			ddat='nrmv'
		}
#       load errors
	if (ictrl==ihe) {
		write(ttyout,303)
		ifile=ifl1
		id=idv1
		call devok (4,id,ifile,lun,ier)
		if (ier==0) {
			call rederr (ifile,lun,ier)
		} else {
			write(ttyout,305)
			ic=ihe
			return
		}
	} else {
		do l=1,nchans {
			if (datab(l)==-1.23e34) next
			data(l)=tol
		}
	}
	do i=1,nchans {
		if (datab(i)==-1.23e34) next
		if (datab(i)<0.0) datab(i)=1.0e-15
	}
	call conanl(dataa,datab,datac,idat,datar,data,nchans,rc,num,it)
#	-----------------------------------------------
#	error computation loop
	if (ictrl==ihe) {
		do l=1,nchans {
			if (it==0) {
				error(l)=data(l)
			} else {
				if (datac(l)==-1.23e34 ||
				 abs(datac(l)) < 0.1e-25) error(l)=0.0
				error(l)=data(l)/datac(l)
			}
		}
	}
#	end errors
#	fix noise tolerance for ihist
	if (ictrl!=ihe) {
		write (edat,'(f6.4)') tol
	} else {
		write (edat,'(a6)') 'errors'
	}
#	-------------------------------------------------
#	determine history
	
599	call namdev (idv1,inam)
	write(ihist,506) inam,ifl1,num,cdat,ddat,edat
	write(mhist(1:74),487) inamwi,itrol(2)
600	call rstart(1)
	return
#     ---------------------------------------------------
#	program end


55      format (' *** ERROR:  NO file ID and record number',
				' entered before f44 ***',/,
'     PRESS RETURN TO EXIT.',/)

115     format (' Function f44: Hull Generation',//,
'     This function will place a convex hull over the higest data  ', //,
'     points opearing on : ',a,i4,':',a,//,
'     Enter new wavelength file id and record number, (e.g. V23),',//,
'     or press return, or "e", or "x" to exit:',/)

165     format ( ' *** ERROR -- invalid input re-enter ***:',/)

300	format ('How many iterations to do? (default = 1)')

301 	format (' Type  c  to remove generated continuum ',
		'after each iteration (default)',/,
'       o  to iterate only on original data',/)

302	format ('Noise Tolerance factor on minima, default = 1% (0.01)'/)

303	format ('computation started using previos errors'/)

305     format (' *** error -- invalid file protection ',
		'for reading of errors ***',/)

307	format ('Remove final continuum y=yes, n=no? (default = y)'/)

487   format('**f44: wavelength set: ',a,' r',i6)

506     format ('f44:',a8,' r',i7,'iter=',i2,' data=',
		a4,'/',a4,' noise tol=',a)

507	format ('**')

	END

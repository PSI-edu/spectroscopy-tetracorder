	subroutine f45(ic)
	implicit integer*4 (i-n)

#ccc  name: f45
#ccc  version date: June 30, 1987
#ccc  author(s): Noel Gorelick
#ccc  language: RATFOR
#ccc
#ccc  short description: Feature Analysis 
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
	integer*4 iwidok    # function

	integer*4 idat(SPMAXCHAN),g,q,p,l,rc,mark
	real*4 datar(SPMAXCHAN),jdat(SPMAXCHAN),anadat(SPMAXCHAN)
	equivalence (idat(1),datsc1(1)),(datar(1),datsc2(1)),
		(jdat(1),datsc3(1)),(anadat(1),datsc4(1))
	character cdat*8,inam*8,inamwi*8
	mark=0
#	load data and wavelengths
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
		ic=ihx
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

#	Enter noise tolerance
3	if (ictrl!=ihe) {
		write (ttyout,302)
		call crtin
		i=1
		call wjfren (i,a,il)
		if (il==ihx || il==ihe) {
			ic=ihx
			return
		}
		tol=a
		if (i>79 & tol==0.0) tol=.01
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
		if (datab(i)<1.0e-15) datab(i)=1.0e-15
	}
	call featur(dataa,datab,datac,idat,data,jdat,anadat,nchans,mark)

# put the feature data into anadat
	do i=1,mark {datac(i)=anadat(i)}
	itrol(1)=ihcc
	itrol(2)=mark
	itrol(3)=ihh
# fix noise tolerance for ihist
	if (ictrl!=ihe) {
		write (cdat,'(f8.6)') tol
	} else {
		write (cdat,'(a8)') 'errors  '
	}
#	-------------------------------------------------
#	determine history
	
599	call namdev (idv1,inam)
	write(ihist,506) inam,ifl1,cdat
	write(mhist(1:74),487) inamwi,itrol(2)
600	call rstart(1)
	return
#     ---------------------------------------------------
#	program end


55      format (' *** error -- invalid file status ***',/,
'     program will exit.',/)

115     format (' Function f45: Feature Analysis',//,
'     This function will do a full feature analysis, from a series of ', //,
'     upper hulls operating on : ',a1,i7,':',a,//,/,
'     Enter new wavelength file id and record number, (e.g. V23),',//,
'     or press return, or "e", or "x" to exit:',/)

165     format (' *** error -- invalid input re-enter ***:',/)

302	format ('Noise Tolerance factor on minima, default = 1% (0.01)'/)

303	format ('computation started using previous errors'/)

305     format (' *** error -- invalid file protection ',
		'for reading of errors ***',/)

487   format('**f45: wavelength set: ',a,' r',i6)

506     format ('f45:',a8,' r',i7,' noise tol = ',a)

507	format ('**')

	END

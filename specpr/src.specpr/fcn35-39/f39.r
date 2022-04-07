	subroutine f39(ic)
	implicit integer*4 (i-n)
#ccc  name:  f39
#ccc  version date:  July 10, 1990
#ccc  author(s):  Gregg A. Swayze, and  Roger N. Clark and Noel Gorelic
#ccc  language:  Ratfor
#ccc
#ccc  short description: 
#ccc   		This program generates a noise spectrum and adds it
#ccc            to an entry spectrum.  The noise is from a random generator
#ccc            and has a mean of zero and standard deviation defined by user.  
#ccc
#ccc  algorithm description see:
#ccc  system requirements:
#ccc  subroutines called:
#ccc
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
	integer*4 iwidok    # function

	real*4 rflibc(SPMAXCHAN)
	equivalence (datsc1,rflibc)

	character*8 inam, inamr, inamwi
	character*26 cmode

	integer*4 nch1, nch2, nch3, nch4
	

#	load data and wavelengths
        if(ictrl == ihe) {
                ic=ihx
                write(ttyout,55)
                call crtin
                return
        }

	do i=1,maxchn {
		data(i)=0.0
		datab(i)=dataa(i)   # this is the observed spectrum
		datac(i)=0.0
		error(i)=0.0
	}


	call eralph
50	call whedr2
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


#       get standard deviation for scaling noise spectrum
#
298	write (ttyout,300)
300	format (1x, 'Enter the desired standard deviation on the',
                        ' errors:',/)
	call crtin
	i=1
	call wjfren (i,x,il)        # get standard deviation
	if (il==ihx || il==ihe) {
		ic=il
		return
	}
	stdev=x
	if (x < 0.00001 || x > 1.0e+7) {
		call what(i)
		write (ttyout, 310)
310		format ('ERROR: STANDARD DEV. OUT OF RANGE OR',
                          ' BAD CHARACTER FOUND',//, 'REENTER:',//)
		go to 298
	}

        if (ictrl ==-1)  {
                write (ttyout, 509)
509             format ('Only noise spectrum will be generated',//)
	}

#       -------------------------------------------------
#       generate noise, scale, and add to spectrum
#
        do i = 1, nchans  {
                 call noise(x)
                 if (ictrl == -1)  {
                         datac(i) = x * stdev
                 } else  {
                         datac(i) = datab(i) + x * stdev
                 }
         }


#	-------------------------------------------------
#	determine history
	
	call namdev (idv1,inam)

	if (ictrl != -1) {
		write(ihist,506) inam,ifl1,stdev
	} else {
		write(ihist,507) stdev
	}

	return
#     ---------------------------------------------------
#	program end


55      format (' *** error -- ',
		'This special function does not handle error bars',/,
'     press return to exit.',//)

115     format (' Function f39: Noise:',//,
'     This function adds random noise to a reference spectrum or',/,
'     generates a noise spectrum', //,
'     opearing on : ',a,i4,':',a,//,
'     Enter new wavelength file id and record number, (e.g. V23),',//,
'     or press return, or "e", or "x" to exit:',//)

165     format (' *** error -- invalid input re-enter ***:',/)

506     format ('f39:',a8,' r',i7,'noise added with std dev=',f14.5)

507     format ('f39:','noise generated with std dev=',f14.5)

	END

	subroutine f11(ic)
	implicit integer*4 (i-n)

#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc         This subroutine removes thermal component from
#ccc         a data.
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc          rstart,hreset,crtin,f11dfl,wjfren,f11nrm,
#ccc          f11tml,filinp,namdev,drite,ertyp,what
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
#     ****************************************************************
#     *
#     * modified 4/19/83  now handles deleted points, 
#     *                   and sets bandnormalization
#     *         factor to 1.0, and saves default values in title file
#     *         record # 28.
#     * latest version: 06/21/79
#     * thermal component is removed from a data record using the
#     * equation:
#     *     ro = (ro' + (ro'/rs)(1-rs)(ps/f) - (po/f))/(1 - po/f)
#     * see roger clark's paper, "planetary reflectance measurements
#     * in the region of planetary thermal emission" (icarus,
#     * in printing).
#     *
#     ****************************************************************


	include "../common/spmaxes"   # max parameters, must be first

	include "../common/blank"
	include "../common/lbl7"
	include "../common/lbl6"
	include "../common/lbl4"
	include "../common/label1"
	include "../common/lbl3"
	include "../common/lblg"
	include "../common/therml"
	include "../common/lundefs"
	include "../common/alphabet"
	include "../common/dscrch"

	real*4 roarry(SPMAXCHAN)
	equivalence (roarry,datsc1)

	logical errs

	character*8 idevn1,idevn2
	real*4 rsarry(SPMAXCHAN),result(SPMAXCHAN)

	equivalence (rsarry,datab),(result,datac)

	integer*4 ier

	call hreset(1)

#     *** check the standard deviation error flag ***
	if (ictrl == -1) {
		write(ttyout,20)
		call crtin
		ic = ihx
		goto 9000
	} else if (ictrl == ihe)
		write(ttyout,11)
	else write(ttyout,10)

#     *** when enter subroutine, ro is in <dataa>. copy data ***
#     *** to another array so can reuse dataa.               ***
	do  i = 1,SPMAXCHAN
		roarry(i) = dataa(i)

	nrodev = idv1
	nrofil = ifl1

50      write(ttyout,60) nrodev,nrofil,ititl1,itrol(1),itrol(2),nchans

	call f11dfl(ic)
	if (ic==ihe | ic==ihx) goto 9000

#     *** save information for history ***
	irodev = nrodev
	irofil = nrofil

#     *************************************
#     * get new albedos, standards, etc.
#     *************************************
	if (ic != 0) call f11nrm(ic)
	if (ic==ihe | ic==ihx) goto 9000

	write(ttyout,55)

	call crtin
	i = 1
	call wjfren(i,x,ic)
	if ((ic == ihe) | (ic == ihx)) goto 9000
	else if (ic==ihy) {
		call f11nrm(ic)
		if (ic==ihe | ic==ihx) goto 9000
	} else if (ic!=ihn) {
		call what(i)
		goto 50
	}

	write(ttyout,70)
	call crtin
	i = 1
	call wjfren(i,potemp,ic)
	if (i>=80) {
		call what(i)
		goto 50
	} else if (ic==ihe | ic==ihx) goto 9000

	call wjfren(i,pstemp,ic)
	if (i>=80) {
		call what(i)
		goto 50
	} else if (ic==ihe | ic==ihx) goto 9000

	write(ttyout,80)
	call crtin
	i = 1
	call wjfren(i,dist,ic)
	if (i>=80) {
		call what(i)
		goto 50
	} else if (ic==ihe | ic==ihx) goto 9000

#     ****************************
#     * remove thermal component
#     ****************************

	call f11tml(wmin,wmax)

#     *****************
#     * write history
#     *****************


#     *** read in ro file to put in header information ***
	errs = .true.
	itmp = irofil
	call filinp(irodev,itmp,errs,ic)
	if ((errs) | (ic == ihx)) {
		write(ttyout,230) irodev,irofil
	 call crtin
	 ic = ihx
	 goto 9000
      }

	xnrm = 1.0

	call namdev(irodev,idevn1)
	write (ihist,250) idevn1,irofil,potemp

	call namdev(irsdev,idevn1)
	call namdev(isodev,idevn2)
	write (mhist(1:74),270) idevn1,irsfil,pstemp,idevn2,isofil

	write (mhist(75:148),290) ronorm,rsnorm

	write (mhist(149:222),300) itrol(1),itrol(2),dist

	write(ttllun,rec=28,iostat=ier) irodev
	call ertyp('f11:',15,ier)
9000    return
10  format (' f11: Thermal removal from a lunar spot (errors not included)'/)
11  format (' f11: Thermal removal from a lunar spot (errors included)'/)
20  format (' no spectrum was input. press return to hard exit.'/)
55  format (' any changes (y/n/e)?'/)
60  format (' object = ',a,i4,':',a,/,
	    ' wavelength file ',a1,i4,' (',i3,' channels)'/)
70  format (' enter temperatures (kelvin) of object & standard'/)
80  format (' enter earth-sun distance in a.u.'/)
230 format (' i/o error. fill out bug sheet in specpr manual .',/,
	    ' press return to hard exit.',a,i7/)
250 format ('f11: thermal removed from ',a,' f',i5,',t=',f6.1,'k,see mhist')
270 format ('standard=',a,' f',i5,' t=',f6.1,'k, solar flux/pi=',a,' f',i5)
290 format ('albedo at normalization: object=',f7.4,' standard=',f7.4)
300 format ('wavlength file=', a1,i5 ,' earth-sun distance in a.u.=',f8.4)
	end

	subroutine f24(ic)
	implicit integer*4 (i-n)

#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc         This subroutine performs thermal corrections
#ccc         of star/moon spot
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc          hreset,f24dfl,crtin,wjfren,f24tml,filinp,
#ccc          namdev,drite,ertyp,rstart
#ccc  argument list description: none
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc
#################################################################
#       See Roger Clark's paper, "Planetary Reflectance         #
#               Measurements in the region of Planetary         #
#               Thermal Emission." ICARUS 1979 vol 40           #
#               p.94-103  eqn 14.                               #
#                                                               #
#       INPUTS                                                  #
#         Iu          uncorrected intensity ratio for star/moon #
#                           spot                                #
#         Rspot       moon spot reflectance                     #
#         Fsun        solar flux / pi (watts/m2/micron)         #
#         y           aperature diameter (arc-sec)              #
#         dist        earth-sun distance (a.u.)                 #
#         potemp      temperature of moon spot                  #
#                                                               #
#       Array usage.                                            #
#         Iu       ->   roarry (datsc1)                         #
#         Rspot    ->   rsarry (datab)                          #
#         Fsun     ->   soarry (data)                           #
#         result   ->   result (datac)                          #
#         wavlen   ->   dataa                                   #
#################################################################

	include "../common/spmaxes"   # max parameters, must be first

	include "../common/blank"
	include "../common/lbl7"
	include "../common/lbl6"
	include "../common/lbl4"
	include "../common/label1"
	include "../common/lbl3"
	include "../common/lblg"
	include "../common/therml"
	include "../common/alphabet"
	include "../common/lundefs"
	include "../common/dscrch"

	logical errs

	real*4 rsarry(SPMAXCHAN),result(SPMAXCHAN),roarry(SPMAXCHAN)
	integer*4 jdummy(64)
	character*8 idevn1,idevn2	

	integer*4 ier

	equivalence (y,rsnorm),(jdummy,irodev)
	equivalence (rsarry,datab),(result,datac)
	equivalence (roarry,datsc1)


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
	do  i = 1,nchans
		roarry(i) = dataa(i)

#     *** save information for history ***
	nrodev = idv1
	nrofil = ifl1

50      write (ttyout,60) nrodev,nrofil,ititl1,itrol(1),itrol(2),nchans

	call f24dfl(ic)
	if (ic==ihe | ic==ihx) goto 9000

	irodev = nrodev
	irofil = nrofil

#     *************************************
#     * get aperture size earth-sun distance & temp of moon-spot
#     *************************************
	if (ic != 0) call f24nrm(ic)
	if (ic==ihe | ic==ihx) goto 9000

	write(ttyout,55)

	call crtin
	i = 1
	call wjfren(i,x,ic)
	if ((ic == ihe) | (ic == ihx)) goto 9000
	else if (ic==ihy) {
		call f24nrm(ic)
		if (ic==ihe | ic==ihx) goto 9000
	} else if (ic!=ihn) {
		call what(i)
		goto 50
	}

	write(ttyout,70)
	call crtin
	i = 1

	call wjfren(i,dist,ic)
	if (i>=80) {
		call what(i)
		goto 50
	} else if (ic==ihe | ic==ihx) goto 9000

	call wjfren(i,potemp,ic)
	if (i>=80) {
		call what(i)
		goto 50
	} else if (ic==ihe | ic==ihx) goto 9000


#     ****************************
#     * remove thermal component
#     ****************************

	call f24tml(wmin,wmax)

#     *****************
#     * write history
#     *****************


#     *** read in ro file to put in header information ***
	errs = .true.
	itmp = irofil
	call filinp(irodev,itmp,errs,ic)
	if ((errs) | (ic == ihx)) {
		write (ttyout,230)
		call crtin
		ic = ihx
		goto 9000
    }

	xnrm = 1.0

	call namdev(irodev,idevn1)
	write (ihist,250) idevn1,irofil,potemp

	call namdev(irsdev,idevn1)
	call namdev(isodev,idevn2)

	write(mhist(1:74),270) idevn1,irsfil
	write(mhist(75:148),290) idevn2,isofil
	write(mhist(149:222),310) dist,wmin,wmax

	write(ttllun,rec=29,iostat=ier) jdummy
	call ertyp('f24:',15,ier)

9000 return
10  format (' f24: Thermal correction of Star/Moon-spot (errors not included)'/)
11  format (' f24: Thermal correction of Star/Moon-spot (errors included)'/)
20  format (' no spectrum was input. press return to hard exit.'/)
55  format (' any changes (y/n/e)?'/)
60  format (' object = ',a,i5,':',a,/,
	    ' wavelength file ',a1,i5,' (',i4,' channels)'/)
70  format (' enter earth-sun distance (a.u.) and ',
		' temperature of moon-spot (deg. Kelvin).',/)
230 format (' i/o error. fill out bug sheet in specpr manual .',/,
	    ' press return to hard exit.'/)
250 format ('f24:Star/Moon-spot Therm Corr. on',a8,' r',i5,',t=',f6.1,'K')
270 format ('Rspot (moon-spot reflectance)=',a,' r',i5)
290 format ('Fsun (solar flux / pi) = ',a,' r',i5)
300 format ('aperture diameter (arc-sec) = ',f7.4)
310 format ('earth-sun dist. (a.u.) =',f8.4,' wavlen min = ',f8.4,
		' max = ',f8.4)
	end

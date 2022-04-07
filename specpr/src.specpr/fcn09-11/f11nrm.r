	subroutine f11nrm(ic)
	implicit integer*4 (i-n)

#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc                   This subroutine ro,rs,po, and ps values for f11.
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc                    crtin,wjfren,filinp,what
#ccc  argument list description:
#ccc     arguments: wmin,wmax
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc

#     ********************************************************
#     *
#     * routine to read in ro, rs, po, and ps values for f11
#     *
#     ********************************************************

	include "../common/spmaxes"   # max parameters, must be first

	include "../common/blank"
	include "../common/lbl3"
	include "../common/therml"
	include "../common/label1"
	include "../common/lbl7"
	include "../common/lundefs"
	include "../common/alphabet"

	real*4 rsarry(SPMAXCHAN)
	equivalence (rsarry,datab)

	logical errs


	write(ttyout,10)
	repeat {
		call crtin
		i = 1
		call wjfren(i,ronorm,ic)
		if (ic==ihe || ic==ihx) return
		if (ronorm<=0.0 || ronorm>1.0 || ic>=80) {
			call what(i)
			errs = .true.
		} else if (ictrl==ihe) {
			call rederr(irofil,irodev,ier)
			if (ier != 0) {
				ic = ihx
				return
			}
			do ii = 1, maxchn
				error(ii) = data(ii)
		}
		if (ic==0) call wjfren(i,wav,ic)
		if (ic==ihe || ic==ihx) return
		if (i>=80) errs = .false.
		else if (ic!=iha) errs = .true.
		else {
			call wjfren(i,wav,ic)
			if (ic==ihe || ic==ihx) return
			if (i>=80) errs = .true.
			else {
				itrol(2) = wav
				if (itrol(2)<=0 || itrol(2)>99) errs = .true.
				else errs = .false.
			}
		}
	} until(!errs)

	write(ttyout,20)
	repeat {
		call crtin
		i = 1
		call wjfren(i,x,irsdev)
		call wjfren(i,rsfil,ic)
		irsfil = rsfil
		if (irsdev==ihe || irsdev==ihx) {
			ic = irsdev
			return
		}
		if (ic==ihe || ic==ihx) return
		itmp = irsfil
		call filinp(irsdev,itmp,errs,ic)
		if (ic==ihe || ic==ihx) return
		do j=1,maxchn
			rsarry(j) = data(j)

		call wjfren(i,rsnorm,ic)
		if (ic==ihe || ic==ihx) return
		if (rsnorm<=0.0 || rsnorm>1) {
			call what(i)
			errs = .true.
		} else errs = .false.
	} until(!errs)

	write(ttyout,30)irsdev,irsfil,ititl,rsnorm

	write(ttyout,40)
	repeat {
		call crtin
		i = 1
		call wjfren(i,x,isodev)
		call wjfren(i,sofil,ic)
		isofil = sofil
		if (isodev==ihe || isodev==ihx) {
			ic = isodev
			return
		}
		if (ic==ihe || ic==ihx) return
		itmp = isofil
		call filinp(isodev,itmp,errs,ic)
		if (ic==ihe || ic==ihx) return
		errs = .false.
	} until(!errs)

	write(ttyout,50)isodev,isofil,ititl
	ic = 0
	return
10 format (' enter albedo of object. (to change wavelength record, follow ',/,
		' the albedo by "a" and the new record number.'/)
20 format (' enter standard file & albedo'/)
30 format (' standard      = ',a,i4,':',a,' albedo=',f7.4)
40 format (' enter solar flux/pi file'/)
50 format (' solar flux/pi = ',a,i4,':',a)
	end

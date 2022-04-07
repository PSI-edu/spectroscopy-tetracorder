	subroutine f6(ic)
	implicit integer*4 (i-n)

#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc                   This subroutine computes the plank body function.
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc                    rstart,hreset,whedr2,crtin,wjfren,wavlng
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
	include "../common/lbl4"
	include "../common/label1"
	include "../common/lbl3"
	include "../common/lbl7"
	include "../common/lblg"
	include "../common/hptrm"
	include "../common/lundefs"
	include "../common/alphabet"



10  call hreset(1)
	call whedr2
12	write(ttyout,2)
	call crtin
	i= 1
	call wjfren (i,x,il)
	if (i>=80) go to 10
	if (il==ihe || il==ihx) go to 1002
	if (il!=0 || x<0.01 || x>1.e10) go to 10
	t= x
	call wjfren (i,x,il)
#       if no new wavelength file entered then use default
        if (i>=80) {
		iwtmpf=itrol(1)
		iwrec=itrol(2)
		go to 20
        }
#       set new wavelength file 
	if ((il==ihcv)|(il==ihcw)|(il==ihcd)|(il==ihcu)|(il==ihcy)) {
        	iwtmpf=il
		call wjfren (i,x,il)
		if ((il != 0)|(i>=80)) {
			call what(i)
			go to 10
		}
        	iwrec=x
	}
        else go to 10
20      call wavlng (iwtmpf, iwrec, ier)
        if(ier!=0) go to 12
	write(ttyout,21)
	ichns= nchans
	if (ichns<2) ichns=2
	if (ichns>maxchn) ichns=maxchn
	wvmin= 1.e12
	wvmax= 1.e-12
	do i= 1,ichns {
		if (dataa(i)>1.e+12 || dataa(i)<1.e-12) next
		if (dataa(i)>wvmax) wvmax= dataa(i)
		if (dataa(i)<wvmin) wvmin= dataa(i)
	}
	write(ttyout,31) wvmin,wvmax,t,ichns
	do i= 1,ichns {
		call planck (dataa(i), datac(i), t, ier)
		error(i)= 0.0
	}
	write(ttyout, 51)
	write (ititl1,60) t
	write (ihist,62) t, wvmin,wvmax
	mhist = 'units: watts per meter squared'
	ic= 0
	return
1002    ic= il
	return

2       format (' Function f6: Plancks Black Body Funcion',//,5x,
	   'This subroutine computes the Planck black body function at ',
	   'temperature', /, 5x, 't and using any wavelength file.',//,5x,
	   'Type in the temperature (degrees kelvin) and the ',
	   'wavelength', /, 5x, 
	   'file id and record number if it is correct', /, 5x,
	   'Type  e  or  x  to EXIT', /)
21      format (' WORKING')
31      format (' wavelength region', f9.3, ' to', f9.3, 3x, 'temp',
	   f9.2, '# chans=', i4)
51      format (' analysis complete')
60      format ('planck bb function, temp', f10.2, 6(1h ))
62      format ('planck bb, temp', f9.1,' wavlngth range', 2(1pe10.3))

	end

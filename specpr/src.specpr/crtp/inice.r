	subroutine inice(alow,ahigh,bint,aamax,astrt)
	implicit integer*4 (i-n)
	real*4 alow,ahigh,bint,aamax,astrt
#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc                   This subroutine finds nice numbers between alow
#ccc                   and ahigh for labelling the axes of the plots.
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc                    none
#ccc  argument list description:
#ccc    arguments: alow,ahigh,bint,aamax,astrt
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc

	double precision rng,amin,amax,a,b
	
	rng = ahigh-alow
	amin = alow
	amax = aamax
	if (amax<=0.d0) amax = 1.d0
	a = dlog10(rng/amax)
	ia = a
	if (ia>a) ia = ia-1.d0
	b = a-ia
	if (b<=dlog10(2.0d0)) b = dlog10(2.0d0)
	else if (b>dlog10(5.0d0)) b = 1.0d0
	else b = dlog10(5.0d0)
	bint = 10.d0**(ia+b)
	a = abs(amin)+bint
	repeat {
		a = a-bint
		if (a-10.0d0*bint>0.0d0) a = a-(10.0d0*bint)
		if (a-10.d0*10.d0*bint>0.0d0) a = a-(10.d0*10.d0*bint)
		if (a-bint<=0.0d0&&a>=0.0d0) break 1
	} until(a<0.d0)
	if (amin<0) a = amin+a
	else if (amin==0.d0) a = amin
	else a = amin-a+bint
	astrt = a
	
	return
	end

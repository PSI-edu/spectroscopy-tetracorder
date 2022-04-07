	subroutine zstuf(dataa,datab,datac,jdat,number,center,depth,
		width,asym,contum,fract,ilow,ewidth)
	implicit integer*4 (i-q)

#ccc  name: zstuf
#ccc  version date: June 30, 1987
#ccc  author(s): Noel Gorelick
#ccc  language: RATFOR
#ccc
#ccc  short description:  Does the actual computations for feature analysis
#ccc			finds band: center, width, depth, asymmetry,
#ccc			 previous continuum level, fraction of the data
#ccc			 that falls on the continuum, and the 1/4width.
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
	integer*4 number,chan,lfchan,rtchan,ilow,g,counter,chan2,rtcha2,lfcha2
	real*4 x1,x2,y1,y2,width,width1,width2,asym,fract
	real*4 dataa(number),datab(number),datac(number),jdat(number),
			low,a,b,c,depth,center,contum,hfmax,ewidth,ehfmax

#	--------
#
#	finds the lowest value (center), and depth (1-center)
#
	if (ilow==0) {
		center = -1.23e34
		depth  = -1.23e34
		width  = -1.23e34
		asym   = -1.23e34
		contum = -1.23e34

		return
	}
	#write (6,*) "Zstuf debug: start"

	g=1
	call quad(a,b,c,ilow,g,number,dataa,datac)
	if (abs(a)<1.0e-15) a=1.0e-15
	center=(-b)/(2.0*a)
	depth=-((a*(center*center))+(b*center)+c)
#	----------
#
#	finds Full Width Half Maximum
#	Full Width 1/4 Maximum
#	and Asymmetry
#
#	----------
	hfmax=1.0-depth/2.0
	ehfmax=1.0-(depth*(3.0/4.0))
	lfchan=0
	do i=ilow,1,-1 {
		if (datac(i)==-1.23e34 | dataa(i)==-1.23e34) next
		if (datac(i) > hfmax) {   # first channel > FWHM, left side
			lfchan=i
			go to 10
		}
	}
	lfchan=1
10	do i=lfchan+1,ilow {              # bounding channel left side
		if (datac(i)!=-1.23e34 & dataa(i)!=-1.23e34) {
			lfcha2=i
			go to 11
		}
	}
	lfcha2=ilow
11	rtchan=0
	do i=ilow,number {
		if (datac(i)==-1.23e34 | dataa(i)==-1.23e34) next
		if (datac(i) > hfmax) {   # first channel > FWHM, right side
			rtchan=i
			go to 20
		}
	}
	rtchan=number
20	do i=rtchan-1,ilow,-1 {           # bounding channel right side
		if (datac(i)!=-1.23e34 & dataa(i)!=-1.23e34) {
			rtcha2=i
			go to 21
		}
	}
	rtcha2=ilow
21	if (lfchan==lfcha2) {
		lfchan=1
		lfcha2=ilow
	}
	if (rtchan==rtcha2) {
		rtchan=number
		rtcha2=ilow
	}
	y1=datac(lfchan)
	y2=datac(lfcha2)
	x1=dataa(lfchan)
	x2=dataa(lfcha2)
	x21 = x2 - x1
	y21 = y2 - y1
	if (abs(x21) < 1.0e-15) x21 = 1.0e-15 # guard against overflow
	if (abs(y21) < 1.0e-15) y21 = 1.0e-15

	#write (6,*) "Zstuf debug: computing width1"

	width1=(hfmax-y1)/(y21/x21)+x1

	#write (6,*) 'DEBUG zstuf, width1:', y1, y2, x1, x2, hfmax, x21, y21

	y1=datac(rtchan)
	y2=datac(rtcha2)
	x1=dataa(rtchan)
	x2=dataa(rtcha2)
	xx21 = x2 - x1
	yy21 = y2 - y1
	if (abs(xx21) < 1.0e-15) xx21 = 1.0e-15 # guard against overflow
	if (abs(yy21) < 1.0e-15) yy21 = 1.0e-15

	#write (6,*) "Zstuf debug: computing width2"
	#write (6,*) "Zstuf debug: ", hfmax, y1, yy21, xx21, x1

	width2=(hfmax-y1)/(yy21/xx21)+x1

	#write (6,*) 'DEBUG zstuf:', center, width1, width2

	if (abs(x21) < 1.001e-15 |
		abs(y21) < 1.001e-15 |
		abs(xx21) < 1.001e-15 |
		abs(yy21) < 1.001e-15 ) {

			width = -1.23e34
			asym  = -1.23e34

	} else {
			width=width2-width1
			xas=width2-center
			if (abs(xas) < 1.0e-15) {
				asym = -1.23e34
			} else {
				asym=(center-width1)/xas
			}
	}
	#write (6,*) 'DEBUG zstuf:', center, width1, width2, width, asym

#	Full width 1/4 max
	lfchan=0
	do i=ilow,1,-1 {
		if (datac(i)==-1.23e34 | dataa(i)==-1.23e34) next
		if (datac(i)>ehfmax) {
			lfchan=i
			go to 210
		}
	}
	lfchan=1
210	do i=lfchan+1,ilow {
		if (datac(i)!=-1.23e34 & dataa(i)!=-1.23e34) {
			lfcha2=i
			go to 211
		}
	}
	lfcha2=ilow
211	rtchan=0
	do i=ilow,number {
		if (datac(i)==-1.23e34 | dataa(i)==-1.23e34) next
		if (datac(i)>ehfmax) {
			rtchan=i
			go to 220
		}
	}
	rtchan=number
220	do i=rtchan-1,ilow,-1 {
		if (datac(i)!=-1.23e34 & dataa(i)!=-1.23e34) {
			rtcha2=i
			go to 221
		}
	}
	rtcha2=ilow
221	if (lfchan==lfcha2) {
		lfchan=1
		lfcha2=ilow
	}
	if (rtchan==rtcha2) {
		rtchan=number
		rtcha2=ilow
	}
	y1=datac(lfchan)
	y2=datac(lfcha2)
	x1=dataa(lfchan)
	x2=dataa(lfcha2)
	x21 = x2 - x1
	y21 = y2 - y1
	if (x21 < 1.0e-15) x21 = 1.0e-15 # guard against overflow
	if (y21 < 1.0e-15) y21 = 1.0e-15

	#write (6,*) "Zstuf debug: computing 1/4 width1"

#	write (6,*) '3:x1,x2,y1,y2',x1,x2,y1,y2,ehfmax
	width1=(ehfmax-y1)/(y21/x21)+x1

	y1=datac(rtchan)
	y2=datac(rtcha2)
	x1=dataa(rtchan)
	x2=dataa(rtcha2)
	xx21 = x2 - x1
	yy21 = y2 - y1
	if (xx21 < 1.0e-15) xx21 = 1.0e-15 # guard against overflow
	if (yy21 < 1.0e-15) yy21 = 1.0e-15

	#write (6,*) "Zstuf debug: computing 1/4 width2"

#	write (6,*) '4:x1,x2,y1,y2',x1,x2,y1,y2
	width2=(ehfmax-y1)/(yy21/xx21)+x1
#	write (6,*) 'width1,width2,diff',width1,width2,width2-width1
	ewidth=width2-width1
#	--------------------
#
#	finds (interpolates) value of old continuum at center
#
#	--------------------
	do i=1,number {
		if (dataa(i)>center) {
			chan=i
			go to 40
		}
	}
	chan=number
40 	do i=chan-1,1,-1 {
		if (datab(i)!=-1.23e34) {
			chan2=i
			go to 41
		}
	}
	chan2=1
41	y2=jdat(chan)
	y1=jdat(chan2)
	x2=dataa(chan)
	x1=dataa(chan2)
	x21 = x2 - x1
	if (x21 < 1.0e-15) x21 = 1.0e-15 # guard against overflow

	contum=((y2-y1)/x21)*(center-x1)+y1
#	---------------------------
#
#	find fractional difference
#
#	---------------------------
	counter=0
	do i=2,number-1 {
		if (datab(i)==datac(i)) {
			counter=counter+1
		}
	}
	if (number>2) {
		fract=float(counter)/float(number-2)
	} else fract=0.0
	return
	end


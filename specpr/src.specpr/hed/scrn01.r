	subroutine scrn01
	implicit integer*4 (i-n)

#ccc  version date: 05/03/85
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc         This subroutine prints the first screen full of
#ccc         information in the information change routine.
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:    none
#ccc  argument list description:
#ccc      arguments: cta,ctb,sta,stb,datea,dateb,ira,idec
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
#       this routine prints the first screan full of            #
#       header information in the information change            #
#       routine.                                                #
#################################################################

	include "../common/spmaxes"   # max parameters, must be first

	include "../common/blank"
	include "../common/label1"
	include "../common/lundefs"
	include "../common/lbl6"

	integer*4 ihr, imm, deg, minute, mm, dd, yy
	integer*2 chkbit, ibit
	character*1 icsgn
	real*8 xjda, xjdb
	integer*4 jdec(3)

	data ihm/1h-/
	data ihb/1h /

	call eralph
	write(ttyout,1) idv1, ifl1, ititl

# time a

	call todms (iscta,24000,ihr,imm,ss)
	ibit = 4
	if (chkbit(icflag,ibit) == 0) {
		write (ttyout,20) ihr, imm, ss
	} else {
		write (ttyout,21) ihr, imm, ss
	}

# time b

	call todms (isctb,24000,ihr,imm,ss)
	ibit = 5
	if (chkbit(icflag,ibit) == 0) {
		write (ttyout,30) ihr, imm, ss
	} else {
		write (ttyout,31) ihr, imm, ss
	}

# siderial time

	call todms (istb,24000,ihr,imm,ss)
	write (ttyout,40) ihr, imm, ss

# date a

	call frjuld (yy,mm,dd, jdatea)
	xjda = dble (jdatea)/10.0d0
	write (ttyout, 50) xjda, mm,dd,yy

# date b
	call frjuld (yy,mm,dd, jdateb)
	xjdb = dble (jdateb)/10.0d0
	write (ttyout, 55) xjdb, mm,dd,yy

# RA or Longitude

	icsgn=' '
	if (isra < 0) icsgn = '-'
	call todms (iabs(isra),1000,deg,minute,sec)
	ibit = 3
	if (chkbit(icflag,ibit) == 0) {
		write (ttyout, 60) icsgn,deg, minute, sec
	} else {
		write (ttyout, 61) icsgn,deg, minute, sec
	}

# declination or latitude

	icsgn=' '
	if (isdec < 0) icsgn = '-'
	call todms (iabs(isdec),1000,deg,minute,sec)
	ibit = 3
	if (chkbit(icflag,ibit) == 0) {
		write (ttyout, 70) icsgn,deg, minute, sec
	} else {
		write (ttyout, 71) icsgn,deg, minute, sec
	}

	write (ttyout, 100)

	return

1   format ('               header information display ',
			'and change routine',/,
	   15x, 45(1h-), /, 15x, a1, i6, 5x, a, /,
		' enter the letter code of the ',
		'information you wish to change', /)

20	format (' a: Civil time (a)  when  data  ',
		'last  processed:   ', 2(i2,':'),f9.4)

21	format (' a: Universal time (a)  when  ',
		'data last processed: ', 2(i2,':'),f9.4)

30	format (' b: Civil time (b)  when  data  ',
		'collection began:  ', 2(i2,':'),f9.4)

31	format (' b: Universal time (b) when data ',
		'collection began: ', 2(i2,':'),f9.4)

40	format (' d: Siderial  time  when  data ',
		'collection began:   ', 2(i2,':'),f9.4,/)

50	format (' f: Date when data was last processed: (JD=',
		f10.1,') ', 2(i2,'/'),i4)

55	format (' o: Date when data collection began:   (JD=',
		f10.1,') ', 2(i2,'/'),i4,/)

60	format (' h:  Right Ascension  (Hours Min Sec)  =  ',
		a1,i3,1x,i2,1x,f8.3)

61	format (' h:  Longitude  (Deg Arc-min Arc-sec)  =  ',
		a1,i3,1x,i2,1x,f8.3)

70	format (' i:  Declination (Deg Arc-min Arc-sec) =  ',
		a1,i3,1x,i2,1x,f8.3,/)

71	format (' i:  Latitude  (Deg  Arc-min  Arc-sec) =  ',
		a1,i3,1x,i2,1x,f8.3,/)

100	format (' type  g  to exit to crt plot,  e  ',
		'to soft exit with write but no plot,',/,
		'       x  to hard exit  r  to return ',
		'to beginning of header routine',/,
		'          press return for next page of information',/)

      end

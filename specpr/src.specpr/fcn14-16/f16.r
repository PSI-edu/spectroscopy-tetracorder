	subroutine f16(ic)
	implicit integer*4 (i-n)

#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc         Line segment generator:
#ccc         This subroutine inputs a data file from specpr
#ccc         and asks the user for x-y coordinates of points.
#ccc         after this stage the user can make corrections
#ccc         using the r and functions. a listing of previous
#ccc         steps can be made, a b is input and the analysis
#ccc         is begun. the routine will calculate data (y)
#ccc         values foreach wavelength, between each set of
#ccc         ordered points.
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc          er,whedr2,crtin,wjfren,what,wavlng,redhed,iwidok,initdt
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
	include "../common/lbl7"
	include "../common/lbl4"
	include "../common/label1"
	include "../common/lbl3"
	include "../common/lblg"
	include "../common/lundefs"
	include "../common/alphabet"
#RED
	integer*4 iwidok     # function

	real*4 x(SPMAXCHAN),y(SPMAXCHAN),xpt(SPMAXCHAN),ypt(SPMAXCHAN)
 	equivalence (x(1),dataa(1)),(y(1),datac(1)),
 				(xpt(1),datab(1)),(ypt(1),data(1))
	integer*4 q

	logical rpnt,dlt,flag

	data n,   rpnt,   dlt /1,.false.,.false./

#     ******************* data assignments **************************
#timj
	do i=1,maxchn {
		x(i) = 0.0
		y(i) = 0.0
		xpt(i) = 0.0
		ypt(i) = 0.0
	}

	call initdt
	
#     ***************************************************************
#
#     ******************** routine description ***********************
#     *  this routine inputs a data file from specpr and asks the    *
#     * user for x-y co-ordinates of points.  after this stage, the  *
#     * user can make correction using the r and d functions.  a list*
#     * -ing of previous steps can be made with the l fctn.  after   *
#     * corrections have been made, a b is input and the analysis is *
#     * begun.  the routine will calculate the data (y) values for   *
#     * each wavlength, between each set of ordered pts.             *
#     ****************************************************************
#
#
	flag=.false.
	n=1
	call hreset(1)
	call whedr2

	if (ictrl!=-1) {
		write(ttyout,5)
		ic=ihx
		go to 1001
	}
10	write(ttyout,15)
	go to 17

16	call what(i)
        write(ttyout,31)
17	call crtin
	i=1
	call wjfren (i,a,il)
        if (i>=80) go to 32
	if (il==ihe || il==ihx) go to 1000
	if (iwidok(il) == 1) {
		iwtmpf = il
30		call wjfren (i,a,il)
		if (i>=80) go to 16
		if (il!=0) go to 16
		if ((a<=0)|(a>maxrec)) go to 16
		if ((iwtmpf == ihcc) & (a > maxchn)) go to 16
		iwvfl=a
		call wavlng(iwtmpf,iwvfl,ier)
		if (ier != 0) {
			write (ttyout,31) 
31			format (1x,'reinput wavelength file',/)
			go to 17
		}
		itrol(1)=iwtmpf
		itrol(2)=iwvfl
		call hreset(1)
		call whedr2
        }else  go to 10
#        *** wavelength file is assigned to default ***
32		iwtmpf = itrol(1)
		iwvfl=itrol(2)
		call wavlng(iwtmpf,iwvfl,ier)


	if (flag)
		do i=1,nchans
			dataa(i)=i
#
#     *** instructions for use of routine ***
50 			write(ttyout,55)
#
#
#     *** input xpt(1), ypt(1) here ***
100	write(ttyout,105)
	call crtin
	i=1
	call wjfren (i,w,il)
#timj
	if (i>=80) go to 110
	if ((il==ihe)|(il==ihx)) go to 1000
	if (il==0) go to 120
110	write(ttyout,115)
	go to 100
#
120	xpt(n)=w
	call wjfren (i,w,il)
	if (i>=80) go to 140
	if (il==0) go to 150
140	write(ttyout,115)
	go to 100
#
150	ypt(n)=w
	nmax=1
	n=n+1
#
#
#
#     *** input xpt(n), ypt(n) where n is in a loop. ***
160	write(ttyout,165) n,n
	call crtin
	i=1
	call wjfren (i,z,il)
	if (i>=80) {
		write(ttyout,115)
		go to 160
	}
	if ((il==ihe)|(il==ihx)) go to 1000
	if (il==ihl) go to 300
	if (il==ihb) go to 500
	if (il==0) {
		if ((rpnt)&(n==1)|(dlt)&(n==1)) go to 180
		if (z>xpt(n-1)) go to 180
		write(ttyout,175)
		go to 160
180		if (!rpnt) go to 200
		if (z<xpt(n+1)) go to 200
		write(ttyout,195)
		go to 160
	}
	go to 250
#
#
200	xpt(n)=z
	call wjfren (i,z,il)
	if (i>=80) go to 220
	if (il==0) go to 230
220	write(ttyout,115)
	go to 160
230	ypt(n)=z
	n=n+1
	if ((nmax-n) < 1) nmax=n-1
	xpt(nmax+1)=1.0e37
	if (nmax > 255) {
		write(ttyout,245) maxchn
		go to 500
	}
	rpnt=.false.
	dlt=.false.
	go to 160
#
#     *** function return ***
250     if (il==ihr) {
		call wjfren (i,z,il)
		if (i>=80) go to 160
		if (il==0) go to 270
260		write(ttyout,115)
		go to 160
270		if ((z<1)|(nmax<z)) go to 260
280		nhold=n
		n=z
		rpnt=.true.
		dlt=.false.
		go to 160
	}
#
#
#     *** function delete ***
	if (il==ihd) {
		call wjfren (i,z,il)
		if (i>=80) go to 160
		if (il==0) go to 410
400		write(ttyout,115)
		go to 160
410		if ((z>nmax)|(z<1)) go to 400
		do j=z,nmax
			xpt(j)=0
		nmax=z-1
		n=z
		rpnt=.false.
		dlt=.true.
	}
	go to 160
#
#
#
#     *** function list steps ***
300	nsav=n
	call wjfren (i,z,il)
	if (il==0) go to 320
	write(ttyout,115)
	go to 160
320	call hreset(1)
#   *** default beginning step no. is set to 1 ***
	if (z==0) z=1
	q=z
	write(ttyout,340)
#
	j=1
	while  ((q<=(nmax))&(j<26))  {
		write(ttyout,355) q,xpt(q),ypt(q)
		j=j+1
		q=q+1
	}
#
	write(ttyout,356)
#
#
	n=nsav
	rpnt=.false.
	dlt=.false.
	go to 160
#
#
#
#     *** function begin analysis ***
#
#
500	k=1
510	do j=1,nchans {
# 		*** find slope (m) and y-intercept (b) ***
		slope=ypt(k)-ypt(k+1)
# 		*** check to see if denominator is xero ***
		denom=xpt(k)-xpt(k+1)
		if (denom==0) {
			write(ttyout,525)
			denom=1.0e-37
		}
		slope=slope/denom
		b=ypt(k)-slope*xpt(k)
		if (x(j) < xpt(k))  next
		if (x(j) > xpt(k+1))  next
		y(j)=slope*x(j)+b
	}
	k=k+1
	if (k>(nmax-1)) go to 560
	go to 510
#
#
#      *** extend points beyond bounds of xpt-ypt array ***
560	do j=1,nchans {
		if (x(j)<xpt(1))   y(j)=ypt(1)
		if (x(j)>xpt(nmax))   y(j)=ypt(nmax)
	}
#
#
#     *** determine history ***
600	ihist = 'f16: generated line segments.'
	ititl = ihist
#
#
#
730	ic=0
	go to 1001

1000    ic = il
1001    call rstart(1)

	return
5       format (' *** ERROR: file has been input.  ',
		'None was supposed to have been.  ***',/)
15      format (' To change the wavlength file, input the ',
		'wavelength file id and rec. number,',/,
		' or enter C and the number of channels to ',
		'assign the channel numbers to the ',/,
		' wavelength file, or press return for the ',
		'default wavelengths given above, or',/,
		' type  e  or  x to exit',/)
55      format (' ',
 '***********************************************************',/,
' *             Line Segment Generator                      *',/,
' * This is Special Funcion f16.  The following is a des-   *',/,
' * cription of control options for use of routine.  First  *',/,
' * entry is starting point values xpt(1) and ypt(1).  Here *',/,
' * e or x can be entered to exit.  Next entries are xpt(n) *',/,
' * and ypt(n) where n is in a loop.  Here cntrl options are*',/,
' * : e or x to exit, b to begin analysis, l to list out    *',/,
' * pairs of x-y co-ordinates and step no., r to return to  *',/,
' * an earlier step, and d to delete steps beginning at the *',/,
' * at the step no entered and ending at the last data value*',/,
' * the routine will ask for new pts. unless other commands *',/,
' * are given.                                              *',/,
' ***********************************************************')
105     format (/,' INPUT xpt(1), ypt(1), or e or x:',/)
115     format (' *** ERROR: INVALID INPUT ***',/)
165     format ('+INPUT xpt(',i5,'), ypt(',i5,'), or e,x,l,b,d, or r.',/)
175     format (' *** ERROR: x value smaller or equal to last x in array',/)
195     format (' *** ERROR: x value larger or equal to next x in array',/)
245     format (' *** NOTE: no. of entries  >=',i5,/,
		' program will begin analysis. ***',/)
340     format (/,'  step       x-value       y-value',/)
355     format (5x,i5,5x,1pg12.5,5x,1pg12.5,/)
356     format (/)
525     format (/,
' *** ERROR: denom. in equ. is zero. it is reset to 1.0e-37.  ***',/)
	end
